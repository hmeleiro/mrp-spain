MRP_REPORT_INTERVAL <- c(lower = 0.20, upper = 0.80)
MRP_REPORT_FOCUS_PARTIES <- c("PP", "PSOE", "Vox", "FA", "Podemos", "SALF")
MRP_REPORT_PARTY_COLORS <- c(
  PP = "#0056A6",
  PSOE = "#E30613",
  Vox = "#63BE21",
  FA = "#E51C55",
  Podemos = "#6A1B9A",
  SALF = "#8B5A2B"
)
MRP_REPORT_OTHER_COLOR <- "#7A7A7A"

mrp_report_focus_parties <- function(parties) {
  intersect(MRP_REPORT_FOCUS_PARTIES, unique(as.character(parties)))
}

mrp_report_party_palette <- function(parties) {
  parties <- unique(as.character(parties))
  known <- intersect(parties, names(MRP_REPORT_PARTY_COLORS))
  other <- setdiff(parties, known)
  c(
    MRP_REPORT_PARTY_COLORS[known],
    stats::setNames(rep(MRP_REPORT_OTHER_COLOR, length(other)), other)
  )
}

mrp_summary <- function(sims, ci = 0.60) {
  lower <- (1 - ci) / 2
  upper <- 1 - lower
  sims |>
    dplyr::summarise(
      votos = sum(votos), seats = sum(seats),
      .by = c(draw, partido)
    ) |>
    dplyr::mutate(pct = votos / sum(votos) * 100, .by = draw) |>
    dplyr::summarise(
      pct_m = mean(pct),
      pct_lwr = unname(stats::quantile(pct, lower)),
      pct_upr = unname(stats::quantile(pct, upper)),
      seats_m = mean(seats),
      seats_lwr = unname(stats::quantile(seats, lower)),
      seats_upr = unname(stats::quantile(seats, upper)),
      .by = partido
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("pct"), ~ round(.x, 1)),
      dplyr::across(dplyr::starts_with("seats"), ~ as.integer(round(.x)))
    ) |>
    dplyr::arrange(dplyr::desc(pct_m))
}

mrp_previous_summary <- function(path, modeled_parties) {
  results <- mrp_read_data(path)
  party_results <- results |>
    dplyr::mutate(
      partido_normalizado = mrp_normalize_electoral_party(partido_recode),
      partido = ifelse(
        partido_normalizado %in% modeled_parties,
        partido_normalizado,
        "OTBL"
      )
    ) |>
    dplyr::summarise(
      votos_prev = sum(votos, na.rm = TRUE),
      seats_prev = sum(representantes, na.rm = TRUE),
      .by = partido
    )
  blank_votes <- results |>
    dplyr::distinct(codigo_provincia, votos_blancos) |>
    dplyr::summarise(votos_prev = sum(votos_blancos, na.rm = TRUE)) |>
    dplyr::mutate(partido = "OTBL", seats_prev = 0)
  dplyr::bind_rows(party_results, blank_votes) |>
    dplyr::summarise(
      votos_prev = sum(votos_prev),
      seats_prev = sum(seats_prev),
      .by = partido
    ) |>
    dplyr::mutate(pct_prev = votos_prev / sum(votos_prev) * 100) |>
    dplyr::select(partido, pct_prev, seats_prev)
}

mrp_province_summary <- function(sims) {
  sims |>
    dplyr::summarise(
      voto_pct = mean(voto_pct) * 100,
      seats = mean(seats),
      .by = c(provincia, partido)
    ) |>
    dplyr::arrange(provincia, dplyr::desc(voto_pct))
}

mrp_previous_turnout <- function(path) {
  results <- mrp_read_data(path) |>
    dplyr::distinct(
      codigo_provincia, censo_ine, votos_validos, votos_nulos, abstenciones
    )
  electorate <- sum(results$censo_ine, na.rm = TRUE)
  if (!is.finite(electorate) || electorate <= 0) {
    electorate <- sum(
      results$votos_validos + results$votos_nulos + results$abstenciones,
      na.rm = TRUE
    )
  }
  100 * sum(results$votos_validos, na.rm = TRUE) / electorate
}

mrp_block_summary <- function(sims, parties, label) {
  totals <- sims |>
    dplyr::summarise(total_votos = sum(votos), .by = draw)
  sims |>
    dplyr::filter(partido %in% parties) |>
    dplyr::summarise(
      votos = sum(votos), seats = sum(seats), .by = draw
    ) |>
    dplyr::left_join(totals, by = "draw") |>
    dplyr::mutate(pct = 100 * votos / total_votos) |>
    dplyr::summarise(
      partido = label,
      pct_m = mean(pct),
      pct_lwr = unname(stats::quantile(pct, MRP_REPORT_INTERVAL[["lower"]])),
      pct_upr = unname(stats::quantile(pct, MRP_REPORT_INTERVAL[["upper"]])),
      seats_m = as.integer(round(mean(seats))),
      seats_lwr = as.integer(round(stats::quantile(
        seats, MRP_REPORT_INTERVAL[["lower"]]
      ))),
      seats_upr = as.integer(round(stats::quantile(
        seats, MRP_REPORT_INTERVAL[["upper"]]
      )))
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("pct"), ~ round(.x, 1))
    )
}

mrp_report_rows <- function(estimate, previous) {
  estimate |>
    dplyr::left_join(previous, by = "partido") |>
    dplyr::mutate(
      pct_change = pct_m - dplyr::coalesce(pct_prev, 0),
      seats_change = sprintf(
        "%+d (%+d/%+d)",
        seats_m - dplyr::coalesce(seats_prev, 0L),
        seats_lwr - dplyr::coalesce(seats_prev, 0L),
        seats_upr - dplyr::coalesce(seats_prev, 0L)
      ),
      seats_est = sprintf("%d (%d-%d)", seats_m, seats_lwr, seats_upr),
      seats_est = ifelse(partido == "OTBL", NA_character_, seats_est),
      seats_change = ifelse(
        partido == "OTBL" | seats_change == "+0 (+0/+0)",
        NA_character_,
        seats_change
      ),
      seats_est = dplyr::coalesce(seats_est, "-"),
      seats_change = dplyr::coalesce(seats_change, "-"),
      partido = gsub("_", " ", partido, fixed = TRUE)
    ) |>
    dplyr::transmute(
      partido,
      pct_prev,
      seats_prev = as.integer(seats_prev),
      pct_est = pct_m,
      seats_est,
      pct_change,
      seats_change
    )
}

mrp_national_report_data <- function(sims, previous_results, domain,
                                     turnout = NULL) {
  estimate <- mrp_summary(sims)
  known_parties <- union(
    estimate$partido,
    c(domain$left_parties, domain$right_parties)
  )
  previous <- mrp_previous_summary(previous_results, known_parties)
  parties <- mrp_report_rows(estimate, previous) |>
    dplyr::arrange(partido == "OTBL")

  block_estimate <- dplyr::bind_rows(
    mrp_block_summary(sims, domain$right_parties, "Derecha"),
    mrp_block_summary(sims, domain$left_parties, "Izquierda")
  )
  block_previous <- previous |>
    dplyr::mutate(
      bloque = dplyr::case_when(
        partido %in% domain$right_parties ~ "Derecha",
        partido %in% domain$left_parties ~ "Izquierda",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(bloque)) |>
    dplyr::summarise(
      pct_prev = sum(pct_prev),
      seats_prev = sum(seats_prev),
      .by = bloque
    ) |>
    dplyr::rename(partido = bloque)
  blocks <- mrp_report_rows(block_estimate, block_previous)

  participation <- NULL
  if (!is.null(turnout)) {
    turnout_pct <- if (turnout <= 1) 100 * turnout else turnout
    previous_pct <- mrp_previous_turnout(previous_results)
    participation <- data.frame(
      partido = "Participación",
      pct_prev = previous_pct,
      seats_prev = NA_integer_,
      pct_est = turnout_pct,
      seats_est = NA_character_,
      pct_change = turnout_pct - previous_pct,
      seats_change = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  list(parties = parties, blocks = blocks, participation = participation)
}

mrp_difference_colors <- function(data) {
  seats <- suppressWarnings(as.numeric(sub(" .*$", "", data$seats_change)))
  values <- cbind(data$pct_change, seats)
  ifelse(
    is.na(values) | values == 0,
    "black",
    ifelse(values > 0, "#70AD47", "#C00000")
  )
}

mrp_table_border <- function(flx, header = TRUE) {
  rule <- officer::fp_border(color = "#BFBFBF", width = 1.2)
  flx <- flextable::border_remove(flx)
  if (header) {
    flx <- flx |>
      flextable::border(
        j = 2:3, i = 1, part = "header", border = rule
      ) |>
      flextable::border(
        j = 4:5, i = 1, part = "header", border = rule
      ) |>
      flextable::border(
        j = 6:7, i = 1, part = "header", border = rule
      )
  } else {
    flx <- flextable::border(
      flx, j = 2:7, i = 1, part = "body", border.top = rule
    )
  }
  flx |>
    flextable::border(j = 2, part = "all", border.left = rule) |>
    flextable::border(j = 3, part = "all", border.right = rule) |>
    flextable::border(j = 5, part = "all", border.right = rule) |>
    flextable::border(j = 7, part = "all", border.right = rule) |>
    flextable::border(
      j = 2:7, i = flextable::nrow_part(flx, "body"),
      part = "body", border.bottom = rule
    )
}

mrp_flextable <- function(data, header = TRUE) {
  flx <- flextable::flextable(data)
  if (header) {
    flx <- flx |>
      flextable::set_header_labels(
        partido = "",
        pct_prev = "%",
        seats_prev = "Escaños",
        pct_est = "%",
        seats_est = "Escaños",
        pct_change = "p.p.",
        seats_change = "Escaños"
      ) |>
      flextable::add_header_row(
        values = c(
          "", "Elecciones Anteriores", "Estimación", "Diferencias"
        ),
        colwidths = c(1, 2, 2, 2)
      ) |>
      flextable::bold(j = 4:5, part = "header")
  } else {
    flx <- flextable::delete_part(flx, part = "header")
  }
  flx |>
    flextable::bold(j = 4:7, part = "body") |>
    flextable::align(j = "partido", align = "right", part = "all") |>
    flextable::align(j = 2:7, align = "center", part = "all") |>
    flextable::bg(j = 4:5, bg = "#F2F2F2", part = "all") |>
    flextable::colformat_double(
      j = c("pct_prev", "pct_est"), digits = 1,
      decimal.mark = ",", big.mark = ".", na_str = "-", suffix = "%"
    ) |>
    flextable::colformat_double(
      j = "pct_change", digits = 1, decimal.mark = ",",
      big.mark = ".", na_str = "-"
    ) |>
    flextable::colformat_int(
      j = "seats_prev", big.mark = "", na_str = "-"
    ) |>
    flextable::color(
      j = c("pct_change", "seats_change"),
      color = mrp_difference_colors(data)
    ) |>
    flextable::font(fontname = "Segoe UI", part = "all") |>
    flextable::fontsize(size = 8, part = "all") |>
    flextable::padding(padding = 0, padding.right = 4, part = "all") |>
    flextable::width(j = "partido", width = 2.31, unit = "cm") |>
    flextable::width(
      j = c("pct_prev", "pct_est", "pct_change"),
      width = 1.9, unit = "cm"
    ) |>
    flextable::width(j = "seats_prev", width = 1.9, unit = "cm") |>
    flextable::width(j = "seats_est", width = 2.32, unit = "cm") |>
    flextable::width(j = "seats_change", width = 2.31, unit = "cm") |>
    flextable::height(height = 0.58, unit = "cm", part = "body") |>
    flextable::height(height = 0.58, unit = "cm", part = "header") |>
    flextable::hrule(rule = "exact", part = "all") |>
    mrp_table_border(header = header)
}

mrp_simple_flextable <- function(data) {
  flx <- flextable::flextable(data) |>
    flextable::theme_vanilla() |>
    flextable::font(fontname = "Segoe UI", part = "all") |>
    flextable::fontsize(size = 8, part = "all") |>
    flextable::align(align = "center", part = "all")
  if ("voto_pct" %in% names(data)) {
    flx <- flx |>
      flextable::set_header_labels(
        provincia = "Provincia", partido = "Partido",
        voto_pct = "%", seats = "Escaños"
      ) |>
      flextable::colformat_double(
        j = "voto_pct", digits = 1, decimal.mark = ",",
        big.mark = ".", suffix = "%"
      )
  }
  flextable::autofit(flx)
}

mrp_write_docx <- function(path, national_tables, provinces) {
  doc <- officer::read_docx() |>
    flextable::body_add_flextable(national_tables$parties, align = "center") |>
    officer::body_add_par("")
  doc <- flextable::body_add_flextable(
    doc, national_tables$blocks, align = "center"
  ) |>
    officer::body_add_par("")
  if (!is.null(national_tables$participation)) {
    doc <- flextable::body_add_flextable(
      doc, national_tables$participation, align = "center"
    )
  }
  doc <- doc |>
    officer::body_add_break() |>
    officer::body_add_par("Estimación provincial", style = "heading 2") |>
    flextable::body_add_flextable(
      mrp_simple_flextable(provinces), align = "center"
    )
  print(doc, target = path)
  invisible(path)
}

mrp_write_xlsx <- function(path, national_tables, provinces, candidates) {
  workbook <- openxlsx2::wb_workbook() |>
    openxlsx2::wb_add_worksheet("Nacional") |>
    openxlsx2::wb_set_grid_lines("Nacional", show = FALSE)
  workbook <- flexlsx::wb_add_flextable(
    workbook, "Nacional", national_tables$parties, dims = "C2"
  )
  main_height <- flextable::nrow_part(
    national_tables$parties, "body"
  ) + flextable::nrow_part(national_tables$parties, "header")
  blocks_row <- 3L + main_height
  workbook <- flexlsx::wb_add_flextable(
    workbook, "Nacional", national_tables$blocks,
    dims = paste0("C", blocks_row)
  )
  if (!is.null(national_tables$participation)) {
    participation_row <- blocks_row +
      flextable::nrow_part(national_tables$blocks, "body") + 2L
    workbook <- flexlsx::wb_add_flextable(
      workbook, "Nacional", national_tables$participation,
      dims = paste0("C", participation_row)
    )
  }
  workbook <- workbook |>
    openxlsx2::wb_set_row_heights(
      "Nacional", rows = 1:100, heights = 17.25
    ) |>
    openxlsx2::wb_add_worksheet("Provincias") |>
    openxlsx2::wb_set_grid_lines("Provincias", show = FALSE)
  workbook <- flexlsx::wb_add_flextable(
    workbook, "Provincias", mrp_simple_flextable(provinces), dims = "C2"
  )
  workbook <- workbook |>
    openxlsx2::wb_add_worksheet("Candidaturas") |>
    openxlsx2::wb_add_data("Candidaturas", candidates)
  openxlsx2::wb_save(workbook, path, overwrite = TRUE)
  invisible(path)
}

mrp_write_availability_report <- function(path, survey_diagnostics, candidates,
                                          availability) {
  discarded <- survey_diagnostics$discarded %||% 0L
  before <- survey_diagnostics$before %||% NA_integer_
  after <- survey_diagnostics$after %||% NA_integer_
  unavailable <- sum(!candidates$se_presenta)
  lines <- c(
    "# Comprobación de candidaturas",
    "",
    sprintf("- Estado: **%s**", availability$status),
    sprintf("- Pares provincia–partido: %d", nrow(candidates)),
    sprintf("- Pares no disponibles: %d", unavailable),
    sprintf("- Filas CIS antes del filtro: %s", before),
    sprintf("- Filas CIS conservadas: %s", after),
    sprintf("- Filas CIS descartadas: %d", discarded),
    sprintf("- Máxima masa en candidaturas no disponibles: %.3g", availability$maximum),
    sprintf("- Tolerancia: %.3g", availability$tolerance),
    "",
    "La disponibilidad se impone dentro del likelihood y durante la predicción.",
    "Este informe solo comprueba que la masa prohibida es numéricamente nula."
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

mrp_write_recall_report <- function(path, recall) {
  national <- recall$national |>
    tidyr::pivot_wider(
      names_from = escenario, values_from = voto_pct, values_fill = 0
    ) |>
    dplyr::mutate(
      delta_pp = 100 * (official - model),
      model = 100 * model,
      official = 100 * official
    ) |>
    dplyr::arrange(dplyr::desc(official))
  national_rows <- sprintf(
    "| %s | %.2f | %.2f | %+.2f |",
    national$partido, national$model, national$official, national$delta_pp
  )
  contributions <- recall$contributions |>
    dplyr::mutate(contribucion_pp = 100 * contribucion_pct) |>
    dplyr::arrange(escenario, partido, dplyr::desc(contribucion_pp))
  contribution_rows <- sprintf(
    "| %s | %s | %s | %.2f |",
    contributions$escenario, contributions$partido,
    contributions$recuerdo, contributions$contribucion_pp
  )
  writeLines(
    c(
      "# Diagnóstico de calibración de recuerdo",
      "",
      sprintf("Peso del escenario oficial en la estimación: **%.2f**.", recall$weight),
      "",
      "## Estimación nacional por escenario",
      "",
      "| Partido | Recuerdo modelado | Recuerdo oficial | Diferencia (pp) |",
      "|---|---:|---:|---:|",
      national_rows,
      "",
      "## Contribución nacional por recuerdo",
      "",
      "| Escenario | Partido | Recuerdo | Contribución (pp) |",
      "|---|---|---|---:|",
      contribution_rows
    ),
    path,
    useBytes = TRUE
  )
  invisible(path)
}

mrp_write_reports <- function(sims, previous_results, paths, domain,
                              survey_diagnostics, candidates, availability,
                              recall, turnout = NULL) {
  national_data <- mrp_national_report_data(
    sims, previous_results, domain, turnout
  )
  national_tables <- list(
    parties = mrp_flextable(national_data$parties),
    blocks = mrp_flextable(national_data$blocks, header = FALSE),
    participation = if (is.null(national_data$participation)) {
      NULL
    } else {
      mrp_flextable(national_data$participation, header = FALSE)
    }
  )
  provinces <- mrp_province_summary(sims)

  dir.create(paths$output_root, recursive = TRUE, showWarnings = FALSE)
  mrp_write_docx(paths$docx, national_tables, provinces)
  mrp_write_xlsx(paths$xlsx, national_tables, provinces, candidates)
  mrp_write_availability_report(
    paths$availability_report, survey_diagnostics, candidates, availability
  )
  mrp_write_recall_report(paths$recall_report, recall)
  list(
    sims = paths$sims,
    word = paths$docx,
    excel = paths$xlsx,
    candidaturas = paths$availability_report,
    recuerdo = paths$recall_report
  )
}
