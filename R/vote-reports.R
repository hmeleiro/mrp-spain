MRP_VOTE_REPORT_VERSION <- 2L

mrp_vote_report_paths <- function(paths) {
  root <- file.path(paths$diagnostics_root, "voto")
  list(
    root = root,
    estimation_report = file.path(root, "informe_estimacion.md"),
    calibration_report = file.path(root, "informe_calibracion_nacional.md"),
    temporal_report = file.path(root, "informe_temporal.md"),
    demographic_report = file.path(root, "informe_grupos_sociodemograficos.md"),
    seat_report = file.path(root, "informe_ultimo_escano.md"),
    blocks_report = file.path(root, "informe_bloques.md"),
    geography_report = file.path(root, "informe_geografia_voto.md"),
    fit_report = file.path(root, "informe_ajuste_voto.md"),
    execution_report = file.path(root, "informe_ejecucion.md"),
    national_csv = file.path(root, "estimacion_nacional.csv"),
    calibration_csv = file.path(root, "calibracion_nacional.csv"),
    temporal_csv = file.path(root, "evolucion_temporal.csv"),
    demographic_strength_csv = file.path(root, "fortaleza_grupos.csv"),
    demographic_change_csv = file.path(root, "cambio_grupos.csv"),
    demographic_evolution_csv = file.path(root, "evolucion_grupos.csv"),
    seat_csv = file.path(root, "ultimo_escano.csv"),
    seat_probability_csv = file.path(root, "probabilidad_escanos_provincia.csv"),
    seat_battle_csv = file.path(root, "batalla_ultimo_escano.csv"),
    seat_battle_summary_csv = file.path(
      root, "batalla_ultimo_escano_resumen.csv"
    ),
    blocks_csv = file.path(root, "bloques.csv"),
    geography_csv = file.path(root, "geografia_voto.csv"),
    fit_csv = file.path(root, "ajuste_voto.csv"),
    national_vote_plot = file.path(root, "voto_nacional_intervalos.png"),
    national_seat_plot = file.path(root, "escanos_intervalos.png"),
    calibration_plot = file.path(root, "calibracion_nacional.png"),
    recall_source_plot = file.path(root, "recuerdo_origen_partido.png"),
    recall_destination_plot = file.path(root, "recuerdo_destino_voto.png"),
    temporal_plot = file.path(root, "evolucion_temporal.png"),
    demographic_strength_plot = file.path(root, "fortaleza_grupos.png"),
    demographic_change_plot = file.path(root, "cambio_grupos.png"),
    demographic_evolution_plot = file.path(root, "evolucion_grupos.png"),
    seat_plot = file.path(root, "competitividad_ultimo_escano.png"),
    seat_battle_party_plot = file.path(
      root, "batalla_ultimo_escano_partidos.png"
    ),
    seat_battle_province_plot = file.path(
      root, "batalla_ultimo_escano_provincias.png"
    ),
    blocks_plot = file.path(root, "distribucion_bloques.png"),
    geography_plot = file.path(root, "efectos_ccaa.png"),
    fit_plot = file.path(root, "residuos_periodo_partido.png")
  )
}

mrp_report_quantile <- function(x, probability) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  unname(stats::quantile(x, probability))
}

mrp_report_escape <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  gsub("|", "\\|", x, fixed = TRUE)
}

mrp_markdown_table <- function(data, align = NULL) {
  if (!ncol(data)) return(character())
  values <- lapply(data, mrp_report_escape)
  rows <- do.call(paste, c(values, sep = " | "))
  if (is.null(align)) align <- rep("---", ncol(data))
  c(
    paste0("| ", paste(names(data), collapse = " | "), " |"),
    paste0("|", paste(align, collapse = "|"), "|"),
    paste0("| ", rows, " |")
  )
}

mrp_write_markdown <- function(path, lines) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

mrp_report_theme <- function() {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

mrp_national_draws <- function(sims) {
  sims |>
    dplyr::summarise(
      votos = sum(votos), seats = sum(seats),
      .by = c(draw, partido)
    ) |>
    dplyr::mutate(voto_pct = votos / sum(votos), .by = draw)
}

mrp_national_uncertainty <- function(sims, previous_results = NULL) {
  draws <- mrp_national_draws(sims)
  summary <- draws |>
    dplyr::summarise(
      voto_media = mean(voto_pct),
      voto_p20 = mrp_report_quantile(
        voto_pct, MRP_REPORT_INTERVAL[["lower"]]
      ),
      voto_p50 = mrp_report_quantile(voto_pct, 0.50),
      voto_p80 = mrp_report_quantile(
        voto_pct, MRP_REPORT_INTERVAL[["upper"]]
      ),
      escanos_media = mean(seats),
      escanos_p20 = mrp_report_quantile(
        seats, MRP_REPORT_INTERVAL[["lower"]]
      ),
      escanos_p50 = mrp_report_quantile(seats, 0.50),
      escanos_p80 = mrp_report_quantile(
        seats, MRP_REPORT_INTERVAL[["upper"]]
      ),
      prob_escanos = mean(seats > 0),
      .by = partido
    )
  if (!is.null(previous_results)) {
    previous <- mrp_previous_summary(
      previous_results, unique(as.character(draws$partido))
    )
    summary <- summary |>
      dplyr::left_join(previous, by = "partido") |>
      dplyr::mutate(
        cambio_voto = 100 * voto_media - dplyr::coalesce(pct_prev, 0),
        cambio_escanos = escanos_media - dplyr::coalesce(seats_prev, 0)
      )
  }
  list(
    draws = draws,
    summary = summary |> dplyr::arrange(dplyr::desc(voto_media))
  )
}

mrp_write_estimation_report <- function(analysis, paths) {
  summary <- analysis$summary
  readr::write_csv(summary, paths$national_csv, na = "")

  focus <- mrp_report_focus_parties(summary$partido)
  ordered <- summary |>
    dplyr::filter(partido %in% focus) |>
    dplyr::mutate(partido = factor(partido, levels = rev(focus)))
  palette <- mrp_report_party_palette(focus)
  vote_plot <- ggplot2::ggplot(
    ordered,
    ggplot2::aes(x = 100 * voto_media, y = partido, color = partido)
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = 100 * voto_p20, xmax = 100 * voto_p80),
      width = 0.2, orientation = "y"
    ) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_color_manual(values = palette, guide = "none") +
    ggplot2::labs(
      title = "Estimación nacional de voto",
      subtitle = "Partidos nacionales; media e intervalo posterior P20–P80",
      x = "Voto nacional (%)", y = NULL
    ) +
    mrp_report_theme()
  ggplot2::ggsave(
    paths$national_vote_plot, vote_plot,
    width = 8, height = 6.5, dpi = 144
  )

  seats <- ordered
  seat_plot <- ggplot2::ggplot(
    seats,
    ggplot2::aes(x = escanos_media, y = partido, color = partido)
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = escanos_p20, xmax = escanos_p80),
      width = 0.2, orientation = "y"
    ) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_color_manual(values = palette, guide = "none") +
    ggplot2::labs(
      title = "Distribución posterior de escaños",
      subtitle = "Partidos nacionales; media e intervalo P20–P80",
      x = "Escaños", y = NULL
    ) +
    mrp_report_theme()
  ggplot2::ggsave(
    paths$national_seat_plot, seat_plot,
    width = 8, height = 6.5, dpi = 144
  )

  table <- summary |>
    dplyr::filter(partido %in% focus) |>
    dplyr::arrange(match(partido, focus)) |>
    dplyr::transmute(
      Partido = gsub("_", " ", partido, fixed = TRUE),
      `Voto medio` = sprintf("%.2f%%", 100 * voto_media),
      `Voto P20–P80` = sprintf(
        "%.2f–%.2f", 100 * voto_p20, 100 * voto_p80
      ),
      `Escaños medios` = sprintf("%.2f", escanos_media),
      `Escaños P20–P80` = sprintf(
        "%.0f–%.0f", escanos_p20, escanos_p80
      ),
      `P(al menos uno)` = sprintf("%.1f%%", 100 * prob_escanos),
      `Cambio voto` = if ("cambio_voto" %in% names(summary)) {
        sprintf("%+.2f", cambio_voto)
      } else {
        ""
      }
    )
  mrp_write_markdown(
    paths$estimation_report,
    c(
      "# Estimación electoral con incertidumbre",
      "",
      "Los intervalos son los percentiles 20 y 80 de los draws completos. Las",
      "medias de escaños no son una asignación electoral literal.",
      "",
      "![Intervalos de voto](voto_nacional_intervalos.png)",
      "",
      "![Intervalos de escaños](escanos_intervalos.png)",
      "",
      "## Resumen nacional",
      "",
      mrp_markdown_table(table),
      "",
      "La vista principal muestra PP, PSOE, Vox, FA, Podemos y SALF. El CSV",
      "conserva todos los partidos modelados.",
      "",
      sprintf(
        "Draws: **%d**. Datos tabulares: `%s`.",
        length(unique(analysis$draws$draw)), basename(paths$national_csv)
      )
    )
  )
}

mrp_calibration_analysis <- function(diagnostics) {
  if (!isTRUE(diagnostics$enabled) || is.null(diagnostics$margins)) {
    return(data.frame())
  }
  diagnostics$margins |>
    dplyr::summarise(
      inicial_media = mean(initial_pct),
      inicial_p20 = mrp_report_quantile(
        initial_pct, MRP_REPORT_INTERVAL[["lower"]]
      ),
      inicial_p80 = mrp_report_quantile(
        initial_pct, MRP_REPORT_INTERVAL[["upper"]]
      ),
      externo_media = mean(external_pct),
      base_media = mean(base_pct),
      final_media = mean(final_pct),
      final_p20 = mrp_report_quantile(
        final_pct, MRP_REPORT_INTERVAL[["lower"]]
      ),
      final_p80 = mrp_report_quantile(
        final_pct, MRP_REPORT_INTERVAL[["upper"]]
      ),
      cambio_pp = 100 * mean(final_pct - initial_pct),
      error_maximo = max(abs(achieved_pct - final_pct)),
      .by = category
    ) |>
    dplyr::arrange(dplyr::desc(final_media))
}

mrp_write_calibration_report <- function(analysis, diagnostics, config, paths) {
  if (!nrow(analysis)) {
    return(mrp_write_markdown(
      paths$calibration_report,
      c(
        "# Auditoría de calibración nacional", "",
        "La calibración nacional está desactivada para esta ejecución."
      )
    ))
  }
  readr::write_csv(analysis, paths$calibration_csv, na = "")
  focus <- mrp_report_focus_parties(analysis$category)
  plot_data <- analysis |>
    dplyr::filter(category %in% focus) |>
    dplyr::mutate(category = factor(category, levels = rev(focus)))
  palette <- mrp_report_party_palette(focus)
  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(y = category)) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 100 * inicial_media, xend = 100 * final_media,
        yend = category, color = category
      ),
      linewidth = 0.9, alpha = 0.45
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = 100 * inicial_media, color = category),
      shape = 1, size = 2.4
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = 100 * externo_media, color = category),
      shape = 4, size = 2.7
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = 100 * final_media, color = category),
      shape = 16, size = 2.4
    ) +
    ggplot2::scale_color_manual(values = palette, guide = "none") +
    ggplot2::labs(
      title = "Efecto de la calibración nacional",
      subtitle = "Círculo hueco: MRP; aspa: objetivo; círculo sólido: final",
      x = "Voto nacional medio (%)", y = NULL
    ) +
    mrp_report_theme()
  ggplot2::ggsave(
    paths$calibration_plot, plot,
    width = 8, height = 6.5, dpi = 144
  )

  source_path <- config$run$national_calibration$draws_path
  source_info <- if (file.exists(source_path)) file.info(source_path) else NULL
  table <- analysis |>
    dplyr::filter(category %in% focus) |>
    dplyr::arrange(match(category, focus)) |>
    dplyr::transmute(
      Partido = category,
      Inicial = sprintf("%.2f%%", 100 * inicial_media),
      Externo = sprintf("%.2f%%", 100 * externo_media),
      `Mezcla base` = sprintf("%.2f%%", 100 * base_media),
      Final = sprintf("%.2f%%", 100 * final_media),
      `Cambio (pp)` = sprintf("%+.2f", cambio_pp)
    )
  mrp_write_markdown(
    paths$calibration_report,
    c(
      "# Auditoría de calibración nacional", "",
      sprintf("- Estado: **%s**.", diagnostics$status),
      sprintf("- Intensidad general: **%.2f**.", diagnostics$strength),
      sprintf("- Intensidad OTBL: **%.2f**.", diagnostics$otbl_strength),
      sprintf("- Hash del objetivo: `%s`.", diagnostics$target_hash),
      if (!is.null(source_info)) sprintf(
        "- Fichero objetivo: `%s`; modificado: **%s**.",
        source_path, format(source_info$mtime, tz = "Europe/Madrid", usetz = TRUE)
      ) else sprintf("- Fichero objetivo: `%s`.", source_path),
      sprintf(
        "- Error máximo de los márgenes: **%.3g**.",
        max(analysis$error_maximo)
      ),
      "",
      "![Calibración nacional](calibracion_nacional.png)",
      "",
      mrp_markdown_table(table),
      "",
      "La vista muestra los seis partidos nacionales principales; el CSV",
      "mantiene todas las categorías calibradas, incluido el residual.",
      "",
      "La mezcla general es geométrica. Si la intensidad OTBL es uno, su margen",
      "se fija al objetivo externo y el complemento se reescala conservando su",
      "composición relativa.",
      "",
      sprintf("Datos tabulares: `%s`.", basename(paths$calibration_csv))
    )
  )
}

mrp_recall_target_audit <- function(frame_result, results_path, schema,
                                    tolerance = 1e-8) {
  base <- frame_result$frame$base
  levels <- levels(base$recuerdo)
  if (is.null(levels)) levels <- unique(as.character(base$recuerdo))
  targets <- mrp_previous_targets(results_path, schema, levels) |>
    dplyr::transmute(
      provincia = as.character(provincia),
      recuerdo = as.character(recuerdo),
      objetivo = target_share,
      votos_oficiales = votos
    )
  official <- frame_result$frame$scenarios$official$n_imputado
  constructed <- base |>
    dplyr::transmute(
      provincia = as.character(provincia),
      recuerdo = as.character(recuerdo),
      n = colMeans(official)
    ) |>
    dplyr::summarise(n = sum(n), .by = c(provincia, recuerdo)) |>
    dplyr::mutate(construido = n / sum(n), .by = provincia) |>
    dplyr::select(provincia, recuerdo, construido)
  audit <- targets |>
    dplyr::left_join(constructed, by = c("provincia", "recuerdo")) |>
    dplyr::mutate(
      construido = dplyr::coalesce(construido, 0),
      diferencia = construido - objetivo
    )
  positive_to_zero <- audit$votos_oficiales > 0 &
    audit$objetivo > tolerance & audit$construido <= tolerance
  maximum <- max(abs(audit$diferencia))
  status <- if (!any(positive_to_zero) && maximum <= tolerance) "PASS" else "FAIL"
  if (status != "PASS") {
    bad <- audit[positive_to_zero | abs(audit$diferencia) > tolerance, ]
    stop(
      "Official recall target audit failed for ", nrow(bad),
      " province-category pair(s); maximum difference = ",
      format(maximum, scientific = TRUE), ".",
      call. = FALSE
    )
  }
  list(status = status, maximum = maximum, tolerance = tolerance, data = audit)
}

mrp_recall_analysis <- function(recall, calibration) {
  national <- recall$national |>
    tidyr::pivot_wider(
      names_from = escenario, values_from = voto_pct, values_fill = 0
    ) |>
    dplyr::mutate(
      mix = (1 - recall$weight) * model + recall$weight * official,
      delta = official - model
    )
  if (isTRUE(calibration$enabled) && !is.null(calibration$achieved)) {
    final <- calibration$achieved |>
      dplyr::summarise(final = mean(achieved_pct), .by = partido)
    national <- national |> dplyr::left_join(final, by = "partido")
  } else {
    national$final <- national$mix
  }

  contributions <- recall$contributions |>
    dplyr::mutate(
      total_party = sum(contribucion_pct), .by = c(escenario, partido)
    ) |>
    dplyr::mutate(
      total_recall = sum(contribucion_pct), .by = c(escenario, recuerdo),
      dentro_partido = dplyr::if_else(
        total_party > 0, contribucion_pct / total_party, 0
      ),
      destino_recuerdo = dplyr::if_else(
        total_recall > 0, contribucion_pct / total_recall, 0
      )
    )
  missing_scenarios <- setdiff(
    c("model", "official"), unique(as.character(contributions$escenario))
  )
  if (length(missing_scenarios)) {
    stop(
      "Recall analysis is missing scenario(s): ",
      paste(missing_scenarios, collapse = ", "), ".",
      call. = FALSE
    )
  }
  mixed <- contributions |>
    dplyr::select(escenario, recuerdo, partido, contribucion_pct) |>
    tidyr::pivot_wider(
      names_from = escenario,
      values_from = contribucion_pct,
      values_fill = 0,
      names_prefix = "contribucion_pct_"
    ) |>
    dplyr::mutate(
      escenario = "mix",
      contribucion_pct =
        (1 - recall$weight) * contribucion_pct_model +
        recall$weight * contribucion_pct_official
    ) |>
    dplyr::mutate(
      total_party = sum(contribucion_pct), .by = partido
    ) |>
    dplyr::mutate(
      total_recall = sum(contribucion_pct), .by = recuerdo,
      dentro_partido = dplyr::if_else(
        total_party > 0, contribucion_pct / total_party, 0
      ),
      destino_recuerdo = dplyr::if_else(
        total_recall > 0, contribucion_pct / total_recall, 0
      )
    ) |>
    dplyr::select(
      escenario, recuerdo, partido, contribucion_pct,
      total_party, total_recall, dentro_partido, destino_recuerdo
    )
  list(
    national = national |> dplyr::arrange(dplyr::desc(final)),
    contributions = dplyr::bind_rows(contributions, mixed)
  )
}

mrp_write_recall_analysis <- function(analysis, audit, weight,
                                      report_path, paths) {
  contributions <- analysis$contributions
  focus <- mrp_report_focus_parties(contributions$partido)
  palette <- mrp_report_party_palette(focus)
  source_plot_data <- contributions |>
    dplyr::filter(
      escenario %in% c("model", "official"), partido %in% focus
    ) |>
    dplyr::mutate(
      partido = factor(partido, levels = focus),
      escenario = dplyr::recode(
        escenario, model = "Recuerdo modelado", official = "Recuerdo oficial"
      )
    )
  source_plot <- ggplot2::ggplot(
    source_plot_data,
    ggplot2::aes(
      x = partido, y = recuerdo, fill = partido,
      alpha = 100 * dentro_partido
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::facet_wrap(ggplot2::vars(escenario), ncol = 1) +
    ggplot2::scale_fill_manual(values = palette, guide = "none") +
    ggplot2::scale_alpha_continuous(
      range = c(0.06, 1), limits = c(0, 100), name = "% del\npartido"
    ) +
    ggplot2::labs(
      title = "Origen del electorado actual de cada partido",
      x = "Voto actual", y = "Recuerdo"
    ) +
    mrp_report_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(
    paths$recall_source_plot, source_plot,
    width = 10, height = 8, dpi = 144
  )

  destination_plot <- ggplot2::ggplot(
    source_plot_data,
    ggplot2::aes(
      x = partido, y = recuerdo, fill = partido,
      alpha = 100 * destino_recuerdo
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::facet_wrap(ggplot2::vars(escenario), ncol = 1) +
    ggplot2::scale_fill_manual(values = palette, guide = "none") +
    ggplot2::scale_alpha_continuous(
      range = c(0.06, 1), limits = c(0, 100), name = "% del\nrecuerdo"
    ) +
    ggplot2::labs(
      title = "Destino estimado de cada categoría de recuerdo",
      x = "Voto actual", y = "Recuerdo"
    ) +
    mrp_report_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(
    paths$recall_destination_plot, destination_plot,
    width = 10, height = 8, dpi = 144
  )

  national_table <- analysis$national |>
    dplyr::filter(partido %in% focus) |>
    dplyr::arrange(match(partido, focus)) |>
    dplyr::transmute(
      Partido = gsub("_", " ", partido, fixed = TRUE),
      Modelado = sprintf("%.2f%%", 100 * model),
      Oficial = sprintf("%.2f%%", 100 * official),
      `Diferencia (pp)` = sprintf("%+.2f", 100 * delta),
      Mezcla = sprintf("%.2f%%", 100 * mix),
      `Final calibrado` = sprintf("%.2f%%", 100 * final)
    )
  top <- contributions |>
    dplyr::filter(escenario == "mix", partido %in% focus) |>
    dplyr::arrange(partido, dplyr::desc(dentro_partido)) |>
    dplyr::slice_head(n = 3, by = partido) |>
    dplyr::transmute(
      Partido = gsub("_", " ", partido, fixed = TRUE),
      Recuerdo = recuerdo,
      `Contribución nacional` = sprintf("%.2f pp", 100 * contribucion_pct),
      `Peso dentro del partido` = sprintf("%.1f%%", 100 * dentro_partido)
    )
  audit_table <- audit$data |>
    dplyr::arrange(dplyr::desc(abs(diferencia))) |>
    dplyr::slice_head(n = 12) |>
    dplyr::transmute(
      Provincia = provincia,
      Recuerdo = recuerdo,
      Objetivo = sprintf("%.4f", objetivo),
      Construido = sprintf("%.4f", construido),
      Diferencia = sprintf("%+.2e", diferencia)
    )

  mrp_write_markdown(
    report_path,
    c(
      "# Diagnóstico de calibración de recuerdo", "",
      sprintf("Peso del escenario oficial: **%.2f**.", weight),
      "",
      "El escenario modelado usa la imputación multinivel de recuerdo. El",
      "escenario oficial recalibra cada provincia a los resultados anteriores.",
      "La mezcla se calcula antes de la calibración nacional externa.",
      "",
      "## Estimación nacional por escenario", "",
      mrp_markdown_table(national_table), "",
      "La diferencia es `oficial - modelado`. `Final calibrado` añade la",
      "calibración nacional y por eso puede no coincidir con la mezcla.",
      "",
      "## Origen del electorado de cada partido", "",
      "![Origen por recuerdo](diagnostics/voto/recuerdo_origen_partido.png)",
      "",
      mrp_markdown_table(top), "",
      "Las contribuciones son masa conjunta sobre el voto nacional. El peso",
      "dentro del partido sí está normalizado para sumar 100% por partido.",
      "",
      "## Destino estimado de cada recuerdo", "",
      "![Destino por recuerdo](diagnostics/voto/recuerdo_destino_voto.png)",
      "",
      "## Auditoría contra resultados oficiales", "",
      sprintf("- Estado: **%s**.", audit$status),
      sprintf("- Diferencia máxima: **%.3g**.", audit$maximum),
      sprintf("- Tolerancia: **%.3g**.", audit$tolerance),
      "",
      mrp_markdown_table(audit_table), "",
      "El pipeline aborta si una categoría con votos oficiales positivos queda",
      "en cero o si el escenario construido no reproduce sus márgenes."
    )
  )
}

mrp_temporal_log_shifts <- function(vote, ndraws) {
  temporal <- vote$temporal
  if (!identical(temporal$structure, "linear_varying")) {
    return(NULL)
  }
  periods <- temporal$periods
  reference <- vote$model$data[rep(1L, length(periods)), , drop = FALSE]
  period_dates <- if (temporal$unit == "day") {
    as.Date(periods)
  } else {
    as.Date(paste0(periods, "-01"))
  }
  indices <- if (temporal$unit == "day") {
    as.integer(period_dates)
  } else {
    as.integer(format(period_dates, "%Y")) * 12L +
      as.integer(format(period_dates, "%m"))
  }
  latest <- max(indices)
  reference$periodo <- factor(periods, levels = periods)
  reference$periodo_tendencia <- if (temporal$span > 0) {
    (indices - latest) / temporal$span
  } else {
    0
  }
  eta <- brms::posterior_linpred(
    vote$model,
    newdata = reference,
    ndraws = ndraws,
    re_formula = NULL,
    allow_new_levels = FALSE,
    transform = FALSE
  )
  if (length(dim(eta)) != 3L || dim(eta)[2] != length(periods)) {
    stop("Temporal linear predictor has unexpected dimensions.", call. = FALSE)
  }
  parties <- vote$parties
  nonreference <- setdiff(parties, vote$model$family$refcat)
  if (length(nonreference) != dim(eta)[3]) {
    nonreference <- setdiff(parties, "PSOE")
  }
  if (length(nonreference) != dim(eta)[3]) {
    stop("Temporal categories do not match the vote model.", call. = FALSE)
  }
  last <- length(periods)
  delta <- sweep(eta, c(1, 3), eta[, last, , drop = FALSE][, 1, ], `-`)
  delta_all <- array(
    0,
    dim = c(dim(eta)[1], length(periods), length(parties)),
    dimnames = list(NULL, periods, parties)
  )
  delta_all[, , match(nonreference, parties)] <- delta
  list(periods = periods, parties = parties, delta = delta_all)
}

mrp_temporal_index <- function(vote, national_draws, ndraws) {
  shifts <- mrp_temporal_log_shifts(vote, ndraws)
  if (is.null(shifts)) {
    return(list(summary = data.frame(), changes = data.frame()))
  }
  periods <- shifts$periods
  parties <- shifts$parties
  delta_all <- shifts$delta
  last <- length(periods)

  base <- national_draws |>
    dplyr::select(draw, partido, voto_pct) |>
    tidyr::pivot_wider(names_from = partido, values_from = voto_pct) |>
    dplyr::arrange(draw)
  base <- as.matrix(base[parties])
  n <- min(nrow(base), dim(delta_all)[1])
  base <- base[seq_len(n), , drop = FALSE]
  delta_all <- delta_all[seq_len(n), , , drop = FALSE]
  output <- vector("list", length(periods))
  for (period_index in seq_along(periods)) {
    shifted <- base * exp(delta_all[, period_index, ])
    shifted <- shifted / rowSums(shifted)
    output[[period_index]] <- data.frame(
      draw = seq_len(n),
      periodo = periods[period_index],
      partido = rep(parties, each = n),
      voto_pct = as.vector(shifted),
      stringsAsFactors = FALSE
    )
  }
  draws <- dplyr::bind_rows(output)
  summary <- draws |>
    dplyr::summarise(
      media = mean(voto_pct),
      p20 = mrp_report_quantile(
        voto_pct, MRP_REPORT_INTERVAL[["lower"]]
      ),
      p50 = mrp_report_quantile(voto_pct, 0.50),
      p80 = mrp_report_quantile(
        voto_pct, MRP_REPORT_INTERVAL[["upper"]]
      ),
      .by = c(periodo, partido)
    )
  endpoints <- draws |>
    dplyr::filter(periodo %in% c(periods[1], periods[last])) |>
    tidyr::pivot_wider(names_from = periodo, values_from = voto_pct)
  names(endpoints)[names(endpoints) == periods[1]] <- "first"
  names(endpoints)[names(endpoints) == periods[last]] <- "last"
  changes <- endpoints |>
    dplyr::mutate(change = last - first) |>
    dplyr::summarise(
      cambio_medio = mean(change),
      cambio_p20 = mrp_report_quantile(
        change, MRP_REPORT_INTERVAL[["lower"]]
      ),
      cambio_p80 = mrp_report_quantile(
        change, MRP_REPORT_INTERVAL[["upper"]]
      ),
      prob_sube = mean(change > 0),
      .by = partido
    ) |>
    dplyr::arrange(dplyr::desc(abs(cambio_medio)))
  list(summary = summary, changes = changes)
}

mrp_demographic_strength <- function(group_draws, national_draws) {
  if (!nrow(group_draws)) return(data.frame())
  national <- national_draws |>
    dplyr::select(draw, partido, nacional_pct = voto_pct)
  group_draws |>
    dplyr::mutate(
      votos_grupo = sum(votos),
      .by = c(draw, variable, nivel)
    ) |>
    dplyr::mutate(
      peso_grupo = votos_grupo / sum(votos),
      .by = c(draw, variable)
    ) |>
    dplyr::left_join(national, by = c("draw", "partido")) |>
    dplyr::mutate(sobreindice = voto_pct - nacional_pct) |>
    dplyr::summarise(
      apoyo_medio = mean(voto_pct),
      apoyo_p20 = mrp_report_quantile(
        voto_pct, MRP_REPORT_INTERVAL[["lower"]]
      ),
      apoyo_p80 = mrp_report_quantile(
        voto_pct, MRP_REPORT_INTERVAL[["upper"]]
      ),
      peso_grupo_medio = mean(peso_grupo),
      nacional_medio = mean(nacional_pct),
      sobreindice_medio = mean(sobreindice),
      sobreindice_p20 = mrp_report_quantile(
        sobreindice, MRP_REPORT_INTERVAL[["lower"]]
      ),
      sobreindice_p80 = mrp_report_quantile(
        sobreindice, MRP_REPORT_INTERVAL[["upper"]]
      ),
      prob_sobreindice = mean(sobreindice > 0),
      .by = c(variable, nivel, partido)
    )
}

mrp_demographic_temporal <- function(group_draws, shifts) {
  if (!nrow(group_draws) || is.null(shifts)) {
    return(list(summary = data.frame(), changes = data.frame()))
  }
  periods <- shifts$periods
  parties <- shifts$parties
  delta_all <- shifts$delta
  groups <- split(
    group_draws,
    interaction(group_draws$variable, group_draws$nivel, drop = TRUE)
  )
  summaries <- vector("list", length(groups))
  changes <- vector("list", length(groups))
  group_index <- 0L
  for (group in groups) {
    group_index <- group_index + 1L
    variable <- group$variable[[1]]
    level <- group$nivel[[1]]
    base <- group |>
      dplyr::select(draw, partido, voto_pct) |>
      tidyr::pivot_wider(names_from = partido, values_from = voto_pct) |>
      dplyr::arrange(draw)
    if (!all(parties %in% names(base))) {
      stop("Demographic draws do not contain every modeled party.", call. = FALSE)
    }
    base <- as.matrix(base[parties])
    n <- min(nrow(base), dim(delta_all)[1])
    base <- base[seq_len(n), , drop = FALSE]
    delta <- delta_all[seq_len(n), , , drop = FALSE]
    period_output <- vector("list", length(periods))
    endpoint <- vector("list", 2L)
    for (period_index in seq_along(periods)) {
      shifted <- base * exp(delta[, period_index, ])
      shifted <- shifted / rowSums(shifted)
      period_output[[period_index]] <- data.frame(
        variable = variable,
        nivel = level,
        periodo = periods[period_index],
        partido = parties,
        media = colMeans(shifted),
        p20 = apply(
          shifted, 2, mrp_report_quantile,
          probability = MRP_REPORT_INTERVAL[["lower"]]
        ),
        p80 = apply(
          shifted, 2, mrp_report_quantile,
          probability = MRP_REPORT_INTERVAL[["upper"]]
        ),
        stringsAsFactors = FALSE
      )
      if (period_index == 1L) endpoint[[1]] <- shifted
      if (period_index == length(periods)) endpoint[[2]] <- shifted
    }
    difference <- endpoint[[2]] - endpoint[[1]]
    changes[[group_index]] <- data.frame(
      variable = variable,
      nivel = level,
      partido = parties,
      inicio_medio = colMeans(endpoint[[1]]),
      actual_medio = colMeans(endpoint[[2]]),
      cambio_medio = colMeans(difference),
      cambio_p20 = apply(
        difference, 2, mrp_report_quantile,
        probability = MRP_REPORT_INTERVAL[["lower"]]
      ),
      cambio_p80 = apply(
        difference, 2, mrp_report_quantile,
        probability = MRP_REPORT_INTERVAL[["upper"]]
      ),
      prob_sube = colMeans(difference > 0),
      stringsAsFactors = FALSE
    )
    summaries[[group_index]] <- dplyr::bind_rows(period_output)
  }
  list(
    summary = dplyr::bind_rows(summaries),
    changes = dplyr::bind_rows(changes)
  )
}

mrp_demographic_analysis <- function(group_draws, national_draws, vote,
                                     ndraws) {
  temporal <- mrp_demographic_temporal(
    group_draws, mrp_temporal_log_shifts(vote, ndraws)
  )
  list(
    strength = mrp_demographic_strength(group_draws, national_draws),
    evolution = temporal$summary,
    changes = temporal$changes
  )
}

mrp_write_temporal_report <- function(analysis, temporal, paths) {
  if (!nrow(analysis$summary)) {
    return(mrp_write_markdown(
      paths$temporal_report,
      c(
        "# Evolución temporal del voto", "",
        "La estructura temporal está desactivada para esta ejecución."
      )
    ))
  }
  readr::write_csv(analysis$summary, paths$temporal_csv, na = "")
  focus <- mrp_report_focus_parties(analysis$summary$partido)
  ordered <- analysis$summary |>
    dplyr::filter(partido %in% focus) |>
    dplyr::mutate(
      periodo = factor(periodo, levels = temporal$periods),
      partido = factor(partido, levels = focus)
    )
  palette <- mrp_report_party_palette(focus)
  plot <- ggplot2::ggplot(
    ordered,
    ggplot2::aes(x = periodo, y = 100 * media, color = partido, group = partido)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = 100 * p20, ymax = 100 * p80, fill = partido),
      alpha = 0.12, color = NA, show.legend = FALSE
    ) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(
      title = "Índice temporal estandarizado de voto",
      subtitle = "Partidos nacionales; composición fija e intervalo P20–P80",
      x = NULL, y = "Voto nacional estandarizado (%)", color = "Partido"
    ) +
    mrp_report_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(
    paths$temporal_plot, plot,
    width = 11, height = 7, dpi = 144
  )
  table <- analysis$changes |>
    dplyr::filter(partido %in% focus) |>
    dplyr::arrange(match(partido, focus)) |>
    dplyr::transmute(
      Partido = gsub("_", " ", partido, fixed = TRUE),
      `Cambio medio (pp)` = sprintf("%+.2f", 100 * cambio_medio),
      `P20–P80 (pp)` = sprintf(
        "%+.2f–%+.2f", 100 * cambio_p20, 100 * cambio_p80
      ),
      `P(sube)` = sprintf("%.1f%%", 100 * prob_sube)
    )
  mrp_write_markdown(
    paths$temporal_report,
    c(
      "# Evolución temporal del voto", "",
      sprintf(
        "Ventana: **%s–%s**; %d periodos; amplitud de calendario: %d %s.",
        temporal$first_period, temporal$last_period, temporal$n_periods,
        temporal$span, ifelse(temporal$unit == "day", "días", "meses")
      ),
      "",
      "![Evolución temporal](evolucion_temporal.png)", "",
      mrp_markdown_table(table), "",
      "La trayectoria mantiene fija la composición nacional del último periodo",
      "y aplica, draw a draw, la tendencia y la desviación temporal estimadas.",
      "Es un índice temporal estandarizado: no mezcla cambios en la composición",
      "muestral y no constituye un pronóstico posterior al último periodo. Se",
      "calcula antes de aplicar el objetivo nacional externo.",
      "",
      sprintf("Datos tabulares: `%s`.", basename(paths$temporal_csv))
    )
  )
}

mrp_demographic_variable_label <- function(variable) {
  labels <- c(
    sexo = "Sexo",
    edad_grupo = "Edad",
    estudios = "Estudios",
    sitlab2 = "Situación laboral",
    tamuni = "Tamaño de municipio"
  )
  unname(labels[variable] %||% variable)
}

mrp_demographic_level_label <- function(variable, level) {
  labels <- list(
    estudios = c(
      `1` = "Hasta bachillerato",
      `2` = "Formación profesional",
      `3` = "Universitarios"
    ),
    sitlab2 = c(
      `1` = "Activos",
      `2` = "Jubilados/pensionistas",
      `3` = "Estudiantes",
      `4` = "Trabajo doméstico",
      `5` = "Otros"
    ),
    tamuni = c(
      `1` = "Hasta 10.000 habitantes",
      `2` = "10.001–50.000 habitantes",
      `3` = "Más de 50.000 habitantes"
    )
  )
  mapping <- labels[[variable]]
  label <- if (is.null(mapping)) NA_character_ else unname(mapping[level])
  if (length(label) != 1L || is.na(label)) as.character(level) else label
}

mrp_demographic_labels <- function(data) {
  data |>
    dplyr::mutate(
      variable_etiqueta = vapply(
        variable, mrp_demographic_variable_label, character(1)
      ),
      nivel_etiqueta = vapply(
        seq_len(dplyr::n()),
        function(index) mrp_demographic_level_label(
          variable[index], nivel[index]
        ),
        character(1)
      ),
      grupo = paste(variable_etiqueta, nivel_etiqueta, sep = " · ")
    )
}

mrp_write_demographic_report <- function(analysis, temporal, paths) {
  dir.create(paths$root, recursive = TRUE, showWarnings = FALSE)
  strength <- mrp_demographic_labels(analysis$strength)
  readr::write_csv(strength, paths$demographic_strength_csv, na = "")
  focus <- mrp_report_focus_parties(strength$partido)
  strength_plot_data <- strength |>
    dplyr::filter(partido %in% focus) |>
    dplyr::mutate(
      partido = factor(partido, levels = focus),
      grupo = factor(grupo, levels = rev(unique(grupo)))
    )
  strength_plot <- ggplot2::ggplot(
    strength_plot_data,
    ggplot2::aes(x = partido, y = grupo, fill = 100 * sobreindice_medio)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%+.1f", 100 * sobreindice_medio)),
      size = 2.5
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0
    ) +
    ggplot2::labs(
      title = "Fortaleza relativa por grupo sociodemográfico",
      subtitle = "Sobreíndice frente al voto nacional modelado; puntos porcentuales",
      x = NULL, y = NULL, fill = "pp"
    ) +
    mrp_report_theme()
  ggplot2::ggsave(
    paths$demographic_strength_plot, strength_plot,
    width = 11, height = max(6, 0.35 * length(unique(strength$grupo))), dpi = 144,
    limitsize = FALSE
  )

  strength_table <- strength |>
    dplyr::filter(partido %in% focus) |>
    dplyr::slice_max(
      sobreindice_medio, n = 2, with_ties = FALSE, by = partido
    ) |>
    dplyr::arrange(match(partido, focus), dplyr::desc(sobreindice_medio)) |>
    dplyr::transmute(
      Partido = partido,
      Grupo = grupo,
      `Apoyo medio` = sprintf("%.1f%%", 100 * apoyo_medio),
      `Sobreíndice` = sprintf("%+.1f pp", 100 * sobreindice_medio),
      `Peso electorado` = sprintf("%.1f%%", 100 * peso_grupo_medio),
      `P(sobreíndice > 0)` = sprintf("%.0f%%", 100 * prob_sobreindice)
    )

  change_lines <- c(
    "La estructura temporal está desactivada; no se estima cambio desde el inicio."
  )
  if (nrow(analysis$changes)) {
    changes <- mrp_demographic_labels(analysis$changes)
    evolution <- mrp_demographic_labels(analysis$evolution)
    readr::write_csv(changes, paths$demographic_change_csv, na = "")
    readr::write_csv(evolution, paths$demographic_evolution_csv, na = "")

    change_plot_data <- changes |>
      dplyr::filter(partido %in% focus) |>
      dplyr::mutate(
        partido = factor(partido, levels = focus),
        grupo = factor(grupo, levels = rev(unique(grupo)))
      )
    change_plot <- ggplot2::ggplot(
      change_plot_data,
      ggplot2::aes(x = partido, y = grupo, fill = 100 * cambio_medio)
    ) +
      ggplot2::geom_tile(color = "white", linewidth = 0.2) +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%+.1f", 100 * cambio_medio)),
        size = 2.5
      ) +
      ggplot2::scale_fill_gradient2(
        low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0
      ) +
      ggplot2::labs(
        title = "Cambio modelado desde el inicio del tracking",
        subtitle = "Composición fija; puntos porcentuales",
        x = NULL, y = NULL, fill = "pp"
      ) +
      mrp_report_theme()
    ggplot2::ggsave(
      paths$demographic_change_plot, change_plot,
      width = 11, height = max(6, 0.35 * length(unique(changes$grupo))),
      dpi = 144, limitsize = FALSE
    )

    evolution_plot_data <- evolution |>
      dplyr::filter(partido %in% focus) |>
      dplyr::mutate(
        periodo = factor(periodo, levels = temporal$periods),
        partido = factor(partido, levels = focus)
      ) |>
      dplyr::arrange(variable, nivel, partido, periodo) |>
      dplyr::mutate(
        cambio_inicio = media - dplyr::first(media),
        .by = c(variable, nivel, partido)
      )
    evolution_plot <- ggplot2::ggplot(
      evolution_plot_data,
      ggplot2::aes(
        x = periodo, y = 100 * cambio_inicio,
        color = nivel_etiqueta, group = nivel_etiqueta
      )
    ) +
      ggplot2::geom_hline(yintercept = 0, color = "grey75", linewidth = 0.3) +
      ggplot2::geom_line(linewidth = 0.45) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(variable_etiqueta),
        cols = ggplot2::vars(partido), scales = "free_y"
      ) +
      ggplot2::labs(
        title = "Evolución modelada por grupo",
        subtitle = "Cambio respecto al primer periodo; composición fija",
        x = NULL, y = "Cambio (pp)", color = "Nivel"
      ) +
      mrp_report_theme() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    ggplot2::ggsave(
      paths$demographic_evolution_plot, evolution_plot,
      width = 16, height = max(9, 2.4 * length(unique(evolution$variable))),
      dpi = 144, limitsize = FALSE
    )

    change_table <- changes |>
      dplyr::filter(partido %in% focus) |>
      dplyr::slice_max(
        abs(cambio_medio), n = 2, with_ties = FALSE, by = partido
      ) |>
      dplyr::arrange(match(partido, focus), dplyr::desc(abs(cambio_medio))) |>
      dplyr::transmute(
        Partido = partido,
        Grupo = grupo,
        `Cambio medio` = sprintf("%+.1f pp", 100 * cambio_medio),
        `P20–P80` = sprintf(
          "%+.1f–%+.1f", 100 * cambio_p20, 100 * cambio_p80
        ),
        `P(sube)` = sprintf("%.0f%%", 100 * prob_sube)
      )
    change_lines <- c(
      "## Cambio desde el inicio", "",
      "![Cambio por grupos](cambio_grupos.png)", "",
      mrp_markdown_table(change_table), "",
      "![Evolución por grupos](evolucion_grupos.png)", "",
      sprintf(
        "Ventana temporal: **%s–%s**.",
        temporal$first_period, temporal$last_period
      ),
      ""
    )
  }

  mrp_write_markdown(
    paths$demographic_report,
    c(
      "# Grupos sociodemográficos y voto", "",
      "## Fortaleza actual", "",
      "![Fortaleza por grupos](fortaleza_grupos.png)", "",
      mrp_markdown_table(strength_table), "",
      change_lines,
      "## Cómo interpretar este diagnóstico", "",
      "El apoyo es la cuota de voto del partido dentro de cada grupo entre",
      "votantes esperados. El sobreíndice resta la cuota nacional del mismo",
      "draw: valores positivos identifican grupos donde el partido es",
      "relativamente más fuerte.", "",
      "Las cifras usan el MRP y la calibración de participación y recuerdo,",
      "pero se calculan antes de la calibración nacional externa, que no tiene",
      "una descomposición sociodemográfica identificada.", "",
      "La fórmula actual aplica una tendencia temporal común por partido y no",
      "incluye interacciones tiempo×grupo. Por ello, las diferencias de cambio",
      "entre grupos son una evolución estandarizada implícita en sus niveles de",
      "apoyo, no evidencia de pendientes temporales específicas de cada grupo.",
      "Tampoco deben interpretarse como efectos causales de pertenecer al grupo.",
      "",
      sprintf(
        "Datos: `%s`%s.",
        basename(paths$demographic_strength_csv),
        if (nrow(analysis$changes)) {
          paste0(
            ", `", basename(paths$demographic_change_csv), "` y `",
            basename(paths$demographic_evolution_csv), "`"
          )
        } else {
          ""
        }
      )
    )
  )
}

mrp_last_seat_analysis <- function(sims, threshold = 0.03) {
  allocations <- sims |>
    dplyr::filter(seats > 0) |>
    dplyr::arrange(draw, provincia, partido) |>
    dplyr::summarise(
      asignacion = paste0(partido, "=", seats, collapse = ";"),
      .by = c(draw, provincia)
    )
  n_draws <- dplyr::n_distinct(sims$draw)
  stability <- allocations |>
    dplyr::count(provincia, asignacion, name = "draws") |>
    dplyr::arrange(provincia, dplyr::desc(draws), asignacion) |>
    dplyr::slice_head(n = 1, by = provincia) |>
    dplyr::mutate(
      prob_asignacion_modal = draws / n_draws
    ) |>
    dplyr::select(provincia, asignacion_modal = asignacion,
                  prob_asignacion_modal)

  eligible <- sims |>
    dplyr::mutate(
      valid_votes = sum(votos),
      district_share = dplyr::if_else(valid_votes > 0, votos / valid_votes, 0),
      eligible = partido != "OTBL" & district_share >= threshold,
      winning_quotient = dplyr::if_else(
        eligible & seats > 0, votos / seats, Inf
      ),
      next_quotient = dplyr::if_else(
        eligible, votos / (seats + 1), -Inf
      ),
      .by = c(draw, provincia)
    )
  winners <- eligible |>
    dplyr::filter(is.finite(winning_quotient)) |>
    dplyr::arrange(draw, provincia, winning_quotient, partido) |>
    dplyr::slice_head(n = 1, by = c(draw, provincia)) |>
    dplyr::transmute(
      draw, provincia,
      ganador_ultimo = partido,
      cociente_ganador = winning_quotient
    )
  challengers <- eligible |>
    dplyr::inner_join(
      winners |> dplyr::select(draw, provincia, ganador_ultimo),
      by = c("draw", "provincia")
    ) |>
    dplyr::filter(eligible, partido != ganador_ultimo) |>
    dplyr::arrange(draw, provincia, dplyr::desc(next_quotient), partido) |>
    dplyr::slice_head(n = 1, by = c(draw, provincia)) |>
    dplyr::transmute(
      draw, provincia,
      aspirante = partido,
      cociente_aspirante = next_quotient
    )
  margins <- winners |>
    dplyr::left_join(challengers, by = c("draw", "provincia")) |>
    dplyr::mutate(
      margen_relativo = dplyr::if_else(
        cociente_ganador > 0,
        (cociente_ganador - cociente_aspirante) / cociente_ganador,
        NA_real_
      )
    )
  bad_margins <- margins |>
    dplyr::filter(is.finite(margen_relativo), margen_relativo < -1e-10)
  if (nrow(bad_margins)) {
    stop(
      "Last-seat quotients are inconsistent with the D'Hondt allocation in ",
      nrow(bad_margins), " draw-province pair(s).",
      call. = FALSE
    )
  }
  modal_pairs <- margins |>
    dplyr::filter(!is.na(aspirante)) |>
    dplyr::count(
      provincia, ganador_ultimo, aspirante, name = "draws_pareja"
    ) |>
    dplyr::arrange(
      provincia, dplyr::desc(draws_pareja), ganador_ultimo, aspirante
    ) |>
    dplyr::slice_head(n = 1, by = provincia) |>
    dplyr::transmute(
      provincia,
      defensor_modal = ganador_ultimo,
      aspirante_modal = aspirante,
      prob_pareja_modal = draws_pareja / n_draws
    )
  summary <- margins |>
    dplyr::summarise(
      margen_mediano = stats::median(margen_relativo, na.rm = TRUE),
      margen_p20 = mrp_report_quantile(
        margen_relativo, MRP_REPORT_INTERVAL[["lower"]]
      ),
      margen_p80 = mrp_report_quantile(
        margen_relativo, MRP_REPORT_INTERVAL[["upper"]]
      ),
      .by = provincia
    ) |>
    dplyr::left_join(modal_pairs, by = "provincia") |>
    dplyr::left_join(stability, by = "provincia") |>
    dplyr::arrange(prob_asignacion_modal, margen_mediano)
  parties <- setdiff(unique(as.character(sims$partido)), "OTBL")
  role_probability <- tidyr::expand_grid(
    provincia = unique(as.character(sims$provincia)),
    partido = parties
  ) |>
    dplyr::left_join(
      margins |>
        dplyr::count(
          provincia, partido = ganador_ultimo, name = "draws_defensor"
        ),
      by = c("provincia", "partido")
    ) |>
    dplyr::left_join(
      margins |>
        dplyr::filter(!is.na(aspirante)) |>
        dplyr::count(
          provincia, partido = aspirante, name = "draws_aspirante"
        ),
      by = c("provincia", "partido")
    ) |>
    dplyr::mutate(
      draws_defensor = dplyr::coalesce(draws_defensor, 0L),
      draws_aspirante = dplyr::coalesce(draws_aspirante, 0L),
      prob_defensor = draws_defensor / n_draws,
      prob_aspirante = draws_aspirante / n_draws,
      prob_involucrado = prob_defensor + prob_aspirante
    ) |>
    dplyr::select(
      provincia, partido,
      prob_defensor, prob_aspirante, prob_involucrado
    )
  any_battle <- dplyr::bind_rows(
    margins |>
      dplyr::transmute(draw, partido = ganador_ultimo),
    margins |>
      dplyr::filter(!is.na(aspirante)) |>
      dplyr::transmute(draw, partido = aspirante)
  ) |>
    dplyr::distinct(draw, partido) |>
    dplyr::count(partido, name = "draws_alguna_batalla")
  party_summary <- role_probability |>
    dplyr::summarise(
      provincias_defensor = sum(prob_defensor),
      provincias_aspirante = sum(prob_aspirante),
      provincias_involucrado = sum(prob_involucrado),
      .by = partido
    ) |>
    dplyr::left_join(any_battle, by = "partido") |>
    dplyr::mutate(
      draws_alguna_batalla = dplyr::coalesce(draws_alguna_batalla, 0L),
      prob_alguna_batalla = draws_alguna_batalla / n_draws
    ) |>
    dplyr::arrange(dplyr::desc(provincias_involucrado))
  leaders <- role_probability |>
    dplyr::arrange(provincia, dplyr::desc(prob_involucrado), partido) |>
    dplyr::slice_head(n = 2, by = provincia) |>
    dplyr::summarise(
      partidos_principales = paste0(
        partido, " ", sprintf("%.0f%%", 100 * prob_involucrado),
        collapse = "; "
      ),
      .by = provincia
    )
  summary <- summary |>
    dplyr::left_join(leaders, by = "provincia")
  seat_probability <- sims |>
    dplyr::summarise(
      prob_escanos = mean(seats > 0),
      escanos_medios = mean(seats),
      .by = c(provincia, partido)
    ) |>
    dplyr::filter(partido != "OTBL")
  list(
    draws = margins,
    summary = summary,
    seat_probability = seat_probability,
    role_probability = role_probability,
    party_summary = party_summary
  )
}

mrp_write_last_seat_report <- function(analysis, paths) {
  readr::write_csv(analysis$summary, paths$seat_csv, na = "")
  readr::write_csv(
    analysis$seat_probability, paths$seat_probability_csv, na = ""
  )
  readr::write_csv(analysis$role_probability, paths$seat_battle_csv, na = "")
  readr::write_csv(
    analysis$party_summary, paths$seat_battle_summary_csv, na = ""
  )
  focus <- mrp_report_focus_parties(analysis$party_summary$partido)
  plot_data <- analysis$summary |>
    dplyr::arrange(dplyr::desc(margen_mediano)) |>
    dplyr::mutate(
      provincia = factor(provincia, levels = provincia),
      defensor_grafico = dplyr::if_else(
        defensor_modal %in% focus, defensor_modal, "Otros/regionales"
      )
    )
  defender_palette <- mrp_report_party_palette(plot_data$defensor_grafico)
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = 100 * margen_mediano, y = provincia,
                 color = defensor_grafico)
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = 100 * margen_p20, xmax = 100 * margen_p80),
      width = 0.12, orientation = "y", alpha = 0.5
    ) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::scale_color_manual(values = defender_palette) +
    ggplot2::labs(
      title = "Competitividad del último escaño provincial",
      subtitle = "Margen D'Hondt mediano e intervalo P20–P80",
      x = "Margen relativo (%)", y = "Provincia", color = "Defensor modal"
    ) +
    mrp_report_theme()
  ggplot2::ggsave(
    paths$seat_plot, plot,
    width = 9, height = 10, dpi = 144
  )
  palette <- mrp_report_party_palette(focus)
  party_plot_data <- analysis$party_summary |>
    dplyr::filter(partido %in% focus) |>
    dplyr::select(
      partido, provincias_defensor,
      provincias_aspirante, provincias_involucrado
    ) |>
    tidyr::pivot_longer(
      -partido, names_to = "rol", values_to = "provincias_esperadas"
    ) |>
    dplyr::mutate(
      partido = factor(partido, levels = rev(focus)),
      rol = dplyr::recode(
        rol,
        provincias_defensor = "Defiende",
        provincias_aspirante = "Aspira",
        provincias_involucrado = "Involucrado"
      ),
      rol = factor(rol, levels = c("Defiende", "Aspira", "Involucrado"))
    )
  party_plot <- ggplot2::ggplot(
    party_plot_data,
    ggplot2::aes(x = provincias_esperadas, y = partido, fill = partido)
  ) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::facet_wrap(ggplot2::vars(rol), nrow = 1) +
    ggplot2::scale_fill_manual(values = palette, guide = "none") +
    ggplot2::labs(
      title = "Participación en la batalla por el último escaño",
      subtitle = "Número esperado de provincias en cada rol",
      x = "Provincias esperadas", y = NULL
    ) +
    mrp_report_theme()
  ggplot2::ggsave(
    paths$seat_battle_party_plot, party_plot,
    width = 11, height = 5.5, dpi = 144
  )
  province_plot_data <- analysis$role_probability |>
    dplyr::filter(partido %in% focus) |>
    dplyr::mutate(partido = factor(partido, levels = focus))
  province_order <- province_plot_data |>
    dplyr::summarise(
      maximo = max(prob_involucrado), .by = provincia
    ) |>
    dplyr::arrange(maximo, provincia) |>
    dplyr::pull(provincia)
  province_plot_data <- province_plot_data |>
    dplyr::mutate(
      provincia = factor(provincia, levels = province_order)
    )
  province_plot <- ggplot2::ggplot(
    province_plot_data,
    ggplot2::aes(
      x = partido, y = provincia,
      fill = partido, alpha = prob_involucrado
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.15) +
    ggplot2::scale_fill_manual(values = palette, guide = "none") +
    ggplot2::scale_alpha_continuous(
      limits = c(0, 1), range = c(0.04, 1),
      name = "P(involucrado)"
    ) +
    ggplot2::labs(
      title = "Probabilidad de intervenir en la batalla marginal",
      subtitle = "Como defensor o como aspirante, por provincia",
      x = NULL, y = "Provincia"
    ) +
    mrp_report_theme()
  ggplot2::ggsave(
    paths$seat_battle_province_plot, province_plot,
    width = 9, height = 10, dpi = 144
  )
  party_table <- analysis$party_summary |>
    dplyr::filter(partido %in% focus) |>
    dplyr::arrange(match(partido, focus)) |>
    dplyr::transmute(
      Partido = partido,
      `Provincias defendiendo` = sprintf("%.2f", provincias_defensor),
      `Provincias aspirando` = sprintf("%.2f", provincias_aspirante),
      `Provincias involucradas` = sprintf("%.2f", provincias_involucrado),
      `P(en alguna batalla)` = sprintf("%.1f%%", 100 * prob_alguna_batalla)
    )
  table <- analysis$summary |>
    dplyr::slice_head(n = 20) |>
    dplyr::transmute(
      Provincia = provincia,
      `Asignación modal` = asignacion_modal,
      `P(asignación modal)` = sprintf("%.1f%%", 100 * prob_asignacion_modal),
      `Pareja modal` = paste(defensor_modal, aspirante_modal, sep = " ↔ "),
      `P(pareja modal)` = sprintf("%.1f%%", 100 * prob_pareja_modal),
      `Margen P20–P80` = sprintf(
        "%.2f–%.2f%%", 100 * margen_p20, 100 * margen_p80
      ),
      `Partidos más involucrados` = partidos_principales
    )
  psoe_table <- analysis$role_probability |>
    dplyr::filter(partido == "PSOE") |>
    dplyr::arrange(dplyr::desc(prob_involucrado), provincia) |>
    dplyr::slice_head(n = 15) |>
    dplyr::transmute(
      Provincia = provincia,
      `P(defensor)` = sprintf("%.1f%%", 100 * prob_defensor),
      `P(aspirante)` = sprintf("%.1f%%", 100 * prob_aspirante),
      `P(involucrado)` = sprintf("%.1f%%", 100 * prob_involucrado)
    )
  mrp_write_markdown(
    paths$seat_report,
    c(
      "# Batalla por el último escaño", "",
      "![Competitividad provincial](competitividad_ultimo_escano.png)", "",
      "![Resumen por partido](batalla_ultimo_escano_partidos.png)", "",
      "![Probabilidad provincial](batalla_ultimo_escano_provincias.png)", "",
      "## Resumen por partido", "",
      mrp_markdown_table(party_table), "",
      "## Provincias con asignación más incierta", "",
      mrp_markdown_table(table), "",
      "## Provincias donde el PSOE está más involucrado", "",
      mrp_markdown_table(psoe_table), "",
      "En cada draw, el defensor posee el último escaño y el aspirante es otro",
      "partido con el siguiente cociente D'Hondt más alto. Los roles son",
      "mutuamente excluyentes dentro de cada draw. La pareja modal se calcula",
      "conjuntamente, por lo que nunca presenta al mismo partido en ambos roles.",
      "P(involucrado) suma la probabilidad de defender y la de aspirar. El margen",
      "compara ambos cocientes; un valor pequeño identifica un escaño sensible.",
      "",
      sprintf(
        "Datos completos: `%s`, `%s`, `%s` y `%s`.",
        basename(paths$seat_csv), basename(paths$seat_battle_csv),
        basename(paths$seat_battle_summary_csv),
        basename(paths$seat_probability_csv)
      )
    )
  )
}

mrp_blocks_analysis <- function(national_draws, domain) {
  definitions <- list(
    Derecha = intersect(domain$right_parties, unique(national_draws$partido)),
    Izquierda = intersect(domain$left_parties, unique(national_draws$partido))
  )
  output <- lapply(names(definitions), function(label) {
    parties <- definitions[[label]]
    if (!length(parties)) return(NULL)
    national_draws |>
      dplyr::filter(partido %in% parties) |>
      dplyr::summarise(
        seats = sum(seats), voto_pct = sum(voto_pct), .by = draw
      ) |>
      dplyr::mutate(
        bloque = label,
        miembros = paste(parties, collapse = ", ")
      )
  })
  draws <- dplyr::bind_rows(output)
  summary <- draws |>
    dplyr::summarise(
      miembros = dplyr::first(miembros),
      voto_medio = mean(voto_pct),
      escanos_media = mean(seats),
      escanos_p20 = mrp_report_quantile(
        seats, MRP_REPORT_INTERVAL[["lower"]]
      ),
      escanos_p80 = mrp_report_quantile(
        seats, MRP_REPORT_INTERVAL[["upper"]]
      ),
      prob_160 = mean(seats >= 160),
      prob_170 = mean(seats >= 170),
      prob_176 = mean(seats >= 176),
      .by = bloque
    )
  assigned <- unique(unlist(definitions, use.names = FALSE))
  list(
    draws = draws,
    summary = summary,
    unassigned = setdiff(unique(national_draws$partido), c(assigned, "OTBL"))
  )
}

mrp_write_blocks_report <- function(analysis, paths) {
  readr::write_csv(analysis$summary, paths$blocks_csv, na = "")
  plot <- ggplot2::ggplot(
    analysis$draws,
    ggplot2::aes(x = seats, fill = bloque)
  ) +
    ggplot2::geom_histogram(binwidth = 1, alpha = 0.55, position = "identity") +
    ggplot2::scale_fill_manual(values = c(
      Derecha = MRP_REPORT_PARTY_COLORS[["PP"]],
      Izquierda = MRP_REPORT_PARTY_COLORS[["PSOE"]]
    )) +
    ggplot2::geom_vline(xintercept = 176, linetype = 2) +
    ggplot2::facet_wrap(ggplot2::vars(bloque), scales = "free_y") +
    ggplot2::labs(
      title = "Distribución de escaños por bloques configurados",
      x = "Escaños", y = "Draws", fill = "Bloque"
    ) +
    mrp_report_theme()
  ggplot2::ggsave(
    paths$blocks_plot, plot,
    width = 9, height = 5.5, dpi = 144
  )
  table <- analysis$summary |>
    dplyr::transmute(
      Bloque = bloque,
      Miembros = miembros,
      `Voto medio` = sprintf("%.2f%%", 100 * voto_medio),
      `Escaños medios` = sprintf("%.2f", escanos_media),
      `P20–P80` = sprintf("%.0f–%.0f", escanos_p20, escanos_p80),
      `P(≥160)` = sprintf("%.1f%%", 100 * prob_160),
      `P(≥170)` = sprintf("%.1f%%", 100 * prob_170),
      `P(≥176)` = sprintf("%.1f%%", 100 * prob_176)
    )
  mrp_write_markdown(
    paths$blocks_report,
    c(
      "# Sumas aritméticas de bloques", "",
      "![Distribución de bloques](distribucion_bloques.png)", "",
      mrp_markdown_table(table), "",
      sprintf(
        "Partidos modelados no asignados a estos bloques: **%s**.",
        ifelse(length(analysis$unassigned),
               paste(analysis$unassigned, collapse = ", "), "ninguno")
      ),
      "",
      "Los bloques proceden de `domain.left_parties` y `domain.right_parties`.",
      "Son sumas aritméticas draw a draw y no presuponen pactos políticos.",
      "",
      sprintf("Datos tabulares: `%s`.", basename(paths$blocks_csv))
    )
  )
}

mrp_geography_analysis <- function(vote) {
  specifications <- dplyr::bind_rows(lapply(vote$geography, function(x) {
    data.frame(
      partido = x$party,
      estrategia = x$strategy,
      soporte_electoral = paste(x$original_support, collapse = ","),
      exclusiones = paste(x$exclusions, collapse = ","),
      soporte_modelado = paste(x$modeled_support, collapse = ","),
      n_ccaa = x$n_ccaa,
      referencia = x$reference_ccaa,
      terminos = paste(x$terms, collapse = " + "),
      stringsAsFactors = FALSE
    )
  }))

  random <- list()
  variances <- list()
  for (party in names(vote$geography)) {
    specification <- vote$geography[[party]]
    if (!identical(specification$strategy, "random")) next
    group <- specification$group_column
    values <- brms::ranef(
      vote$model, groups = group,
      probs = unname(MRP_REPORT_INTERVAL), summary = TRUE
    )[[group]]
    coefficient <- dimnames(values)[[3]][1]
    random[[party]] <- data.frame(
      partido = party,
      ccaa = dimnames(values)[[1]],
      estimacion = values[, "Estimate", coefficient],
      p20 = values[, "Q20", coefficient],
      p80 = values[, "Q80", coefficient],
      stringsAsFactors = FALSE
    )
    vc <- brms::VarCorr(
      vote$model, groups = group,
      probs = unname(MRP_REPORT_INTERVAL), summary = TRUE
    )[[group]]$sd
    variances[[party]] <- data.frame(
      partido = party,
      sd_media = vc[1, "Estimate"],
      sd_p20 = vc[1, "Q20"],
      sd_p80 = vc[1, "Q80"],
      stringsAsFactors = FALSE
    )
  }
  fixed <- as.data.frame(brms::fixef(
    vote$model, probs = unname(MRP_REPORT_INTERVAL), summary = TRUE
  ))
  fixed$termino <- rownames(fixed)
  fixed <- fixed |>
    dplyr::filter(grepl("geo_ccaa", termino)) |>
    dplyr::transmute(
      termino, estimacion = Estimate, p20 = Q20, p80 = Q80
    )
  list(
    specifications = specifications,
    random = dplyr::bind_rows(random),
    variances = dplyr::bind_rows(variances),
    fixed = fixed
  )
}

mrp_write_geography_report <- function(analysis, paths) {
  readr::write_csv(analysis$specifications, paths$geography_csv, na = "")
  if (nrow(analysis$random)) {
    focus <- mrp_report_focus_parties(analysis$random$partido)
    plot_data <- analysis$random |>
      dplyr::filter(partido %in% focus) |>
      dplyr::mutate(
        partido = factor(partido, levels = focus),
        ccaa = factor(ccaa, levels = rev(sort(unique(ccaa))))
      )
    palette <- mrp_report_party_palette(focus)
    plot <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = estimacion, y = ccaa, color = partido)
    ) +
      ggplot2::geom_vline(xintercept = 0, color = "#969696") +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = p20, xmax = p80),
        width = 0.12, orientation = "y"
      ) +
      ggplot2::geom_point(size = 1.4) +
      ggplot2::scale_color_manual(values = palette, guide = "none") +
      ggplot2::facet_wrap(ggplot2::vars(partido), scales = "free_x") +
      ggplot2::labs(
        title = "Efectos autonómicos específicos por partido",
        subtitle = "Partidos nacionales; escala logit e intervalo P20–P80",
        x = "Efecto posterior", y = "CCAA"
      ) +
      mrp_report_theme()
    ggplot2::ggsave(
      paths$geography_plot, plot,
      width = 11, height = 8, dpi = 144
    )
  }
  focus <- mrp_report_focus_parties(analysis$specifications$partido)
  spec_table <- analysis$specifications |>
    dplyr::filter(partido %in% focus) |>
    dplyr::arrange(match(partido, focus)) |>
    dplyr::transmute(
      Partido = gsub("_", " ", partido, fixed = TRUE),
      Estrategia = estrategia,
      `Soporte electoral` = soporte_electoral,
      Exclusiones = exclusiones,
      `Soporte modelado` = soporte_modelado,
      Referencia = referencia,
      Terminos = terminos
    )
  variance_table <- analysis$variances |>
    dplyr::filter(partido %in% focus) |>
    dplyr::arrange(match(partido, focus)) |>
    dplyr::transmute(
      Partido = partido,
      `SD media` = sprintf("%.3f", sd_media),
      `SD P20–P80` = sprintf("%.3f–%.3f", sd_p20, sd_p80)
    )
  fixed_table <- analysis$fixed |>
    dplyr::transmute(
      Termino = termino,
      Estimacion = sprintf("%.3f", estimacion),
      `P20–P80` = sprintf("%.3f–%.3f", p20, p80)
    )
  mrp_write_markdown(
    paths$geography_report,
    c(
      "# Diagnóstico geográfico del voto", "",
      "## Estrategia por partido", "",
      mrp_markdown_table(spec_table), "",
      if (nrow(analysis$random)) "![Efectos CCAA](efectos_ccaa.png)" else "",
      "",
      "## Dispersión de los efectos aleatorios", "",
      if (nrow(variance_table)) mrp_markdown_table(variance_table) else
        "No hay efectos CCAA aleatorios.",
      "",
      "## Contrastes geográficos fijos", "",
      if (nrow(fixed_table)) mrp_markdown_table(fixed_table) else
        "No hay contrastes geográficos fijos.",
      "",
      "Las filas fuera del soporte modelado reciben el nivel de referencia o",
      "indicadores cero. La disponibilidad electoral continúa gobernada por los",
      "offsets de candidaturas.",
      "",
      sprintf("Especificación tabular: `%s`.", basename(paths$geography_csv))
    )
  )
}

mrp_vote_fit_analysis <- function(vote, ndraws = 100L) {
  model <- vote$model
  data <- model$data
  probability <- brms::posterior_epred(
    model, ndraws = ndraws, re_formula = NULL
  )
  probability <- mrp_prepare_epred(probability)
  predicted <- apply(probability, c(2, 3), mean)
  parties <- dimnames(probability)[[3]]
  if (is.null(parties)) parties <- vote$parties
  weights <- data$peso_modelo %||% rep(1, nrow(data))
  observed <- as.character(data$voto)

  aggregate_group <- function(variable) {
    groups <- if (identical(variable, "nacional")) {
      rep("España", nrow(data))
    } else {
      as.character(data[[variable]])
    }
    indices <- split(seq_len(nrow(data)), groups)
    dplyr::bind_rows(lapply(names(indices), function(level) {
      index <- indices[[level]]
      total <- sum(weights[index])
      obs <- vapply(
        parties,
        function(party) sum(weights[index] * (observed[index] == party)) / total,
        numeric(1)
      )
      pred <- colSums(predicted[index, , drop = FALSE] * weights[index]) / total
      data.frame(
        variable = variable,
        nivel = level,
        partido = parties,
        observado = obs,
        predicho = pred,
        residuo_pp = 100 * (pred - obs),
        stringsAsFactors = FALSE
      )
    }))
  }
  variables <- c(
    "nacional",
    intersect(c("periodo", "provincia"), names(data))
  )
  residuals <- dplyr::bind_rows(lapply(variables, aggregate_group))
  summary <- residuals |>
    dplyr::summarise(
      error_abs_medio = mean(abs(residuo_pp)),
      error_abs_maximo = max(abs(residuo_pp)),
      .by = variable
    )
  list(residuals = residuals, summary = summary, ndraws = ndraws)
}

mrp_write_fit_report <- function(analysis, vote, paths) {
  readr::write_csv(analysis$residuals, paths$fit_csv, na = "")
  focus <- mrp_report_focus_parties(analysis$residuals$partido)
  period <- analysis$residuals |>
    dplyr::filter(variable == "periodo", partido %in% focus) |>
    dplyr::mutate(nivel = factor(nivel, levels = unique(nivel)))
  if (nrow(period)) {
    plot <- ggplot2::ggplot(
      period,
      ggplot2::aes(x = nivel, y = partido, fill = residuo_pp)
    ) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient2(
        low = "#2166AC", mid = "white", high = "#B2182B",
        midpoint = 0, name = "Predicho -\nobservado (pp)"
      ) +
      ggplot2::labs(
        title = "Residuos agregados por periodo y partido",
        x = "Periodo", y = "Partido"
      ) +
      mrp_report_theme() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    ggplot2::ggsave(
      paths$fit_plot, plot,
      width = 11, height = 6.5, dpi = 144
    )
  }
  national <- analysis$residuals |>
    dplyr::filter(variable == "nacional", partido %in% focus) |>
    dplyr::arrange(dplyr::desc(abs(residuo_pp))) |>
    dplyr::transmute(
      Partido = partido,
      Observado = sprintf("%.2f%%", 100 * observado),
      Predicho = sprintf("%.2f%%", 100 * predicho),
      `Residuo (pp)` = sprintf("%+.2f", residuo_pp)
    )
  worst <- analysis$residuals |>
    dplyr::filter(variable != "nacional", partido %in% focus) |>
    dplyr::arrange(dplyr::desc(abs(residuo_pp))) |>
    dplyr::slice_head(n = 20) |>
    dplyr::transmute(
      Dimension = variable,
      Nivel = nivel,
      Partido = partido,
      `Residuo (pp)` = sprintf("%+.2f", residuo_pp)
    )
  diagnostic <- vote$diagnostics
  mrp_write_markdown(
    paths$fit_report,
    c(
      "# Ajuste predictivo del modelo de voto", "",
      sprintf("- Divergencias: **%d**.", diagnostic$divergences),
      sprintf("- R-hat máximo: **%.4f**.", diagnostic$rhat_max),
      sprintf("- ESS bulk mínimo: **%.0f**.", diagnostic$ess_bulk_min),
      sprintf("- ESS tail mínimo: **%.0f**.", diagnostic$ess_tail_min),
      sprintf("- Saturaciones de treedepth: **%d**.", diagnostic$treedepth_hits),
      "",
      "## Ajuste nacional in-sample", "",
      mrp_markdown_table(national), "",
      if (nrow(period)) "![Residuos temporales](residuos_periodo_partido.png)" else "",
      "",
      "## Mayores residuos agregados", "",
      mrp_markdown_table(worst), "",
      sprintf(
        "Las probabilidades predichas usan %d draws posteriores. Estos son",
        analysis$ndraws
      ),
      "chequeos in-sample ponderados, no validación fuera de muestra ni efectos",
      "causales.", "",
      sprintf("Datos tabulares: `%s`.", basename(paths$fit_csv))
    )
  )
}

mrp_results_recode_coverage <- function(results_path, schema) {
  results <- mrp_read_data(results_path)
  normalized <- mrp_normalize_electoral_party(results$partido_recode)
  remap <- unlist(schema$results_recode$remap, use.names = TRUE)
  mapped <- unname(remap[normalized])
  data.frame(
    etiqueta_original = as.character(results$partido_recode),
    etiqueta_canonica = normalized,
    categoria = dplyr::coalesce(mapped, schema$results_recode$default),
    usa_default = is.na(mapped),
    votos = results$votos,
    stringsAsFactors = FALSE
  ) |>
    dplyr::summarise(
      votos = sum(votos, na.rm = TRUE),
      .by = c(etiqueta_original, etiqueta_canonica, categoria, usa_default)
    ) |>
    dplyr::arrange(dplyr::desc(votos))
}

mrp_write_execution_report <- function(config, frame, estimate, prediction,
                                       recall_audit, paths) {
  sims <- prediction$sims
  seat_totals <- sims |>
    dplyr::summarise(seats = sum(seats), .by = draw)
  shares <- sims |>
    dplyr::summarise(total = sum(voto_pct), .by = c(draw, provincia))
  diagnostics <- dplyr::bind_rows(
    data.frame(modelo = "participación", frame$diagnostics$turnout),
    data.frame(modelo = "recuerdo", frame$diagnostics$recall),
    data.frame(modelo = "voto", estimate$vote$diagnostics)
  ) |>
    dplyr::transmute(
      Modelo = modelo,
      Algoritmo = algorithm,
      `R-hat máximo` = sprintf("%.4f", rhat_max),
      `ESS bulk mínimo` = sprintf("%.0f", ess_bulk_min),
      `ESS tail mínimo` = sprintf("%.0f", ess_tail_min),
      Divergencias = divergences,
      `Treedepth hits` = treedepth_hits
    )
  coverage <- mrp_results_recode_coverage(
    config$resolved_paths$raw_results, config$schemas$previous
  )
  defaults <- coverage |>
    dplyr::filter(usa_default) |>
    dplyr::slice_head(n = 15) |>
    dplyr::transmute(
      `Etiqueta original` = dplyr::coalesce(etiqueta_original, "<NA>"),
      `Etiqueta canónica` = dplyr::coalesce(etiqueta_canonica, "<NA>"),
      Categoria = categoria,
      Votos = round(votos)
    )
  config_table <- data.frame(
    Parametro = c(
      "Perfil", "Marco", "Estudios recientes", "Unidad temporal",
      "Estructura temporal", "adapt_delta voto", "max_treedepth voto",
      "Cadenas voto", "Objetivo participación", "Peso recuerdo oficial",
      "Calibración nacional", "Intensidad nacional", "Intensidad OTBL"
    ),
    Valor = c(
      config$inference$name, config$run$poststrat_source,
      config$run$recent_studies, config$run$temporal$unit,
      config$run$temporal$structure,
      config$run$vote_sampling$adapt_delta,
      config$run$vote_sampling$max_treedepth,
      config$run$vote_sampling$chains,
      config$run$calibration$turnout,
      config$run$calibration$recall,
      config$run$national_calibration$enabled,
      config$run$national_calibration$strength,
      config$run$national_calibration$otbl_strength
    ),
    stringsAsFactors = FALSE
  )
  input_paths <- unique(unname(unlist(config$resolved_paths[c(
    "raw_vote", "raw_turnout", "raw_previous", "raw_results",
    "raw_candidates", "raw_seats", "raw_degurba"
  )])))
  input_paths <- input_paths[file.exists(input_paths)]
  inputs <- data.frame(
    Fichero = input_paths,
    Modificado = format(
      file.info(input_paths)$mtime, tz = "Europe/Madrid", usetz = TRUE
    ),
    stringsAsFactors = FALSE
  )
  mrp_write_markdown(
    paths$execution_report,
    c(
      "# Panel técnico de la ejecución", "",
      sprintf("- Versión del informe: **%d**.", MRP_VOTE_REPORT_VERSION),
      sprintf("- Firma del marco: `%s`.", frame$signature),
      sprintf("- Firma de estimación: `%s`.", estimate$vote$signature),
      sprintf("- Draws publicados: **%d**.", length(unique(sims$draw))),
      sprintf("- Provincias: **%d**.", length(unique(sims$provincia))),
      sprintf("- Partidos: **%d**.", length(unique(sims$partido))),
      "",
      "## Configuración", "", mrp_markdown_table(config_table), "",
      "## Diagnósticos MCMC", "", mrp_markdown_table(diagnostics), "",
      "## Invariantes", "",
      sprintf(
        "- Escaños por draw: **%d–%d**; objetivo: **%d**.",
        min(seat_totals$seats), max(seat_totals$seats),
        config$domain$expected_seats
      ),
      sprintf(
        "- Error máximo de suma provincial: **%.3g**.",
        max(abs(shares$total - 1))
      ),
      sprintf(
        "- Masa máxima en candidaturas prohibidas: **%.3g**.",
        prediction$availability$maximum
      ),
      sprintf(
        "- Auditoría de recuerdo oficial: **%s**; error máximo **%.3g**.",
        recall_audit$status, recall_audit$maximum
      ),
      sprintf(
        "- Calibración nacional: **%s**.",
        prediction$national_calibration$status
      ),
      "",
      "## Inputs", "", mrp_markdown_table(inputs), "",
      "## Etiquetas electorales enviadas a la categoría por defecto", "",
      mrp_markdown_table(defaults), "",
      "La categoría por defecto agrupa deliberadamente candidaturas menores,",
      "pero esta tabla permite detectar etiquetas canónicas inesperadas antes de",
      "que contaminen OTBL."
    )
  )
}

mrp_write_vote_analysis <- function(config, frame, estimate, prediction) {
  paths <- mrp_vote_report_paths(config$resolved_paths)
  dir.create(paths$root, recursive = TRUE, showWarnings = FALSE)

  national <- mrp_national_uncertainty(
    prediction$sims, config$resolved_paths$raw_results
  )
  calibration <- mrp_calibration_analysis(prediction$national_calibration)
  recall <- mrp_recall_analysis(
    prediction$recall, prediction$national_calibration
  )
  recall_audit <- frame$diagnostics$recall_calibration %||%
    mrp_recall_target_audit(
      frame, config$resolved_paths$raw_results, config$schemas$previous
    )
  temporal <- mrp_temporal_index(
    estimate$vote,
    prediction$precalibrated_national,
    length(unique(prediction$sims$draw))
  )
  demographic <- mrp_demographic_analysis(
    prediction$demographics,
    prediction$precalibrated_national,
    estimate$vote,
    length(unique(prediction$sims$draw))
  )
  seats <- mrp_last_seat_analysis(
    prediction$sims, config$domain$district_threshold
  )
  blocks <- mrp_blocks_analysis(national$draws, config$domain)
  geography <- mrp_geography_analysis(estimate$vote)
  fit <- mrp_vote_fit_analysis(estimate$vote)

  mrp_write_estimation_report(national, paths)
  mrp_write_calibration_report(
    calibration, prediction$national_calibration, config, paths
  )
  mrp_write_recall_analysis(
    recall, recall_audit, prediction$recall$weight,
    config$resolved_paths$recall_report, paths
  )
  mrp_write_temporal_report(temporal, estimate$vote$temporal, paths)
  mrp_write_demographic_report(
    demographic, estimate$vote$temporal, paths
  )
  mrp_write_last_seat_report(seats, paths)
  mrp_write_blocks_report(blocks, paths)
  mrp_write_geography_report(geography, paths)
  mrp_write_fit_report(fit, estimate$vote, paths)
  mrp_write_execution_report(
    config, frame, estimate, prediction, recall_audit, paths
  )

  data_files <- unname(unlist(paths[c(
    "national_csv", "calibration_csv", "temporal_csv",
    "demographic_strength_csv", "demographic_change_csv",
    "demographic_evolution_csv", "seat_csv",
    "seat_probability_csv", "seat_battle_csv", "seat_battle_summary_csv",
    "blocks_csv", "geography_csv", "fit_csv"
  )]))
  figure_files <- unname(unlist(paths[c(
    "national_vote_plot", "national_seat_plot", "calibration_plot",
    "recall_source_plot", "recall_destination_plot", "temporal_plot",
    "demographic_strength_plot", "demographic_change_plot",
    "demographic_evolution_plot",
    "seat_plot", "seat_battle_party_plot", "seat_battle_province_plot",
    "blocks_plot",
    "geography_plot", "fit_plot"
  )]))

  list(
    estimacion = paths$estimation_report,
    calibracion_nacional = paths$calibration_report,
    recuerdo = config$resolved_paths$recall_report,
    temporal = paths$temporal_report,
    grupos_sociodemograficos = paths$demographic_report,
    ultimo_escano = paths$seat_report,
    bloques = paths$blocks_report,
    geografia = paths$geography_report,
    ajuste = paths$fit_report,
    ejecucion = paths$execution_report,
    data = data_files[file.exists(data_files)],
    figures = figure_files[file.exists(figure_files)]
  )
}
