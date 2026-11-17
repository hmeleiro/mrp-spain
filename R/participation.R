MRP_TURNOUT_ANALYSIS_VERSION <- 3L

mrp_turnout_analysis_paths <- function(paths) {
  root <- file.path(paths$diagnostics_root, "participacion")
  list(
    root = root,
    report = file.path(root, "informe_participacion.md"),
    summary = file.path(root, "participacion_resumen.csv"),
    correlations = file.path(root, "participacion_correlaciones.csv"),
    sensitivity = file.path(root, "participacion_sensibilidad.csv"),
    draws = file.path(root, "participacion_draws.rds"),
    recall_plot = file.path(root, "participacion_recuerdo.png"),
    demographics_plot = file.path(root, "participacion_demografia.png"),
    provinces_plot = file.path(root, "participacion_provincia.png"),
    predictors_plot = file.path(root, "participacion_predictores.png"),
    calibration_plot = file.path(root, "participacion_calibracion.png")
  )
}

mrp_turnout_ccaa_labels <- function() {
  c(
    `01` = "Andalucía", `02` = "Aragón", `03` = "Asturias",
    `04` = "Illes Balears", `05` = "Canarias", `06` = "Cantabria",
    `07` = "Castilla y León", `08` = "Castilla-La Mancha",
    `09` = "Cataluña", `10` = "Comunitat Valenciana",
    `11` = "Extremadura", `12` = "Galicia", `13` = "Madrid",
    `14` = "Murcia", `15` = "Navarra", `16` = "País Vasco",
    `17` = "La Rioja", `18` = "Ceuta", `19` = "Melilla"
  )
}

mrp_turnout_display_summary <- function(summary, config) {
  results <- mrp_read_data(config$resolved_paths$raw_results) |>
    dplyr::transmute(
      codigo = mrp_normalize_province(codigo_provincia),
      nombre = as.character(territorio_nombre)
    ) |>
    dplyr::distinct()
  province_labels <- stats::setNames(results$nombre, results$codigo)
  ccaa_labels <- mrp_turnout_ccaa_labels()
  summary |>
    dplyr::mutate(
      nivel = dplyr::case_when(
        variable == "ccaa" & nivel %in% names(ccaa_labels) ~
          paste0(nivel, " — ", unname(ccaa_labels[nivel])),
        variable == "provincia" & nivel %in% names(province_labels) ~
          paste0(nivel, " — ", unname(province_labels[nivel])),
        TRUE ~ as.character(nivel)
      )
    )
}

mrp_turnout_calibration_contributions <- function(draws) {
  national <- draws |>
    dplyr::filter(
      variable == "nacional", escenario == "final", etapa == "raw"
    ) |>
    dplyr::select(draw, poblacion_nacional = poblacion)
  draws |>
    dplyr::filter(variable == "recuerdo", escenario == "final") |>
    dplyr::select(draw, nivel, etapa, votantes_esperados) |>
    tidyr::pivot_wider(names_from = etapa, values_from = votantes_esperados) |>
    dplyr::left_join(national, by = "draw") |>
    dplyr::mutate(
      contribucion_pp = 100 * (calibrated - raw) / poblacion_nacional
    ) |>
    dplyr::summarise(
      contribucion_media = mean(contribucion_pp),
      contribucion_p20 = mrp_turnout_quantile(
        contribucion_pp, MRP_REPORT_INTERVAL[["lower"]]
      ),
      contribucion_p80 = mrp_turnout_quantile(
        contribucion_pp, MRP_REPORT_INTERVAL[["upper"]]
      ),
      .by = nivel
    ) |>
    dplyr::arrange(contribucion_media)
}

mrp_turnout_sensitivity <- function(frame_result, targets) {
  frame <- frame_result$frame
  population <- matrix(colMeans(frame$n_imputado), nrow = 1)
  probability <- matrix(colMeans(frame$p_vota), nrow = 1)
  base <- frame$base
  groups <- split(seq_len(nrow(base)), as.character(base$recuerdo))
  dplyr::bind_rows(lapply(targets, function(target) {
    calibrated <- mrp_calibrate_turnout_logit(
      probability, population, target
    )$probability[1, ]
    dplyr::bind_rows(lapply(names(groups), function(level) {
      index <- groups[[level]]
      data.frame(
        objetivo = target,
        recuerdo = level,
        participacion = sum(population[1, index] * calibrated[index]) /
          sum(population[1, index]),
        stringsAsFactors = FALSE
      )
    }))
  }))
}

mrp_validate_turnout_analysis_frame <- function(frame_result) {
  if (!is.list(frame_result) || !is.list(frame_result$frame)) {
    stop("Turnout analysis requires a cached poststrat frame result.", call. = FALSE)
  }
  frame <- frame_result$frame
  required <- c(
    "base", "n_imputado", "p_vota", "n_votantes_esperados",
    "scenarios", "recall_weight"
  )
  missing <- setdiff(required, names(frame))
  if (length(missing)) {
    stop(
      "Poststrat frame is missing turnout component(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.data.frame(frame$base) || !nrow(frame$base)) {
    stop("Poststrat turnout base must be a non-empty data frame.", call. = FALSE)
  }
  if (!all(c("model", "official") %in% names(frame$scenarios))) {
    stop("Turnout analysis requires model and official recall scenarios.",
         call. = FALSE)
  }

  inputs <- list(
    final = list(
      population = frame$n_imputado,
      probability = frame$p_vota,
      voters = frame$n_votantes_esperados
    )
  )
  for (scenario in c("model", "official")) {
    current <- frame$scenarios[[scenario]]
    scenario_required <- c(
      "n_imputado", "p_vota", "p_vota_cal", "n_votantes_esperados"
    )
    scenario_missing <- setdiff(scenario_required, names(current))
    if (length(scenario_missing)) {
      stop(
        "Turnout scenario ", scenario, " is missing component(s): ",
        paste(scenario_missing, collapse = ", "),
        call. = FALSE
      )
    }
    inputs[[scenario]] <- list(
      population = current$n_imputado,
      probability = current$p_vota,
      calibrated_probability = current$p_vota_cal,
      voters = current$n_votantes_esperados
    )
  }
  inputs <- inputs[c("model", "official", "final")]

  expected_dim <- NULL
  for (scenario in names(inputs)) {
    for (component in names(inputs[[scenario]])) {
      value <- inputs[[scenario]][[component]]
      if (!is.matrix(value) || !is.numeric(value)) {
        stop(
          "Turnout ", scenario, " ", component, " must be a numeric matrix.",
          call. = FALSE
        )
      }
      if (is.null(expected_dim)) expected_dim <- dim(value)
      if (!identical(dim(value), expected_dim)) {
        stop("Turnout analysis matrices must have identical dimensions.",
             call. = FALSE)
      }
      if (any(!is.finite(value))) {
        stop("Turnout analysis matrices must contain only finite values.",
             call. = FALSE)
      }
    }
    if (any(inputs[[scenario]]$population < 0) ||
        any(inputs[[scenario]]$voters < 0)) {
      stop("Turnout populations and expected voters must be non-negative.",
           call. = FALSE)
    }
    if (any(inputs[[scenario]]$probability < 0 |
            inputs[[scenario]]$probability > 1)) {
      stop("Raw turnout probabilities must stay in [0, 1].", call. = FALSE)
    }
    if (!is.null(inputs[[scenario]]$calibrated_probability) &&
        any(inputs[[scenario]]$calibrated_probability < 0 |
            inputs[[scenario]]$calibrated_probability > 1)) {
      stop("Calibrated turnout probabilities must stay in [0, 1].",
           call. = FALSE)
    }
  }
  if (expected_dim[[2]] != nrow(frame$base)) {
    stop("Turnout matrix columns must match poststrat base rows.",
         call. = FALSE)
  }
  if (any(vapply(inputs, function(x) {
    any(rowSums(x$population) <= 0)
  }, logical(1)))) {
    stop("Every turnout draw and scenario must have positive population.",
         call. = FALSE)
  }

  grouping <- intersect(
    c(
      "recuerdo", "sexo", "edad_grupo", "estudios", "sitlab2",
      "ccaa", "provincia", "tamuni"
    ),
    names(frame$base)
  )
  if (anyNA(frame$base[grouping])) {
    stop("Turnout grouping variables must not contain missing values.",
         call. = FALSE)
  }
  list(
    frame = frame,
    inputs = inputs,
    grouping = grouping,
    ndraws = expected_dim[[1]],
    ncells = expected_dim[[2]]
  )
}

mrp_turnout_group_indices <- function(base, variables) {
  groups <- list(nacional = list("España" = seq_len(nrow(base))))
  for (variable in variables) {
    values <- as.character(base[[variable]])
    level_order <- if (is.factor(base[[variable]])) {
      levels(base[[variable]])[levels(base[[variable]]) %in% unique(values)]
    } else {
      unique(values)
    }
    groups[[variable]] <- split(
      seq_along(values),
      factor(values, levels = level_order),
      drop = TRUE
    )
  }
  groups
}

mrp_turnout_group_sums <- function(population, probability, voters, indices,
                                   chunk_size = 5000L) {
  population_total <- numeric(nrow(population))
  raw_voters <- numeric(nrow(population))
  calibrated_voters <- numeric(nrow(population))
  starts <- seq.int(1L, length(indices), by = chunk_size)
  for (start in starts) {
    chunk <- indices[start:min(start + chunk_size - 1L, length(indices))]
    population_chunk <- population[, chunk, drop = FALSE]
    population_total <- population_total + rowSums(population_chunk)
    raw_voters <- raw_voters + rowSums(
      population_chunk * probability[, chunk, drop = FALSE]
    )
    calibrated_voters <- calibrated_voters +
      rowSums(voters[, chunk, drop = FALSE])
  }
  list(
    population = population_total,
    raw = raw_voters,
    calibrated = calibrated_voters
  )
}

mrp_turnout_draw_aggregates <- function(frame_result) {
  checked <- mrp_validate_turnout_analysis_frame(frame_result)
  groups <- mrp_turnout_group_indices(
    checked$frame$base, checked$grouping
  )
  output <- vector("list", length(checked$inputs) * sum(lengths(groups)) * 2L)
  output_index <- 0L

  for (scenario in names(checked$inputs)) {
    current <- checked$inputs[[scenario]]
    for (variable in names(groups)) {
      for (level in names(groups[[variable]])) {
        totals <- mrp_turnout_group_sums(
          current$population,
          current$probability,
          current$voters,
          groups[[variable]][[level]]
        )
        for (stage in c("raw", "calibrated")) {
          expected_voters <- totals[[stage]]
          output_index <- output_index + 1L
          output[[output_index]] <- data.frame(
            draw = seq_len(checked$ndraws),
            variable = variable,
            nivel = level,
            escenario = scenario,
            etapa = stage,
            poblacion = totals$population,
            votantes_esperados = expected_voters,
            participacion = ifelse(
              totals$population > 0,
              expected_voters / totals$population,
              NA_real_
            ),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  draws <- dplyr::bind_rows(output[seq_len(output_index)])
  national <- draws |>
    dplyr::filter(variable == "nacional") |>
    dplyr::select(
      draw, escenario, etapa,
      poblacion_total = poblacion,
      votantes_total = votantes_esperados
    )
  draws |>
    dplyr::left_join(national, by = c("draw", "escenario", "etapa")) |>
    dplyr::mutate(
      peso_poblacion = poblacion / poblacion_total,
      peso_votantes = votantes_esperados / votantes_total
    ) |>
    dplyr::select(
      draw, variable, nivel, escenario, etapa,
      poblacion, votantes_esperados, participacion,
      peso_poblacion, peso_votantes
    )
}

mrp_turnout_quantile <- function(x, probability) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  unname(stats::quantile(x, probability))
}

mrp_turnout_summary <- function(draws) {
  summary <- draws |>
    dplyr::summarise(
      participacion_media = mean(participacion, na.rm = TRUE),
      participacion_p20 = mrp_turnout_quantile(
        participacion, MRP_REPORT_INTERVAL[["lower"]]
      ),
      participacion_p50 = mrp_turnout_quantile(participacion, 0.50),
      participacion_p80 = mrp_turnout_quantile(
        participacion, MRP_REPORT_INTERVAL[["upper"]]
      ),
      peso_poblacion_media = mean(peso_poblacion, na.rm = TRUE),
      peso_votantes_media = mean(peso_votantes, na.rm = TRUE),
      .by = c(variable, nivel, escenario, etapa)
    )
  raw <- summary |>
    dplyr::filter(etapa == "raw") |>
    dplyr::select(
      variable, nivel, escenario,
      participacion_bruta_media = participacion_media
    )
  summary |>
    dplyr::left_join(raw, by = c("variable", "nivel", "escenario")) |>
    dplyr::mutate(
      delta_calibracion_pp = 100 * (
        participacion_media - participacion_bruta_media
      )
    ) |>
    dplyr::select(-participacion_bruta_media)
}

mrp_turnout_safe_cor <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  if (sum(keep) < 3L ||
      stats::sd(x[keep]) == 0 ||
      stats::sd(y[keep]) == 0) {
    return(NA_real_)
  }
  stats::cor(x[keep], y[keep], method = "pearson")
}

mrp_turnout_correlations <- function(draws, base) {
  predictors <- intersect(
    c(
      "participacion_prev", "margen_der_izq_prev", "pct_alquiler",
      "pct_mayores65", "degurba1", "degurba2", "degurba3"
    ),
    names(base)
  )
  if (!length(predictors) || !"provincia" %in% names(base)) {
    return(data.frame(
      draw = integer(), escenario = character(), etapa = character(),
      predictor = character(), correlacion = numeric()
    ))
  }
  for (predictor in predictors) {
    values_per_province <- tapply(
      base[[predictor]], as.character(base$provincia),
      function(x) length(unique(x[is.finite(x)]))
    )
    if (any(values_per_province > 1L, na.rm = TRUE)) {
      stop(
        "Provincial predictor ", predictor,
        " is not constant within province.",
        call. = FALSE
      )
    }
  }
  provincial <- base |>
    dplyr::transmute(
      provincia = as.character(provincia),
      dplyr::across(dplyr::all_of(predictors))
    ) |>
    dplyr::distinct()
  province_draws <- draws |>
    dplyr::filter(variable == "provincia") |>
    dplyr::transmute(
      draw, escenario, etapa,
      provincia = as.character(nivel),
      participacion
    ) |>
    dplyr::left_join(provincial, by = "provincia")
  correlation_wide <- province_draws |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(predictors),
        ~ mrp_turnout_safe_cor(.x, participacion)
      ),
      .by = c(draw, escenario, etapa)
    )
  tidyr::pivot_longer(
    correlation_wide,
    dplyr::all_of(predictors),
    names_to = "predictor",
    values_to = "correlacion"
  )
}

mrp_turnout_correlation_summary <- function(correlations) {
  if (!nrow(correlations)) {
    return(data.frame(
      predictor = character(), escenario = character(), etapa = character(),
      correlacion_media = numeric(), correlacion_p20 = numeric(),
      correlacion_p50 = numeric(), correlacion_p80 = numeric()
    ))
  }
  correlations |>
    dplyr::summarise(
      correlacion_media = if (all(is.na(correlacion))) {
        NA_real_
      } else {
        mean(correlacion, na.rm = TRUE)
      },
      correlacion_p20 = mrp_turnout_quantile(
        correlacion, MRP_REPORT_INTERVAL[["lower"]]
      ),
      correlacion_p50 = mrp_turnout_quantile(correlacion, 0.50),
      correlacion_p80 = mrp_turnout_quantile(
        correlacion, MRP_REPORT_INTERVAL[["upper"]]
      ),
      .by = c(predictor, escenario, etapa)
    )
}

mrp_build_turnout_analysis <- function(frame_result) {
  draws <- mrp_turnout_draw_aggregates(frame_result)
  correlations <- mrp_turnout_correlations(draws, frame_result$frame$base)
  list(
    draws = draws,
    summary = mrp_turnout_summary(draws),
    correlations = correlations,
    correlation_summary = mrp_turnout_correlation_summary(correlations)
  )
}

mrp_assert_turnout_analysis_target <- function(analysis, target,
                                               tolerance = 1e-8) {
  national <- analysis$draws |>
    dplyr::filter(
      variable == "nacional",
      escenario == "final",
      etapa == "calibrated"
    )
  if (!nrow(national) ||
      any(abs(national$participacion - target) > tolerance)) {
    stop(
      "Final calibrated turnout draws do not match the configured target.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

mrp_turnout_variable_labels <- function(variable) {
  labels <- c(
    nacional = "Nacional",
    recuerdo = "Recuerdo de voto",
    sexo = "Sexo",
    edad_grupo = "Edad",
    estudios = "Estudios",
    sitlab2 = "Situación laboral",
    tamuni = "Tamaño de municipio",
    ccaa = "Comunidad autónoma",
    provincia = "Provincia"
  )
  output <- unname(labels[variable])
  ifelse(is.na(output), variable, output)
}

mrp_turnout_scenario_labels <- function(scenario) {
  labels <- c(
    model = "Recuerdo modelado",
    official = "Recuerdo oficial",
    final = "Mezcla final"
  )
  output <- unname(labels[scenario])
  ifelse(is.na(output), scenario, output)
}

mrp_turnout_stage_labels <- function(stage) {
  ifelse(stage == "raw", "Bruta", "Calibrada")
}

mrp_turnout_markdown_value <- function(x) {
  gsub("|", "\\|", as.character(x), fixed = TRUE)
}

mrp_turnout_write_plots <- function(analysis, paths) {
  summary <- analysis$summary
  interval <- ggplot2::aes(
    xmin = 100 * participacion_p20,
    xmax = 100 * participacion_p80
  )
  common_theme <- ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )

  calibration <- summary |>
    dplyr::filter(variable == "nacional") |>
    dplyr::mutate(
      escenario = mrp_turnout_scenario_labels(escenario),
      etapa = mrp_turnout_stage_labels(etapa)
    )
  calibration_plot <- ggplot2::ggplot(
    calibration,
    ggplot2::aes(
      x = etapa, y = 100 * participacion_media,
      group = escenario, color = escenario
    )
  ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = 100 * participacion_p20,
        ymax = 100 * participacion_p80
      ),
      width = 0.08
    ) +
    ggplot2::labs(
      title = "Efecto de la calibración nacional de participación",
      subtitle = "Media e intervalo P20–P80",
      x = NULL, y = "Participación (%)", color = "Escenario"
    ) +
    common_theme
  ggplot2::ggsave(
    paths$calibration_plot, calibration_plot,
    width = 8, height = 4.8, dpi = 144
  )

  recall <- summary |>
    dplyr::filter(variable == "recuerdo", escenario == "final") |>
    dplyr::mutate(etapa = mrp_turnout_stage_labels(etapa))
  recall$nivel <- factor(recall$nivel, levels = rev(unique(recall$nivel)))
  recall_plot <- ggplot2::ggplot(
    recall,
    ggplot2::aes(
      x = 100 * participacion_media,
      y = nivel,
      color = etapa
    )
  ) +
    ggplot2::geom_errorbar(
      interval, width = 0.15, orientation = "y", position = "identity"
    ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = "Participación por recuerdo de voto",
      subtitle = "Mezcla final; media e intervalo P20–P80",
      x = "Participación estimada (%)", y = NULL, color = "Etapa"
    ) +
    common_theme
  ggplot2::ggsave(
    paths$recall_plot, recall_plot,
    width = 8, height = 5.2, dpi = 144
  )

  demographic_variables <- intersect(
    c("sexo", "edad_grupo", "estudios", "sitlab2", "tamuni"),
    unique(summary$variable)
  )
  demographics <- summary |>
    dplyr::filter(
      variable %in% demographic_variables,
      escenario == "final",
      etapa == "calibrated"
    ) |>
    dplyr::mutate(
      dimension = mrp_turnout_variable_labels(variable),
      nivel = as.character(nivel)
    )
  demographics_plot <- ggplot2::ggplot(
    demographics,
    ggplot2::aes(x = 100 * participacion_media, y = nivel)
  ) +
    ggplot2::geom_errorbar(
      interval, width = 0.15, orientation = "y", color = "#5B8DB8"
    ) +
    ggplot2::geom_point(color = "#1F4E79", size = 1.8) +
    ggplot2::facet_wrap(
      ggplot2::vars(dimension), scales = "free_y", ncol = 2
    ) +
    ggplot2::labs(
      title = "Participación por variables demográficas",
      subtitle = "Mezcla final calibrada; media e intervalo P20–P80",
      x = "Participación estimada (%)", y = NULL
    ) +
    common_theme
  ggplot2::ggsave(
    paths$demographics_plot, demographics_plot,
    width = 9, height = max(6, 1.2 * length(demographic_variables)),
    dpi = 144
  )

  provinces <- summary |>
    dplyr::filter(
      variable == "provincia",
      escenario == "final",
      etapa == "calibrated"
    ) |>
    dplyr::arrange(participacion_media)
  provinces$nivel <- factor(provinces$nivel, levels = provinces$nivel)
  provinces_plot <- ggplot2::ggplot(
    provinces,
    ggplot2::aes(x = 100 * participacion_media, y = nivel)
  ) +
    ggplot2::geom_errorbar(
      interval, width = 0.1, orientation = "y", color = "#9ECAE1"
    ) +
    ggplot2::geom_point(color = "#08519C", size = 1.2) +
    ggplot2::labs(
      title = "Participación estimada por provincia",
      subtitle = "Mezcla final calibrada; media e intervalo P20–P80",
      x = "Participación estimada (%)", y = "Código de provincia"
    ) +
    common_theme
  ggplot2::ggsave(
    paths$provinces_plot, provinces_plot,
    width = 8, height = 10, dpi = 144
  )

  predictors <- analysis$correlation_summary |>
    dplyr::filter(
      escenario == "final",
      etapa == "calibrated",
      is.finite(correlacion_media)
    ) |>
    dplyr::arrange(correlacion_media)
  predictors$predictor <- factor(
    predictors$predictor, levels = predictors$predictor
  )
  predictors_plot <- ggplot2::ggplot(
    predictors,
    ggplot2::aes(x = correlacion_media, y = predictor)
  ) +
    ggplot2::geom_vline(xintercept = 0, color = "#969696") +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        xmin = correlacion_p20,
        xmax = correlacion_p80
      ),
      width = 0.15, orientation = "y", color = "#74A9CF"
    ) +
    ggplot2::geom_point(color = "#045A8D", size = 2) +
    ggplot2::labs(
      title = "Correlaciones provinciales descriptivas",
      subtitle = "Mezcla final calibrada; media e intervalo P20–P80",
      x = "Correlación de Pearson", y = NULL
    ) +
    ggplot2::coord_cartesian(xlim = c(-1, 1)) +
    common_theme
  ggplot2::ggsave(
    paths$predictors_plot, predictors_plot,
    width = 8, height = 4.8, dpi = 144
  )
  invisible(paths)
}

mrp_turnout_report_lines <- function(analysis, metadata, paths) {
  summary <- analysis$summary
  national <- summary |>
    dplyr::filter(variable == "nacional") |>
    dplyr::arrange(
      match(escenario, c("model", "official", "final")),
      match(etapa, c("raw", "calibrated"))
    )
  national_rows <- sprintf(
    "| %s | %s | %.2f | %.2f–%.2f | %+.2f |",
    mrp_turnout_scenario_labels(national$escenario),
    mrp_turnout_stage_labels(national$etapa),
    100 * national$participacion_media,
    100 * national$participacion_p20,
    100 * national$participacion_p80,
    national$delta_calibracion_pp
  )

  recall <- summary |>
    dplyr::filter(variable == "recuerdo", escenario == "final") |>
    dplyr::select(
      nivel, etapa, participacion_media,
      participacion_p20, participacion_p80, peso_votantes_media
    ) |>
    tidyr::pivot_wider(
      names_from = etapa,
      values_from = c(
        participacion_media, participacion_p20,
        participacion_p80, peso_votantes_media
      )
    ) |>
    dplyr::arrange(dplyr::desc(participacion_media_calibrated))
  recall_rows <- sprintf(
    "| %s | %.2f | %.2f | %.2f–%.2f | %.2f |",
    mrp_turnout_markdown_value(recall$nivel),
    100 * recall$participacion_media_raw,
    100 * recall$participacion_media_calibrated,
    100 * recall$participacion_p20_calibrated,
    100 * recall$participacion_p80_calibrated,
    100 * recall$peso_votantes_media_calibrated
  )

  demographic_variables <- intersect(
    c("sexo", "edad_grupo", "estudios", "sitlab2", "tamuni"),
    unique(summary$variable)
  )
  demographic_sections <- unlist(lapply(demographic_variables, function(variable) {
    current <- summary |>
      dplyr::filter(
        variable == !!variable,
        escenario == "final",
        etapa == "calibrated"
      )
    rows <- sprintf(
      "| %s | %.2f | %.2f–%.2f | %.2f |",
      mrp_turnout_markdown_value(current$nivel),
      100 * current$participacion_media,
      100 * current$participacion_p20,
      100 * current$participacion_p80,
      100 * current$peso_votantes_media
    )
    c(
      paste("###", mrp_turnout_variable_labels(variable)),
      "",
      "| Nivel | Participación (%) | P20–P80 | Peso entre votantes (%) |",
      "|---|---:|---:|---:|",
      rows,
      ""
    )
  }))

  ccaa <- summary |>
    dplyr::filter(
      variable == "ccaa",
      escenario == "final",
      etapa == "calibrated"
    ) |>
    dplyr::arrange(dplyr::desc(participacion_media))
  ccaa_rows <- sprintf(
    "| %s | %.2f | %.2f–%.2f |",
    mrp_turnout_markdown_value(ccaa$nivel),
    100 * ccaa$participacion_media,
    100 * ccaa$participacion_p20,
    100 * ccaa$participacion_p80
  )

  provinces <- summary |>
    dplyr::filter(
      variable == "provincia",
      escenario == "final",
      etapa == "calibrated"
    ) |>
    dplyr::arrange(participacion_media)
  province_extremes <- dplyr::bind_rows(
    utils::head(provinces, 10),
    utils::tail(provinces, 10)
  ) |>
    dplyr::distinct(nivel, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(participacion_media))
  province_rows <- sprintf(
    "| %s | %.2f | %.2f–%.2f |",
    mrp_turnout_markdown_value(province_extremes$nivel),
    100 * province_extremes$participacion_media,
    100 * province_extremes$participacion_p20,
    100 * province_extremes$participacion_p80
  )

  correlations <- analysis$correlation_summary |>
    dplyr::filter(escenario == "final", etapa == "calibrated") |>
    dplyr::arrange(dplyr::desc(abs(correlacion_media)))
  correlation_rows <- sprintf(
    "| %s | %.3f | %.3f–%.3f |",
    mrp_turnout_markdown_value(correlations$predictor),
    correlations$correlacion_media,
    correlations$correlacion_p20,
    correlations$correlacion_p80
  )

  contributions <- analysis$calibration_contributions
  contribution_rows <- sprintf(
    "| %s | %+.2f | %+.2f–%+.2f |",
    mrp_turnout_markdown_value(contributions$nivel),
    contributions$contribucion_media,
    contributions$contribucion_p20,
    contributions$contribucion_p80
  )
  sensitivity <- analysis$sensitivity |>
    dplyr::arrange(objetivo, recuerdo)
  sensitivity_rows <- sprintf(
    "| %.0f%% | %s | %.2f%% |",
    100 * sensitivity$objetivo,
    mrp_turnout_markdown_value(sensitivity$recuerdo),
    100 * sensitivity$participacion
  )

  final_raw <- national |>
    dplyr::filter(escenario == "final", etapa == "raw") |>
    dplyr::pull(participacion_media)
  final_calibrated <- national |>
    dplyr::filter(escenario == "final", etapa == "calibrated") |>
    dplyr::pull(participacion_media)
  recall_high <- recall[which.max(recall$participacion_media_calibrated), ]
  recall_low <- recall[which.min(recall$participacion_media_calibrated), ]
  province_high <- provinces[which.max(provinces$participacion_media), ]
  province_low <- provinces[which.min(provinces$participacion_media), ]
  inference_note <- if (identical(metadata$inference, "fast")) {
    paste(
      "La ejecución usa inferencia variacional `meanfield`; los intervalos",
      "son provisionales y pueden subestimar la incertidumbre."
    )
  } else {
    "La ejecución usa el perfil completo de inferencia MCMC."
  }

  c(
    "# Diagnóstico de participación postestratificada",
    "",
    sprintf(
      "Marco: **%s**; perfil de inferencia: **%s**; draws: **%d**.",
      metadata$poststrat_source, metadata$inference, metadata$ndraws
    ),
    sprintf(
      "Objetivo nacional de participación: **%.2f%%**; peso del recuerdo oficial: **%.2f**.",
      100 * metadata$turnout_target, metadata$recall_weight
    ),
    "",
    "## Lectura principal",
    "",
    sprintf(
      "- La propensión bruta nacional es %.2f%% y la calibración la lleva a %.2f%%.",
      100 * final_raw, 100 * final_calibrated
    ),
    sprintf(
      "- Por recuerdo, la participación final más alta corresponde a **%s** (%.2f%%) y la más baja a **%s** (%.2f%%), una brecha de %.2f puntos.",
      recall_high$nivel, 100 * recall_high$participacion_media_calibrated,
      recall_low$nivel, 100 * recall_low$participacion_media_calibrated,
      100 * (
        recall_high$participacion_media_calibrated -
          recall_low$participacion_media_calibrated
      )
    ),
    sprintf(
      "- El rango provincial va de **%s** (%.2f%%) a **%s** (%.2f%%).",
      province_low$nivel, 100 * province_low$participacion_media,
      province_high$nivel, 100 * province_high$participacion_media
    ),
    "",
    "## Calibración nacional y escenarios de recuerdo",
    "",
    "![Calibración nacional](participacion_calibracion.png)",
    "",
    "| Escenario | Etapa | Participación (%) | P20–P80 | Cambio frente a bruta (pp) |",
    "|---|---|---:|---:|---:|",
    national_rows,
    "",
    "La etapa bruta agrega la propensión producida por el modelo. La etapa calibrada",
    "aplica el desplazamiento logit nacional. `model` usa el recuerdo imputado,",
    "`official` el recuerdo ajustado a resultados anteriores y `final` su mezcla.",
    "",
    "## Participación por recuerdo de voto",
    "",
    "![Participación por recuerdo](participacion_recuerdo.png)",
    "",
    "| Recuerdo | Bruta (%) | Calibrada (%) | P20–P80 calibrada | Peso entre votantes (%) |",
    "|---|---:|---:|---:|---:|",
    recall_rows,
    "",
    "### Contribución de cada recuerdo al ajuste nacional",
    "",
    "| Recuerdo | Contribución media (pp) | P20–P80 |",
    "|---|---:|---:|",
    contribution_rows,
    "",
    "Las contribuciones suman el cambio nacional entre la propensión bruta y",
    "la calibrada. Un valor negativo indica cuántos puntos nacionales absorbe",
    "cada grupo tras aplicar el desplazamiento logit común.",
    "",
    "### Sensibilidad a objetivos alternativos",
    "",
    "| Objetivo nacional | Recuerdo | Participación estandarizada |",
    "|---:|---|---:|",
    sensitivity_rows,
    "",
    "La sensibilidad utiliza la población y propensiones medias. Sirve para",
    "comparar escenarios; no incorpora intervalos posteriores adicionales.",
    "",
    "## Variables demográficas",
    "",
    "![Participación por variables demográficas](participacion_demografia.png)",
    "",
    demographic_sections,
    "## Geografía",
    "",
    "![Participación provincial](participacion_provincia.png)",
    "",
    "### Comunidades autónomas",
    "",
    "| CCAA | Participación (%) | P20–P80 |",
    "|---|---:|---:|",
    ccaa_rows,
    "",
    "### Provincias con valores extremos",
    "",
    "| Provincia | Participación (%) | P20–P80 |",
    "|---|---:|---:|",
    province_rows,
    "",
    "El CSV contiene las 52 provincias; la tabla muestra las diez estimaciones",
    "superiores y las diez inferiores.",
    "",
    "## Predictores provinciales",
    "",
    "![Correlaciones provinciales](participacion_predictores.png)",
    "",
    "| Predictor | Correlación media | P20–P80 |",
    "|---|---:|---:|",
    correlation_rows,
    "",
    "Las correlaciones son descriptivas entre provincias y no identifican efectos",
    "causales ni coeficientes aislados del modelo.",
    "",
    "## Metodología y cautelas",
    "",
    "- Las tasas son cocientes de votantes esperados y población, agregados por draw.",
    "- Los intervalos son los percentiles 20 y 80 de los agregados posteriores.",
    "- Cada desglose es marginal: mezcla la composición restante del marco.",
    "- La calibración fija el total nacional, pero conserva heterogeneidad entre grupos.",
    "- La propensión bruta procede de la escala declarada y no es una tasa electoral sin calibrar.",
    paste0("- ", inference_note),
    "",
    sprintf(
      "Datos tabulares: `%s`, `%s`, `%s` y `%s`.",
      basename(paths$summary), basename(paths$correlations),
      basename(paths$sensitivity), basename(paths$draws)
    )
  )
}

mrp_write_turnout_analysis <- function(frame_result, config,
                                       frame_signature = NULL) {
  paths <- mrp_turnout_analysis_paths(config$resolved_paths)
  dir.create(paths$root, recursive = TRUE, showWarnings = FALSE)
  analysis <- mrp_build_turnout_analysis(frame_result)
  analysis$calibration_contributions <-
    mrp_turnout_calibration_contributions(analysis$draws)
  targets <- sort(unique(pmin(
    0.99,
    pmax(0.01, config$run$calibration$turnout + c(-0.04, -0.02, 0, 0.02))
  )))
  analysis$sensitivity <- mrp_turnout_sensitivity(frame_result, targets)
  mrp_assert_turnout_analysis_target(
    analysis, config$run$calibration$turnout
  )
  metadata <- list(
    schema_version = MRP_TURNOUT_ANALYSIS_VERSION,
    generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    frame_signature = if (is.null(frame_signature)) {
      frame_result$signature
    } else {
      frame_signature
    },
    poststrat_source = config$run$poststrat_source,
    inference = config$inference$name,
    ndraws = length(unique(analysis$draws$draw)),
    turnout_target = config$run$calibration$turnout,
    recall_weight = frame_result$frame$recall_weight,
    variables = unique(analysis$draws$variable)
  )

  readr::write_csv(analysis$summary, paths$summary, na = "")
  readr::write_csv(
    analysis$correlation_summary, paths$correlations, na = ""
  )
  readr::write_csv(analysis$sensitivity, paths$sensitivity, na = "")
  mrp_atomic_save_rds(
    list(
      metadata = metadata,
      draws = analysis$draws,
      correlations = analysis$correlations,
      summary = analysis$summary,
      correlation_summary = analysis$correlation_summary,
      calibration_contributions = analysis$calibration_contributions,
      sensitivity = analysis$sensitivity
    ),
    paths$draws
  )
  display_analysis <- analysis
  display_analysis$summary <- mrp_turnout_display_summary(
    analysis$summary, config
  )
  mrp_turnout_write_plots(display_analysis, paths)
  writeLines(
    mrp_turnout_report_lines(display_analysis, metadata, paths),
    paths$report,
    useBytes = TRUE
  )
  list(
    markdown = paths$report,
    summary_csv = paths$summary,
    correlations_csv = paths$correlations,
    sensitivity_csv = paths$sensitivity,
    draws_rds = paths$draws,
    figures = unname(unlist(paths[c(
      "recall_plot", "demographics_plot",
      "provinces_plot", "predictors_plot", "calibration_plot"
    )]))
  )
}
