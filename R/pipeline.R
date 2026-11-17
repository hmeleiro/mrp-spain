mrp_previous_targets <- function(results_path, schema, recall_levels) {
  schema <- mrp_schema(schema)
  results <- mrp_read_data(results_path)
  extras <- results |>
    dplyr::select(provincia = codigo_provincia, votos_blancos, votos_nulos, abstenciones) |>
    dplyr::distinct() |>
    tidyr::pivot_longer(
      votos_blancos:abstenciones, names_to = "recuerdo", values_to = "votos"
    ) |>
    dplyr::mutate(recuerdo = ifelse(recuerdo == "votos_blancos", "OTBL", "ABNL"))
  main <- results |>
    dplyr::transmute(
      provincia = stringr::str_pad(as.character(codigo_provincia), 2, pad = "0"),
      recuerdo = mrp_flat_recode(
        mrp_normalize_electoral_party(partido_recode),
        schema$results_recode
      ),
      votos
    )
  dplyr::bind_rows(main, extras) |>
    dplyr::mutate(
      provincia = stringr::str_pad(as.character(provincia), 2, pad = "0"),
      recuerdo = factor(recuerdo, levels = recall_levels)
    ) |>
    dplyr::summarise(votos = sum(votos), .by = c(provincia, recuerdo)) |>
    tidyr::complete(provincia, recuerdo, fill = list(votos = 0)) |>
    dplyr::mutate(target_share = votos / sum(votos), .by = provincia)
}

mrp_impute_recall <- function(fit, poststrat, previous, provincial, results_path,
                              schema, ndraws) {
  aligned <- mrp_align_frame(poststrat, previous)
  groups <- c(
    "ccaa", "provincia", mrp_optional_terms(aligned, "tamuni"),
    "sexo", "edad_grupo", "estudios", "sitlab2"
  )
  cells <- aligned |>
    dplyr::summarise(n = sum(FACTOREL), .by = dplyr::all_of(groups)) |>
    dplyr::left_join(provincial, by = "provincia")
  mrp_assert_complete(cells, mrp_provincial_predictors(cells), "recall poststrat")
  probability <- brms::posterior_epred(
    fit, newdata = cells, ndraws = ndraws, re_formula = NULL,
    allow_new_levels = TRUE, sample_new_levels = "uncertainty"
  )
  probability <- mrp_prepare_epred(probability)
  levels <- dimnames(probability)[[3]]
  targets <- mrp_previous_targets(results_path, schema, levels)
  n_cells <- nrow(cells)
  n_levels <- length(levels)
  base <- cells[rep(seq_len(n_cells), each = n_levels), , drop = FALSE]
  base$recuerdo <- factor(rep(levels, times = n_cells), levels = levels)
  base$n_celda <- base$n
  counts_model <- matrix(0, nrow = dim(probability)[1], ncol = nrow(base))
  counts_official <- matrix(0, nrow = dim(probability)[1], ncol = nrow(base))
  province_index <- split(seq_len(n_cells), as.character(cells$provincia))

  for (draw in seq_len(nrow(counts_model))) {
    draw_probability <- probability[draw, , , drop = FALSE]
    dim(draw_probability) <- c(n_cells, n_levels)
    draw_counts <- sweep(draw_probability, 1, cells$n, `*`)
    official_counts <- draw_counts
    for (province in names(province_index)) {
      index <- province_index[[province]]
      province_counts <- official_counts[index, , drop = FALSE]
      target <- targets$target_share[
        targets$provincia == province
      ]
      target <- target[match(levels, as.character(
        targets$recuerdo[targets$provincia == province]
      ))]
      official_counts[index, ] <- mrp_rake_matrix(
        province_counts,
        row_targets = cells$n[index],
        column_targets = sum(cells$n[index]) * target
      )
    }
    counts_model[draw, ] <- as.vector(t(draw_counts))
    counts_official[draw, ] <- as.vector(t(official_counts))
  }
  base$n_imputado_model <- colMeans(counts_model)
  base$n_imputado_official <- colMeans(counts_official)
  list(
    base = base,
    scenarios = list(model = counts_model, official = counts_official)
  )
}

mrp_turnout_epred <- function(fit, newdata, ndraws, type = "binary_codes") {
  probability <- brms::posterior_epred(
    fit, newdata = newdata, ndraws = ndraws,
    re_formula = NULL, allow_new_levels = TRUE,
    sample_new_levels = "uncertainty"
  )
  if (type == "ordinal_scale") {
    if (!is.array(probability) || length(dim(probability)) != 3L) {
      stop("Ordinal turnout prediction has unexpected shape.", call. = FALSE)
    }
    labels <- levels(fit$data$participa)
    if (length(labels) != dim(probability)[3]) {
      labels <- dimnames(probability)[[3]]
    }
    values <- suppressWarnings(as.numeric(labels))
    if (anyNA(values) || diff(range(values)) <= 0) {
      stop("Ordinal turnout categories must be numeric.", call. = FALSE)
    }
    values <- (values - min(values)) / diff(range(values))
    expected <- matrix(0, nrow = dim(probability)[1], ncol = dim(probability)[2])
    for (index in seq_along(values)) {
      expected <- expected + probability[, , index] * values[index]
    }
    return(expected)
  }
  if (length(dim(probability)) == 3L) probability <- probability[, , 1]
  if (!is.matrix(probability)) {
    stop("Binary turnout prediction has unexpected shape.", call. = FALSE)
  }
  probability
}

mrp_calibrate_turnout_logit <- function(probability, population, target,
                                        epsilon = 1e-8) {
  if (!is.matrix(probability) || !is.matrix(population) ||
      !identical(dim(probability), dim(population))) {
    stop("Turnout probabilities and population must have identical matrices.",
         call. = FALSE)
  }
  if (!is.numeric(target) || length(target) != 1L || !is.finite(target) ||
      target <= 0 || target >= 1) {
    stop("Turnout target must be strictly between zero and one.", call. = FALSE)
  }
  calibrated <- probability
  delta <- numeric(nrow(probability))
  for (draw in seq_len(nrow(probability))) {
    base <- pmin(pmax(probability[draw, ], epsilon), 1 - epsilon)
    weights <- population[draw, ]
    objective <- function(shift) {
      sum(weights * stats::plogis(stats::qlogis(base) + shift)) /
        sum(weights) - target
    }
    delta[draw] <- stats::uniroot(
      objective, interval = c(-30, 30), tol = 1e-12
    )$root
    calibrated[draw, ] <- stats::plogis(stats::qlogis(base) + delta[draw])
  }
  list(
    probability = calibrated,
    voters = population * calibrated,
    delta = delta
  )
}

mrp_impute_turnout <- function(fit, base, population, turnout_target,
                               type = "binary_codes", probability = NULL) {
  if (is.null(probability)) {
    probability <- mrp_turnout_epred(
      fit, base, nrow(population), type = type
    )
  }
  if (!is.matrix(probability) ||
      !identical(dim(probability), dim(population))) {
    stop("Shared turnout draws do not match recall draws.", call. = FALSE)
  }
  calibrated <- mrp_calibrate_turnout_logit(
    probability, population, turnout_target
  )
  list(
    n_imputado = population,
    p_vota = probability,
    p_vota_cal = calibrated$probability,
    n_votantes_esperados = calibrated$voters,
    calibration_delta = calibrated$delta
  )
}

mrp_build_frame <- function(config, signature, rebuild = FALSE,
                            signature_manifest = NULL,
                            previous_schema = NULL, turnout_schema = NULL) {
  paths <- config$resolved_paths
  cache_paths <- c(
    frame = paths$frame,
    turnout = paths$model_turnout,
    recall = paths$model_recall
  )
  cache_status <- if (!rebuild) {
    lapply(
      cache_paths, mrp_cache_status,
      signature = signature, manifest = signature_manifest
    )
  } else {
    lapply(cache_paths, function(path) {
      list(valid = FALSE, reason = "rebuild requested")
    })
  }
  cache_complete <- !rebuild &&
    all(vapply(cache_status, `[[`, logical(1), "valid"))
  if (cache_complete) {
    message("Reusing poststrat frame.")
    return(readRDS(paths$frame))
  }
  if (!rebuild) {
    invalid <- names(cache_status)[
      !vapply(cache_status, `[[`, logical(1), "valid")
    ]
    message(
      "Frame cache miss: ",
      paste(
        paste0(
          invalid, " (",
          vapply(cache_status[invalid], `[[`, character(1), "reason"),
          ")"
        ),
        collapse = "; "
      )
    )
  }
  message("Building poststrat frame and auxiliary models.")
  frame_schema <- config$schemas$poststrat[[config$run$poststrat_source]]
  previous_schema <- previous_schema %||% config$schemas$previous
  turnout_schema <- turnout_schema %||% config$schemas$vote
  mrp_require_paths(
    c(
      paths$raw_poststrat, paths$raw_previous, paths$raw_results,
      paths$raw_degurba, file.path(config$paths$raw_root, "CensoPersonas_2021.parquet")
    ),
    "frame input"
  )
  provincial <- mrp_prepare_provincial_data(
    paths$raw_results,
    file.path(config$paths$raw_root, "CensoPersonas_2021.parquet"),
    paths$raw_degurba,
    previous_schema,
    config$domain
  )
  poststrat <- mrp_prepare_poststrat(paths$raw_poststrat, frame_schema)
  previous <- mrp_prepare_previous_survey(paths$raw_previous, previous_schema)
  turnout <- if (identical(paths$raw_turnout %||% paths$raw_previous, paths$raw_previous) &&
                 identical(turnout_schema, previous_schema)) {
    previous
  } else {
    mrp_prepare_previous_survey(
      paths$raw_turnout, turnout_schema,
      recent_studies = config$run$recent_studies
    )
  }
  auxiliary <- mrp_fit_auxiliary_models(
    previous, turnout, poststrat, provincial, config$inference,
    checkpoints = list(
      turnout = paths$checkpoint_turnout %||%
        file.path(
          paths$models_root, "checkpoints", config$inference$name,
          "turnout.rds"
        ),
      recall = paths$checkpoint_recall %||%
        file.path(
          paths$models_root, "checkpoints", config$inference$name,
          "recall.rds"
        )
    ),
    reuse_checkpoints = !rebuild
  )
  mrp_atomic_save_rds(
    c(
      list(
        signature = signature,
        signature_manifest = signature_manifest
      ),
      auxiliary$turnout
    ),
    paths$model_turnout
  )
  mrp_atomic_save_rds(
    c(
      list(
        signature = signature,
        signature_manifest = signature_manifest
      ),
      auxiliary$recall
    ),
    paths$model_recall
  )
  recall <- mrp_impute_recall(
    auxiliary$recall$model, poststrat, previous, provincial,
    paths$raw_results, previous_schema,
    config$inference$prediction_draws
  )
  turnout_probability <- mrp_turnout_epred(
    auxiliary$turnout$model, recall$base,
    config$inference$prediction_draws,
    type = auxiliary$turnout$type
  )
  turnout_scenarios <- lapply(recall$scenarios, function(population) {
    mrp_impute_turnout(
      auxiliary$turnout$model, recall$base, population,
      config$run$calibration$turnout,
      type = auxiliary$turnout$type,
      probability = turnout_probability
    )
  })
  recall_weight <- config$run$calibration$recall
  blend <- function(name) {
    (1 - recall_weight) * turnout_scenarios$model[[name]] +
      recall_weight * turnout_scenarios$official[[name]]
  }
  n_imputado <- blend("n_imputado")
  n_votantes <- blend("n_votantes_esperados")
  frame_base <- recall$base |>
    dplyr::mutate(
      n_imputado = colMeans(n_imputado),
      p_vota = colMeans(turnout_scenarios$model$p_vota),
      n_votantes_esperados = colMeans(n_votantes),
      p_vota_cal = n_votantes_esperados / n_imputado
    )
  mrp_assert_complete(
    frame_base,
    c("n_imputado", "p_vota", "n_votantes_esperados", "p_vota_cal"),
    "final poststrat frame"
  )
  if (any(frame_base$p_vota_cal < 0 | frame_base$p_vota_cal > 1)) {
    stop("Calibrated turnout probabilities must stay in [0, 1].", call. = FALSE)
  }
  frame <- list(
    base = frame_base,
    n_imputado = n_imputado,
    p_vota = turnout_scenarios$model$p_vota,
    n_votantes_esperados = n_votantes,
    scenarios = turnout_scenarios,
    recall_weight = recall_weight
  )
  recall_calibration <- mrp_recall_target_audit(
    list(frame = frame), paths$raw_results, previous_schema
  )
  result <- list(
    signature = signature,
    signature_manifest = signature_manifest,
    frame = frame,
    provincial = provincial,
    diagnostics = list(
      turnout = auxiliary$turnout$diagnostics,
      recall = auxiliary$recall$diagnostics,
      recall_calibration = recall_calibration
    )
  )
  mrp_atomic_save_rds(result, paths$frame)
  result
}

mrp_fit_estimation <- function(config, frame_result, signature, rebuild = FALSE,
                               signature_manifest = NULL,
                               vote_schema = NULL, variant = NULL) {
  paths <- config$resolved_paths
  variant <- variant %||% list(weight_scheme = "sample", geo_effect = "ccaa")
  cache_paths <- c(vote = paths$model_vote, survey = paths$survey)
  cache_status <- if (!rebuild) {
    lapply(
      cache_paths, mrp_cache_status,
      signature = signature, manifest = signature_manifest
    )
  } else {
    lapply(cache_paths, function(path) {
      list(valid = FALSE, reason = "rebuild requested")
    })
  }
  cached <- !rebuild &&
    all(vapply(cache_status, `[[`, logical(1), "valid"))
  if (cached) {
    message("Reusing prepared survey and vote model.")
    return(list(
      survey = readRDS(paths$survey),
      vote = readRDS(paths$model_vote)
    ))
  }
  if (!rebuild) {
    invalid <- names(cache_status)[
      !vapply(cache_status, `[[`, logical(1), "valid")
    ]
    message(
      "Estimate cache miss: ",
      paste(
        paste0(
          invalid, " (",
          vapply(cache_status[invalid], `[[`, character(1), "reason"),
          ")"
        ),
        collapse = "; "
      )
    )
  }
  message("Preparing CIS survey and fitting vote model.")
  vote_schema <- vote_schema %||% config$schemas$vote
  parties <- setdiff(unlist(vote_schema$voto_levels), c("ABNL", "<18", "Indecisos"))
  candidates <- mrp_validate_candidates(
    paths$raw_candidates, parties, config$domain$reference_party
  )
  candidates_fit <- mrp_apply_candidate_overrides(candidates, variant$overrides %||% NULL)
  survey <- mrp_prepare_vote_survey(
    paths$raw_vote, vote_schema, config$run$recent_studies, candidates_fit
  )
  prepared <- mrp_prepare_vote_model(
    survey$data, frame_result$provincial, frame_result$frame$base,
    paths$raw_candidates, config$domain, variant, config$run$temporal
  )
  fitted <- mrp_fit_vote_model(
    prepared,
    config$inference,
    checkpoint_path = paths$checkpoint_vote %||%
      file.path(
        paths$models_root, "checkpoints", config$inference$name,
        "vote.rds"
      ),
    reuse_checkpoint = !rebuild
  )
  survey_result <- list(
    signature = signature,
    signature_manifest = signature_manifest,
    data = survey$data,
    diagnostics = survey$diagnostics
  )
  vote_result <- c(
    list(
      signature = signature,
      signature_manifest = signature_manifest,
      parties = prepared$parties,
      candidates = prepared$candidates,
      variant = variant,
      temporal = prepared$temporal,
      geography = prepared$geography,
      weight_summary = prepared$weight_summary
    ),
    fitted
  )
  mrp_atomic_save_rds(survey_result, paths$survey)
  mrp_atomic_save_rds(vote_result, paths$model_vote)
  list(survey = survey_result, vote = vote_result)
}

mrp_blend_recall_votes <- function(model, official, weight) {
  if (!is.numeric(weight) || length(weight) != 1L || !is.finite(weight) ||
      weight < 0 || weight > 1) {
    stop("Recall blend weight must be between zero and one.", call. = FALSE)
  }
  keys <- c("draw", "provincia", "partido")
  if (!identical(model[keys], official[keys])) {
    stop("Recall scenario votes are not aligned.", call. = FALSE)
  }
  model |>
    dplyr::mutate(
      votos = (1 - weight) * votos + weight * official$votos
    ) |>
    dplyr::mutate(voto_pct = votos / sum(votos), .by = c(draw, provincia))
}

mrp_filter_seat_eligible <- function(votes, threshold,
                                     excluded_parties = "OTBL") {
  mrp_assert_complete(
    votes, c("draw", "provincia", "partido", "votos"),
    "seat eligibility"
  )
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      !is.finite(threshold) || threshold < 0 || threshold >= 1) {
    stop("District threshold must be between zero (inclusive) and one.",
         call. = FALSE)
  }
  if (any(!is.finite(votes$votos)) || any(votes$votos < 0)) {
    stop("Seat eligibility requires finite, non-negative votes.",
         call. = FALSE)
  }
  checked <- votes |>
    dplyr::mutate(
      .valid_votes = sum(votos),
      .district_share = votos / .valid_votes,
      .by = c(draw, provincia)
    )
  if (any(!is.finite(checked$.district_share)) ||
      any(checked$.valid_votes <= 0)) {
    stop("Every draw-province must contain positive valid votes.",
         call. = FALSE)
  }
  eligible <- checked |>
    dplyr::filter(
      !partido %in% excluded_parties,
      .district_share >= threshold
    )
  districts <- votes |>
    dplyr::distinct(draw, provincia)
  represented <- eligible |>
    dplyr::distinct(draw, provincia)
  missing <- dplyr::anti_join(
    districts, represented, by = c("draw", "provincia")
  )
  if (nrow(missing)) {
    stop(
      "District threshold leaves no eligible candidacy in ",
      nrow(missing), " draw-province pair(s).",
      call. = FALSE
    )
  }
  eligible |>
    dplyr::select(-.valid_votes, -.district_share)
}

mrp_allocate_votes <- function(votes, seats_path, domain) {
  seats <- mrp_read_data(seats_path) |>
    dplyr::mutate(provincia = mrp_normalize_province(provincia))
  allocation <- mrp_filter_seat_eligible(
    votes, domain$district_threshold
  ) |>
    dplyr::left_join(seats, by = "provincia")
  mrp_assert_complete(allocation, "nseats", "seat allocation")
  allocation <- vota::fast_dhondt(
    allocation,
    cod_prov = provincia, sim = draw, partido = partido,
    votos_prov = votos, nseats = nseats, n_next = 0
  )
  seat_counts <- allocation |>
    dplyr::count(draw, provincia, partido, name = "seats")
  sims <- votes |>
    dplyr::left_join(seat_counts, by = c("draw", "provincia", "partido")) |>
    dplyr::mutate(seats = tidyr::replace_na(seats, 0L)) |>
    dplyr::arrange(draw, provincia, dplyr::desc(votos))
  mrp_validate_output(sims, seats, domain$expected_seats)
  sims
}

mrp_predict <- function(config, frame_result, estimation,
                        include_ablation = FALSE) {
  paths <- config$resolved_paths
  survey <- estimation$survey$data
  vote <- estimation$vote
  poststrat <- frame_result$frame$base |>
    dplyr::mutate(
      sexo = factor(sexo, levels = levels(survey$sexo)),
      edad_grupo = factor(edad_grupo, levels = levels(survey$edad_grupo)),
      estudios = factor(estudios, levels = levels(survey$estudios)),
      provincia = mrp_factor_levels(provincia, levels(survey$provincia)),
      recuerdo = factor(recuerdo, levels = levels(survey$recuerdo))
    )
  if ("tamuni" %in% names(poststrat)) {
    poststrat$tamuni <- factor(poststrat$tamuni, levels = levels(survey$tamuni))
  }
  poststrat <- mrp_add_prediction_temporal(poststrat, vote$temporal)
  poststrat <- mrp_add_vote_geography(poststrat, vote$geography)
  poststrat <- mrp_add_availability_offsets(
    poststrat, vote$candidates, vote$parties,
    config$domain$reference_party, config$domain$unavailable_offset,
    availability_col = "se_presenta"
  )
  probability <- brms::posterior_epred(
    vote$model,
    newdata = poststrat,
    ndraws = nrow(frame_result$frame$n_votantes_esperados),
    re_formula = NULL,
    allow_new_levels = TRUE,
    sample_new_levels = "uncertainty"
  )
  probability <- mrp_prepare_epred(probability)
  if (!identical(dimnames(probability)[[3]], vote$parties)) {
    stop("Predicted parties do not match the fitted model.", call. = FALSE)
  }
  availability <- mrp_assert_forbidden_mass(
    probability, poststrat, vote$candidates,
    config$domain$availability_tolerance
  )
  scenario_votes <- lapply(
    frame_result$frame$scenarios,
    function(scenario) {
      mrp_aggregate_epred(
        probability, poststrat, scenario$n_votantes_esperados
      )
    }
  )
  recall_weight <- frame_result$frame$recall_weight
  votes <- mrp_blend_recall_votes(
    scenario_votes$model, scenario_votes$official, recall_weight
  )
  demographic_weights <-
    (1 - recall_weight) *
      frame_result$frame$scenarios$model$n_votantes_esperados +
    recall_weight *
      frame_result$frame$scenarios$official$n_votantes_esperados
  demographics <- mrp_aggregate_epred_by_groups(
    probability, poststrat, demographic_weights,
    c("sexo", "edad_grupo", "estudios", "sitlab2", "tamuni")
  )
  precalibrated_national <- votes |>
    dplyr::summarise(votos = sum(votos), .by = c(draw, partido)) |>
    dplyr::mutate(voto_pct = votos / sum(votos), .by = draw) |>
    dplyr::select(draw, partido, voto_pct)
  scenario_national <- dplyr::bind_rows(
    dplyr::mutate(scenario_votes$model, escenario = "model"),
    dplyr::mutate(scenario_votes$official, escenario = "official")
  ) |>
    dplyr::summarise(votos = sum(votos), .by = c(escenario, draw, partido)) |>
    dplyr::mutate(voto_pct = votos / sum(votos), .by = c(escenario, draw)) |>
    dplyr::summarise(
      voto_pct = mean(voto_pct),
      .by = c(escenario, partido)
    )
  scenario_contributions <- dplyr::bind_rows(
    dplyr::mutate(
      mrp_aggregate_epred_by_recall(
        probability, poststrat,
        frame_result$frame$scenarios$model$n_votantes_esperados
      ),
      escenario = "model"
    ),
    dplyr::mutate(
      mrp_aggregate_epred_by_recall(
        probability, poststrat,
        frame_result$frame$scenarios$official$n_votantes_esperados
      ),
      escenario = "official"
    )
  ) |>
    dplyr::mutate(
      total = sum(votos), .by = c(escenario, draw),
      contribucion_pct = votos / total
    ) |>
    dplyr::summarise(
      contribucion_pct = mean(contribucion_pct),
      .by = c(escenario, recuerdo, partido)
    )
  national <- mrp_calibrate_national_votes(
    votes, config$run$national_calibration
  )
  national$diagnostics$availability <- mrp_assert_forbidden_votes(
    national$votes, vote$candidates,
    config$domain$availability_tolerance
  )
  sims <- mrp_allocate_votes(
    national$votes, paths$raw_seats, config$domain
  )
  ablation <- NULL
  if (isTRUE(include_ablation)) {
    raw_scenario_votes <- lapply(
      frame_result$frame$scenarios,
      function(scenario) {
        mrp_aggregate_epred(
          probability, poststrat,
          scenario$n_imputado * scenario$p_vota
        )
      }
    )
    stage_votes <- list(
      actual = raw_scenario_votes$model,
      `+recuerdo_robusto` = mrp_blend_recall_votes(
        raw_scenario_votes$model, raw_scenario_votes$official, recall_weight
      ),
      `+participacion` = votes
    )
    if (identical(national$diagnostics$status, "PASS")) {
      stage_votes$`+calibracion_nacional` <- national$votes
    }
    ablation <- lapply(
      stage_votes,
      mrp_allocate_votes,
      seats_path = paths$raw_seats,
      domain = config$domain
    )
  }
  list(
    sims = sims,
    availability = availability,
    recall = list(
      weight = recall_weight,
      national = scenario_national,
      contributions = scenario_contributions
    ),
    precalibrated_national = precalibrated_national,
    demographics = demographics,
    national_calibration = national$diagnostics,
    ablation = ablation
  )
}

mrp_validate_output <- function(sims, seats, expected_seats = 350L) {
  mrp_assert_complete(
    sims, c("draw", "provincia", "partido", "votos", "voto_pct", "seats"),
    "sims_est"
  )
  totals <- sims |>
    dplyr::summarise(seats = sum(seats), .by = draw)
  if (any(totals$seats != expected_seats)) {
    stop("Every draw must allocate exactly ", expected_seats, " seats.", call. = FALSE)
  }
  shares <- sims |>
    dplyr::summarise(total = sum(voto_pct), .by = c(draw, provincia))
  if (any(abs(shares$total - 1) > 1e-8)) {
    stop("Provincial vote shares do not sum to one.", call. = FALSE)
  }
  invisible(TRUE)
}

mrp_source_modules <- function() {
  for (file in c("R/config.R", "R/data.R", "R/availability.R",
                 "R/calibration.R", "R/models.R", "R/reporting.R",
                 "R/participation.R", "R/vote-reports.R")) {
    if (!exists("mrp_load_config", mode = "function") || file != "R/config.R") {
      sys.source(file, envir = .GlobalEnv)
    }
  }
  invisible(TRUE)
}

run_mrp <- function(config_path = "config/mrp.yml", rebuild = FALSE,
                    .config = NULL, .variant = NULL,
                    .previous_schema = NULL, .turnout_schema = NULL,
                    .vote_schema = NULL, .signature_extra = list(),
                    .report = TRUE, .ablation = FALSE) {
  config <- .config %||% mrp_load_config(config_path)
  mrp_require_packages(c(
    "arrow", "brms", "cmdstanr", "digest", "dplyr", "flextable", "haven",
    "flexlsx", "ggplot2", "infoelectoral", "janitor", "officer", "openxlsx2",
    "posterior", "purrr", "readr", "readxl", "stringr", "tidyr", "vota",
    "yaml"
  ))
  mrp_ensure_dirs(config)
  frame_signature_result <- mrp_signature_manifest(
    config, "frame", extra = .signature_extra
  )
  frame_signature <- frame_signature_result$signature
  frame <- mrp_build_frame(
    config, frame_signature, rebuild,
    signature_manifest = frame_signature_result$manifest,
    previous_schema = .previous_schema,
    turnout_schema = .turnout_schema
  )
  turnout_reports <- if (isTRUE(.report)) {
    mrp_write_turnout_analysis(frame, config, frame_signature)
  } else {
    NULL
  }
  variant <- .variant %||% list(weight_scheme = "sample", geo_effect = "ccaa")
  estimate_signature_result <- mrp_signature_manifest(
    config, "estimate",
    extra = list(frame = frame_signature, variant = variant, runtime = .signature_extra)
  )
  estimate_signature <- estimate_signature_result$signature
  estimate <- mrp_fit_estimation(
    config, frame, estimate_signature, rebuild,
    signature_manifest = estimate_signature_result$manifest,
    vote_schema = .vote_schema, variant = variant
  )
  prediction <- mrp_predict(
    config, frame, estimate, include_ablation = .ablation
  )
  mrp_atomic_save_rds(prediction$sims, config$resolved_paths$sims)
  reports <- if (isTRUE(.report)) {
    main_reports <- mrp_write_reports(
      prediction$sims,
      config$resolved_paths$raw_results,
      config$resolved_paths,
      config$domain,
      estimate$survey$diagnostics,
      estimate$vote$candidates,
      prediction$availability,
      prediction$recall,
      config$run$calibration$turnout
    )
    invisible(gc())
    vote_reports <- mrp_write_vote_analysis(
      config, frame, estimate, prediction
    )
    c(
      main_reports,
      list(participacion = turnout_reports, voto = vote_reports)
    )
  } else {
    list(sims = config$resolved_paths$sims)
  }
  metadata <- list(
    schema_version = MRP_CONFIG_VERSION,
    completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    inference = config$inference$name,
    frame_signature = frame_signature,
    estimate_signature = estimate_signature,
    configuration = config$run,
    diagnostics = list(
      frame = frame$diagnostics,
      vote = estimate$vote$diagnostics,
      temporal = estimate$vote$temporal,
      geography = estimate$vote$geography,
      survey = estimate$survey$diagnostics,
      availability = prediction$availability,
      recall = prediction$recall,
      national_calibration = prediction$national_calibration
    ),
    outputs = reports,
    session = utils::sessionInfo()
  )
  mrp_atomic_save_rds(metadata, config$resolved_paths$metadata)
  invisible(list(
    sims = prediction$sims,
    metadata = metadata,
    ablation = prediction$ablation
  ))
}
