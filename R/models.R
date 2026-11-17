mrp_priors <- function(formula, data, family) {
  defaults <- brms::default_prior(formula, data = data, family = family)
  make <- function(class, prior) {
    dpars <- unique(defaults$dpar[defaults$class == class])
    if (!length(dpars)) return(NULL)
    Reduce(c, lapply(dpars, function(dpar) {
      brms::set_prior(prior, class = class, dpar = dpar)
    }))
  }
  c(
    make("b", "normal(0, 0.5)"),
    make("Intercept", "student_t(3, 0, 2)"),
    make("sd", "exponential(3)")
  )
}

mrp_mcmc_draws <- function(fit) {
  cmdstan_stanfit <- inherits(fit, "brmsfit") &&
    identical(fit$backend, "cmdstanr") &&
    inherits(fit$fit, "stanfit")
  draws <- if (cmdstan_stanfit) {
    # brms converts CmdStan output to stanfit. Warm-up is not stored in the
    # CSVs, although stanfit keeps the original warm-up count. Asking brms to
    # exclude warm-up again can therefore remove valid post-warm-up draws.
    posterior::as_draws_array(fit, inc_warmup = TRUE)
  } else {
    posterior::as_draws_array(fit)
  }
  if (posterior::ndraws(draws) == 0L) {
    stop(
      "Full-inference fit contains no post-warm-up draws.",
      call. = FALSE
    )
  }
  if (posterior::nchains(draws) < 2L) {
    stop(
      "Full-inference diagnostics require at least two MCMC chains.",
      call. = FALSE
    )
  }
  draws
}

mrp_stan_sampler_diagnostics <- function(fit) {
  empty <- list(divergences = NA_integer_, treedepth_hits = NA_integer_)
  if (!inherits(fit, "brmsfit") || !inherits(fit$fit, "stanfit")) {
    return(empty)
  }
  stanfit <- fit$fit
  sampler <- lapply(
    stanfit@sim$samples,
    function(chain) attr(chain, "sampler_params")
  )
  sampler <- Filter(
    function(chain) !is.null(chain) && nrow(chain) > 0L,
    sampler
  )
  if (!length(sampler)) return(empty)

  cmdstan_stanfit <- identical(fit$backend, "cmdstanr")
  if (!cmdstan_stanfit) {
    warmup <- stanfit@sim$warmup2 %||% rep(0L, length(sampler))
    sampler <- Map(function(chain, n) {
      if (n <= 0L) return(chain)
      chain[-seq_len(min(n, nrow(chain))), , drop = FALSE]
    }, sampler, warmup)
  }
  sampler <- do.call(rbind, sampler)
  divergences <- if ("divergent__" %in% colnames(sampler)) {
    sum(sampler[, "divergent__"], na.rm = TRUE)
  } else {
    NA_integer_
  }
  max_treedepth <- tryCatch(
    stanfit@stan_args[[1]]$control$max_treedepth,
    error = function(error) NULL
  )
  if (is.null(max_treedepth)) {
    max_treedepth <- tryCatch(
      stanfit@stan_args[[1]]$max_depth,
      error = function(error) NULL
    )
  }
  max_treedepth <- max_treedepth %||% 10L
  treedepth_hits <- if ("treedepth__" %in% colnames(sampler)) {
    sum(sampler[, "treedepth__"] >= max_treedepth, na.rm = TRUE)
  } else {
    NA_integer_
  }
  list(
    divergences = as.integer(divergences),
    treedepth_hits = as.integer(treedepth_hits)
  )
}

mrp_model_diagnostics <- function(fit, profile) {
  out <- list(profile = profile$name, algorithm = profile$algorithm)
  if (!profile$require_mcmc) return(out)
  draws <- mrp_mcmc_draws(fit)
  summary <- posterior::summarise_draws(
    draws,
    rhat = posterior::rhat,
    ess_bulk = posterior::ess_bulk,
    ess_tail = posterior::ess_tail
  )
  required <- c("rhat", "ess_bulk", "ess_tail")
  if (!all(required %in% names(summary)) || !nrow(summary)) {
    stop(
      "Full-inference posterior diagnostics could not be summarised.",
      call. = FALSE
    )
  }
  out$rhat_max <- max(summary$rhat, na.rm = TRUE)
  out$ess_bulk_min <- min(summary$ess_bulk, na.rm = TRUE)
  out$ess_tail_min <- min(summary$ess_tail, na.rm = TRUE)
  sampler <- mrp_stan_sampler_diagnostics(fit)
  out$divergences <- sampler$divergences
  out$divergence_rate <- if (is.na(out$divergences)) {
    NA_real_
  } else {
    out$divergences / posterior::ndraws(draws)
  }
  out$treedepth_hits <- sampler$treedepth_hits

  failures <- character()
  if (!is.finite(out$rhat_max) || out$rhat_max > 1.05) {
    failures <- c(failures, sprintf("max Rhat = %s", out$rhat_max))
  }
  if (!is.finite(out$ess_bulk_min) || out$ess_bulk_min < 400) {
    failures <- c(
      failures, sprintf("min bulk ESS = %s", out$ess_bulk_min)
    )
  }
  if (!is.finite(out$ess_tail_min) || out$ess_tail_min < 400) {
    failures <- c(
      failures, sprintf("min tail ESS = %s", out$ess_tail_min)
    )
  }
  max_divergence_rate <- 0.001
  if (!is.na(out$divergence_rate) &&
      out$divergence_rate > max_divergence_rate) {
    failures <- c(
      failures,
      sprintf(
        "divergences = %d (%.4f%%)",
        out$divergences,
        100 * out$divergence_rate
      )
    )
  } else if (!is.na(out$divergences) && out$divergences > 0) {
    warning(
      sprintf(
        paste0(
          "Proceeding with %d divergences (%.4f%%), ",
          "below the %.2f%% diagnostic gate."
        ),
        out$divergences,
        100 * out$divergence_rate,
        100 * max_divergence_rate
      ),
      call. = FALSE
    )
  }
  if (!is.na(out$treedepth_hits) && out$treedepth_hits > 0) {
    failures <- c(
      failures, sprintf("maximum-treedepth hits = %d", out$treedepth_hits)
    )
  }
  if (length(failures)) {
    stop(
      "Full-inference MCMC diagnostics failed: ",
      paste(failures, collapse = "; "),
      ".",
      call. = FALSE
    )
  }
  out
}

mrp_observed_model_data <- function(data, response) {
  if (!response %in% names(data)) {
    stop("Model response is absent from validation data: ", response, call. = FALSE)
  }
  observed <- data[!is.na(data[[response]]), , drop = FALSE]
  if (!nrow(observed)) {
    stop("Model response has no observed values: ", response, call. = FALSE)
  }
  observed
}

mrp_predictive_check <- function(fit, data, response, ndraws = 100L) {
  data <- mrp_observed_model_data(data, response)
  rows <- unique(round(seq(1, nrow(data), length.out = min(2000L, nrow(data)))))
  check_data <- data[rows, , drop = FALSE]
  predicted <- brms::posterior_predict(
    fit,
    newdata = check_data,
    ndraws = ndraws,
    allow_new_levels = TRUE
  )
  observed <- check_data[[response]]
  if (is.factor(observed)) {
    levels <- levels(observed)
    predicted <- factor(as.vector(predicted), levels = seq_along(levels), labels = levels)
    return(list(
      observed = prop.table(table(observed)),
      predicted = prop.table(table(predicted))
    ))
  }
  list(
    observed_mean = mean(observed),
    predicted_mean = mean(predicted)
  )
}

mrp_validate_categorical_probabilities <- function(fit, data, response, ndraws = 20L) {
  data <- mrp_observed_model_data(data, response)
  if (!is.factor(data[[response]]) || nlevels(data[[response]]) < 3L) {
    return(invisible(TRUE))
  }
  rows <- unique(round(seq(1, nrow(data), length.out = min(500L, nrow(data)))))
  probability <- brms::posterior_epred(
    fit,
    newdata = data[rows, , drop = FALSE],
    ndraws = ndraws,
    allow_new_levels = TRUE
  )
  if (!is.array(probability) || length(dim(probability)) != 3L ||
      any(!is.finite(probability))) {
    stop("Categorical fit produced non-finite probabilities.", call. = FALSE)
  }
  totals <- probability[, , 1]
  if (dim(probability)[3] > 1L) {
    for (category in 2:dim(probability)[3]) {
      totals <- totals + probability[, , category]
    }
  }
  if (any(abs(totals - 1) > 1e-8)) {
    stop(
      "Categorical fit produced probability vectors that do not sum to one; ",
      "the inference approximation is invalid.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

MRP_CHECKPOINT_VERSION <- 2L

mrp_fit_checkpoint_arguments <- function(arguments) {
  arguments[
    setdiff(
      names(arguments),
      c("formula", "data", "family", "prior", "cores", "refresh")
    )
  ]
}

mrp_fit_checkpoint_versions <- function(arguments) {
  cmdstan_version <- if (identical(arguments$backend, "cmdstanr")) {
    tryCatch(
      as.character(cmdstanr::cmdstan_version()),
      error = function(error) NA_character_
    )
  } else {
    NA_character_
  }
  c(
    R = paste(R.version$major, R.version$minor, sep = "."),
    brms = as.character(utils::packageVersion("brms")),
    cmdstan = cmdstan_version
  )
}

mrp_fit_checkpoint_manifest <- function(model_name, formula, data, family,
                                        prior, arguments) {
  effective_prior <- brms::validate_prior(
    prior, formula, data = data, family = family
  )
  stan_code <- brms::make_stancode(
    formula, data = data, family = family, prior = prior
  )
  stan_data <- brms::make_standata(
    formula, data = data, family = family, prior = prior
  )
  list(
    checkpoint_version = MRP_CHECKPOINT_VERSION,
    model_name = model_name,
    formula = digest::digest(stan_code, algo = "sha256"),
    family = capture.output(print(family)),
    prior = as.data.frame(effective_prior),
    stan_data = digest::digest(stan_data, algo = "sha256"),
    fit_arguments = mrp_fit_checkpoint_arguments(arguments),
    versions = mrp_fit_checkpoint_versions(arguments)
  )
}

mrp_fit_checkpoint_signature <- function(model_name, formula, data, family,
                                         prior, arguments) {
  manifest <- mrp_fit_checkpoint_manifest(
    model_name, formula, data, family, prior, arguments
  )
  digest::digest(manifest, algo = "sha256")
}

mrp_fit_checkpoint_signature_v1 <- function(model_name, formula, data, family,
                                            prior, arguments) {
  fit_arguments <- arguments[
    setdiff(
      names(arguments),
      c("formula", "data", "family", "prior", "cores", "refresh")
    )
  ]
  cmdstan_version <- if (identical(arguments$backend, "cmdstanr")) {
    tryCatch(
      as.character(cmdstanr::cmdstan_version()),
      error = function(error) NA_character_
    )
  } else {
    NA_character_
  }
  digest::digest(
    list(
      checkpoint_version = 1L,
      model_name = model_name,
      formula = capture.output(print(formula)),
      family = capture.output(print(family)),
      prior = as.data.frame(prior),
      data = digest::digest(data, algo = "sha256"),
      fit_arguments = fit_arguments,
      versions = c(
        R = paste(R.version$major, R.version$minor, sep = "."),
        brms = as.character(utils::packageVersion("brms")),
        cmdstan = cmdstan_version
      )
    ),
    algo = "sha256"
  )
}

mrp_legacy_sampling_arguments <- function(fit) {
  if (!inherits(fit, "brmsfit") ||
      !inherits(fit$fit, "stanfit") ||
      !identical(fit$algorithm, "sampling")) {
    return(NULL)
  }
  stan_args <- fit$fit@stan_args[[1]]
  control <- stan_args$control %||% list()
  list(
    backend = fit$backend,
    chains = as.integer(fit$fit@sim$chains),
    iter = as.integer(stan_args$iter),
    warmup = as.integer(stan_args$warmup),
    seed = as.integer(stan_args$seed),
    algorithm = fit$algorithm,
    control = list(
      adapt_delta = as.numeric(control$adapt_delta %||% 0.8),
      max_treedepth = as.integer(control$max_treedepth %||% 10L)
    )
  )
}

mrp_legacy_checkpoint_compatibility <- function(checkpoint, manifest) {
  fit <- checkpoint$model
  if (!inherits(fit, "brmsfit") || is.null(manifest)) {
    return(list(compatible = FALSE, changed = "checkpoint"))
  }
  stan_data <- tryCatch(
    {
      prior <- mrp_priors(fit$formula, fit$data, fit$family)
      list(
        code = brms::make_stancode(
          fit$formula, data = fit$data, family = fit$family,
          prior = prior
        ),
        data = brms::make_standata(
          fit$formula, data = fit$data, family = fit$family,
          prior = prior
        )
      )
    },
    error = function(error) NULL
  )
  actual_arguments <- mrp_legacy_sampling_arguments(fit)
  actual_versions <- c(
    brms = as.character(fit$version$brms %||% NA_character_),
    cmdstan = as.character(fit$version$cmdstan %||% NA_character_)
  )
  expected_versions <- manifest$versions[names(actual_versions)]
  checks <- c(
    model_name = identical(checkpoint$model_name, manifest$model_name),
    formula = !is.null(stan_data) && identical(
      digest::digest(stan_data$code, algo = "sha256"),
      manifest$formula
    ),
    family = identical(
      capture.output(print(fit$family)), manifest$family
    ),
    prior = identical(as.data.frame(fit$prior), manifest$prior),
    stan_data = !is.null(stan_data) && identical(
      digest::digest(stan_data$data, algo = "sha256"),
      manifest$stan_data
    ),
    fit_arguments = !is.null(actual_arguments) &&
      isTRUE(all.equal(
        actual_arguments, manifest$fit_arguments,
        check.attributes = TRUE
      )),
    versions = identical(actual_versions, expected_versions)
  )
  list(compatible = all(checks), changed = names(checks)[!checks])
}

mrp_read_fit_checkpoint <- function(path, signature, manifest = NULL,
                                    legacy_signature = NULL) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  checkpoint <- tryCatch(readRDS(path), error = function(error) NULL)
  if (!is.list(checkpoint) || !inherits(checkpoint$model, "brmsfit")) {
    return(NULL)
  }
  if (identical(checkpoint$checkpoint_version, MRP_CHECKPOINT_VERSION)) {
    if (!identical(checkpoint$signature, signature)) {
      changed <- mrp_manifest_changes(
        checkpoint$signature_manifest, manifest
      )
      if (length(changed)) {
        message(
          "Ignoring incompatible ", checkpoint$model_name,
          " checkpoint (changed: ",
          paste(changed, collapse = ", "), ")."
        )
      }
      return(NULL)
    }
    return(checkpoint)
  }
  if (!identical(checkpoint$checkpoint_version, 1L)) return(NULL)
  exact_legacy <- !is.null(legacy_signature) &&
    identical(checkpoint$signature, legacy_signature)
  compatibility <- if (exact_legacy) {
    list(compatible = TRUE, changed = character())
  } else {
    mrp_legacy_checkpoint_compatibility(checkpoint, manifest)
  }
  if (!isTRUE(compatibility$compatible)) {
    if (length(compatibility$changed)) {
      message(
        "Ignoring incompatible legacy ", checkpoint$model_name,
        " checkpoint (changed: ",
        paste(compatibility$changed, collapse = ", "), ")."
      )
    }
    return(NULL)
  }
  checkpoint$needs_migration <- TRUE
  checkpoint
}

mrp_save_fit_checkpoint <- function(path, signature, model_name, fit,
                                    warnings, manifest = NULL,
                                    completed_at = NULL,
                                    announce = TRUE) {
  if (is.null(path)) return(invisible(NULL))
  mrp_atomic_save_rds(
    list(
      checkpoint_version = MRP_CHECKPOINT_VERSION,
      signature = signature,
      signature_manifest = manifest,
      model_name = model_name,
      completed_at = completed_at %||%
        format(Sys.time(), tz = "UTC", usetz = TRUE),
      model = fit,
      warnings = unique(warnings)
    ),
    path
  )
  if (isTRUE(announce)) {
    message("Saved completed ", model_name, " fit checkpoint: ", path)
  }
  invisible(path)
}

mrp_fit_model <- function(model_name, formula, data, family, profile, response,
                          checkpoint_path = NULL, reuse_checkpoint = TRUE) {
  budget <- mrp_model_budget(model_name, profile)
  prior <- mrp_priors(formula, data, family)
  control <- mrp_model_control(model_name, profile)
  arguments <- list(
    formula = formula,
    data = data,
    family = family,
    prior = prior,
    backend = profile$backend,
    chains = mrp_model_chains(model_name, profile),
    cores = mrp_model_cores(model_name, profile),
    iter = budget$iter,
    warmup = budget$warmup,
    seed = profile$seed,
    algorithm = profile$algorithm,
    refresh = 100
  )
  if (length(control)) arguments$control <- control
  checkpoint_manifest <- mrp_fit_checkpoint_manifest(
    model_name, formula, data, family, prior, arguments
  )
  checkpoint_signature <- digest::digest(
    checkpoint_manifest, algo = "sha256"
  )
  legacy_signature <- mrp_fit_checkpoint_signature_v1(
    model_name, formula, data, family, prior, arguments
  )
  checkpoint <- if (isTRUE(reuse_checkpoint)) {
    mrp_read_fit_checkpoint(
      checkpoint_path, checkpoint_signature, checkpoint_manifest,
      legacy_signature
    )
  } else {
    NULL
  }
  if (!is.null(checkpoint)) {
    fit <- checkpoint$model
    warnings <- checkpoint$warnings %||% character()
    if (isTRUE(checkpoint$needs_migration)) {
      mrp_save_fit_checkpoint(
        checkpoint_path, checkpoint_signature, model_name, fit, warnings,
        checkpoint_manifest, checkpoint$completed_at, announce = FALSE
      )
      message("Migrated and reusing completed ", model_name, " fit checkpoint.")
    } else {
      message("Reusing completed ", model_name, " fit checkpoint.")
    }
  } else {
    warnings <- character()
    fit <- withCallingHandlers(
      do.call(brms::brm, arguments),
      warning = function(warning) {
        warnings <<- c(warnings, conditionMessage(warning))
        invokeRestart("muffleWarning")
      }
    )
    if (!inherits(fit, "brmsfit")) {
      stop(model_name, " fitting did not return a brmsfit object.", call. = FALSE)
    }
    mrp_save_fit_checkpoint(
      checkpoint_path, checkpoint_signature, model_name, fit, warnings,
      checkpoint_manifest
    )
  }
  mrp_validate_categorical_probabilities(fit, data, response)
  list(
    model = fit,
    diagnostics = mrp_model_diagnostics(fit, profile),
    predictive_check = mrp_predictive_check(
      fit, data, response, ndraws = profile$checks_ndraws
    ),
    warnings = unique(warnings)
  )
}

mrp_align_frame <- function(frame, survey) {
  out <- frame |>
    dplyr::mutate(
      sexo = factor(sexo, levels = levels(survey$sexo)),
      edad_grupo = factor(edad_grupo, levels = levels(survey$edad_grupo)),
      estudios = factor(estudios, levels = levels(survey$estudios)),
      sitlab2 = factor(sitlab2, levels = levels(survey$sitlab2)),
      provincia = mrp_factor_levels(provincia, levels(survey$provincia))
    )
  if ("tamuni" %in% names(out)) {
    out$tamuni <- factor(out$tamuni, levels = levels(survey$tamuni))
  }
  out
}

mrp_fit_auxiliary_models <- function(previous, turnout, poststrat, provincial,
                                     profile, checkpoints = list(),
                                     reuse_checkpoints = TRUE) {
  turnout_type <- attr(turnout, "participation_type") %||%
    if (is.ordered(turnout$participa)) "ordinal_scale" else "binary_codes"
  turnout_data <- turnout |>
    dplyr::left_join(provincial, by = "provincia")
  turnout_predictors <- mrp_provincial_predictors(turnout_data, include_turnout = TRUE)
  mrp_assert_complete(turnout_data, turnout_predictors, "turnout model data")
  turnout_formula <- mrp_build_formula(
    "participa",
    c(
      "sexo", "edad_grupo", "estudios", "sitlab2",
      mrp_optional_terms(poststrat, "tamuni"), turnout_predictors, "recuerdo"
    ),
    "(1 | provincia)"
  )
  turnout_fit <- mrp_fit_model(
    "turnout", turnout_formula, turnout_data,
    if (turnout_type == "ordinal_scale") {
      brms::cumulative("logit")
    } else {
      brms::bernoulli("logit")
    },
    profile, "participa",
    checkpoint_path = checkpoints$turnout %||% NULL,
    reuse_checkpoint = reuse_checkpoints
  )
  turnout_fit$type <- turnout_type

  recall_data <- previous |>
    dplyr::left_join(provincial, by = "provincia")
  recall_predictors <- mrp_provincial_predictors(recall_data)
  mrp_assert_complete(recall_data, recall_predictors, "recall model data")
  recall_formula <- mrp_build_formula(
    "recuerdo",
    c(
      "sexo", "edad_grupo", "estudios", "sitlab2",
      mrp_optional_terms(poststrat, "tamuni"), recall_predictors
    ),
    "(1 | provincia)"
  )
  recall_fit <- mrp_fit_model(
    "recall", recall_formula, recall_data, brms::categorical(),
    profile, "recuerdo",
    checkpoint_path = checkpoints$recall %||% NULL,
    reuse_checkpoint = reuse_checkpoints
  )
  list(turnout = turnout_fit, recall = recall_fit)
}

MRP_TEMPORAL_MIN_PERIODS <- 5L

mrp_temporal_features <- function(data, temporal) {
  temporal <- mrp_temporal_spec(temporal, "temporal")
  if (!"fecha_campo" %in% names(data)) {
    data$fecha_campo <- as.Date(NA)
  }
  dates <- mrp_coerce_date(data$fecha_campo)
  index <- if (temporal$unit == "month") {
    as.integer(format(dates, "%Y")) * 12L +
      as.integer(format(dates, "%m"))
  } else {
    as.integer(dates)
  }
  labels <- ifelse(
    is.na(dates), NA_character_,
    format(dates, if (temporal$unit == "month") "%Y-%m" else "%Y-%m-%d")
  )
  observed <- sort(unique(index[!is.na(index)]))
  period_levels <- if (length(observed)) {
    labels[match(observed, index)]
  } else {
    character()
  }
  span <- if (length(observed)) {
    as.integer(max(observed) - min(observed))
  } else {
    NA_integer_
  }
  latest <- if (length(observed)) max(observed) else NA_integer_
  trend <- if (length(observed) > 1L && span > 0L) {
    (index - latest) / span
  } else ifelse(is.na(index), NA_real_, 0)
  age <- if (length(observed)) latest - index else rep(NA_integer_, length(index))
  out <- dplyr::mutate(
    data,
    periodo = factor(labels, levels = period_levels),
    periodo_indice = as.integer(index),
    periodo_tendencia = as.numeric(trend),
    periodo_edad = as.integer(age)
  )
  attr(out, "temporal") <- c(
    temporal,
    list(
      periods = period_levels,
      n_periods = length(observed),
      first_period = if (length(period_levels)) period_levels[1] else NA_character_,
      last_period = if (length(period_levels)) tail(period_levels, 1) else NA_character_,
      span = span
    )
  )
  out
}

mrp_temporal_terms <- function(temporal) {
  if (identical(temporal$structure, "linear_varying")) {
    return(list(fixed = "periodo_tendencia", group = "(1 | periodo)"))
  }
  list(fixed = character(), group = character())
}

mrp_add_prediction_temporal <- function(data, temporal) {
  if (!identical(temporal$structure, "linear_varying")) return(data)
  periods <- temporal$periods %||% character()
  latest <- temporal$last_period %||% NA_character_
  if (!length(periods) || is.na(latest) || !latest %in% periods) {
    stop("Fitted temporal metadata are incomplete.", call. = FALSE)
  }
  data |>
    dplyr::mutate(
      periodo = factor(rep(latest, dplyr::n()), levels = periods),
      periodo_tendencia = 0
    )
}

mrp_add_vote_weights <- function(data, scheme = "sample", recency_lambda = 0.35,
                                 temporal) {
  if (!scheme %in% c("sample", "sample_recency")) {
    stop("Internal weight scheme must be sample or sample_recency.", call. = FALSE)
  }
  use_recency <- scheme == "sample_recency"
  data <- mrp_temporal_features(data, temporal)
  temporal_info <- attr(data, "temporal")
  use_model <- identical(temporal_info$structure, "linear_varying")
  if ((use_recency || use_model) && anyNA(data$fecha_campo)) {
    stop(
      "Temporal modeling and recency weighting require complete survey dates.",
      call. = FALSE
    )
  }
  if (use_model && temporal_info$n_periods < MRP_TEMPORAL_MIN_PERIODS) {
    stop(
      "Temporal structure linear_varying requires at least ",
      MRP_TEMPORAL_MIN_PERIODS, " distinct ", temporal_info$unit,
      " periods; found ", temporal_info$n_periods, ".",
      call. = FALSE
    )
  }
  if (use_recency &&
      (!is.numeric(recency_lambda) || length(recency_lambda) != 1L ||
       !is.finite(recency_lambda) || recency_lambda < 0)) {
    stop("recency_lambda must be a finite non-negative number.", call. = FALSE)
  }
  sample_weight <- data$ponde / mean(data$ponde)
  time_weight <- if (use_recency) {
    exp(-recency_lambda * data$periodo_edad)
  } else {
    1
  }
  out <- data |>
    dplyr::mutate(
      peso_modelo = as.numeric(sample_weight * time_weight)
    )
  attr(out, "temporal") <- temporal_info
  out
}

mrp_prepare_vote_model <- function(survey, provincial, poststrat, candidates,
                                   domain, variant, temporal) {
  predictors <- mrp_provincial_predictors(provincial)
  data <- survey |>
    dplyr::left_join(provincial, by = "provincia") |>
    dplyr::filter(!voto %in% c("ABNL", "<18", "Indecisos")) |>
    droplevels() |>
    mrp_add_vote_weights(
      variant$weight_scheme,
      variant$recency_lambda %||% 0.35,
      temporal
    )
  temporal_info <- attr(data, "temporal")
  temporal_terms <- mrp_temporal_terms(temporal_info)
  parties <- levels(data$voto)
  candidates <- mrp_validate_candidates(
    candidates, parties, reference = domain$reference_party
  )
  candidates_fit <- mrp_apply_candidate_overrides(candidates, variant$overrides %||% NULL)
  mrp_assert_observed_available(data, candidates_fit)
  data <- mrp_add_availability_offsets(
    data, candidates_fit, parties, domain$reference_party,
    domain$unavailable_offset
  )
  map <- attr(data, "offset_map")
  province_geography <- switch(
    variant$geo_effect,
    ccaa = character(),
    ccaa_provincia = "(1 | provincia)",
    stop("Unsupported internal geography: ", variant$geo_effect, call. = FALSE)
  )
  geography <- mrp_prepare_vote_geography(
    data, candidates_fit, poststrat, parties, domain$reference_party,
    domain$vote_geography
  )
  data <- geography$data
  fixed_terms <- c(
    "sexo", "edad_grupo", mrp_optional_terms(poststrat, "tamuni"),
    "sexo * edad_grupo", "estudios", "recuerdo",
    temporal_terms$fixed,
    predictors
  )
  nonreference <- setdiff(parties, domain$reference_party)
  rhs <- stats::setNames(lapply(nonreference, function(party) {
    mrp_build_formula(
      "voto", fixed_terms,
      c(
        geography$terms[[party]], province_geography,
        temporal_terms$group
      )
    )
  }), nonreference)
  formula <- mrp_choice_formula(
    "voto", rhs, parties, domain$reference_party,
    weight_col = "peso_modelo", offset_map = map
  )
  list(
    data = data,
    candidates = candidates,
    parties = parties,
    formula = formula,
    family = brms::categorical(refcat = domain$reference_party),
    temporal = temporal_info,
    geography = geography$specification,
    weight_summary = data |>
      dplyr::summarise(
        n = dplyr::n(),
        min = min(peso_modelo),
        mean = mean(peso_modelo),
        max = max(peso_modelo),
        effective_n = sum(peso_modelo)^2 / sum(peso_modelo^2)
      )
  )
}

mrp_fit_vote_model <- function(prepared, profile, checkpoint_path = NULL,
                               reuse_checkpoint = TRUE) {
  mrp_fit_model(
    "vote", prepared$formula, prepared$data, prepared$family,
    profile, "voto",
    checkpoint_path = checkpoint_path,
    reuse_checkpoint = reuse_checkpoint
  )
}
