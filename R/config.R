MRP_CONFIG_VERSION <- 4L
MRP_FRAME_CACHE_VERSION <- 1L
MRP_ESTIMATE_CACHE_VERSION <- 3L

`%||%` <- function(x, y) if (is.null(x)) y else x

mrp_require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing R packages: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(packages)
}

mrp_assert_names <- function(x, allowed, label, required = allowed) {
  if (!is.list(x) || is.null(names(x))) {
    stop(label, " must be a named mapping.", call. = FALSE)
  }
  unknown <- setdiff(names(x), allowed)
  missing <- setdiff(required, names(x))
  if (length(unknown)) {
    stop("Unknown ", label, " key(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  if (length(missing)) {
    stop("Missing ", label, " key(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(x)
}

mrp_scalar_choice <- function(x, choices, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% choices) {
    stop(label, " must be one of: ", paste(choices, collapse = ", "), call. = FALSE)
  }
  x
}

mrp_scalar_number <- function(x, label, lower = -Inf, upper = Inf, integer = FALSE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
    x < lower || x > upper || (integer && x != as.integer(x))) {
    stop(label, " is invalid.", call. = FALSE)
  }
  if (integer) as.integer(x) else as.numeric(x)
}

mrp_scalar_logical <- function(x, label) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(label, " must be TRUE or FALSE.", call. = FALSE)
  }
  isTRUE(x)
}

mrp_temporal_spec <- function(x, label = "run.temporal") {
  mrp_assert_names(x, c("unit", "structure"), label)
  list(
    unit = mrp_scalar_choice(
      x$unit, c("month", "day"), paste0(label, ".unit")
    ),
    structure = mrp_scalar_choice(
      x$structure, c("none", "linear_varying"),
      paste0(label, ".structure")
    )
  )
}

mrp_vote_sampling_spec <- function(x, label = "run.vote_sampling") {
  mrp_assert_names(
    x, c("adapt_delta", "max_treedepth", "chains"), label
  )
  list(
    adapt_delta = mrp_scalar_number(
      x$adapt_delta, paste0(label, ".adapt_delta"), 0.8, 0.999
    ),
    max_treedepth = mrp_scalar_number(
      x$max_treedepth, paste0(label, ".max_treedepth"), 8, 15,
      integer = TRUE
    ),
    chains = mrp_scalar_number(
      x$chains, paste0(label, ".chains"), 2, 8, integer = TRUE
    )
  )
}

mrp_vote_geography_spec <- function(x, parties = NULL,
                                    label = "domain.vote_geography") {
  mrp_assert_names(
    x, c("random_min_ccaa", "support_exclusions"), label
  )
  random_min_ccaa <- mrp_scalar_number(
    x$random_min_ccaa, paste0(label, ".random_min_ccaa"),
    3, 19, integer = TRUE
  )
  exclusions <- x$support_exclusions
  if (is.null(exclusions) || !length(exclusions)) {
    exclusions <- data.frame(
      partido = character(), ccaa = character(),
      stringsAsFactors = FALSE
    )
  } else {
    if (!is.list(exclusions) || is.data.frame(exclusions)) {
      stop(label, ".support_exclusions must be a list.", call. = FALSE)
    }
    rows <- lapply(seq_along(exclusions), function(index) {
      entry_label <- paste0(label, ".support_exclusions[[", index, "]]")
      entry <- exclusions[[index]]
      mrp_assert_names(entry, c("partido", "ccaa"), entry_label)
      if (!is.character(entry$partido) || length(entry$partido) != 1L ||
          is.na(entry$partido) || !nzchar(trimws(entry$partido))) {
        stop(entry_label, ".partido is invalid.", call. = FALSE)
      }
      if (!is.character(entry$ccaa) || length(entry$ccaa) != 1L ||
          is.na(entry$ccaa) || !grepl("^[0-9]{2}$", entry$ccaa) ||
          !entry$ccaa %in% sprintf("%02d", 1:19)) {
        stop(entry_label, ".ccaa is invalid.", call. = FALSE)
      }
      data.frame(
        partido = trimws(entry$partido), ccaa = entry$ccaa,
        stringsAsFactors = FALSE
      )
    })
    exclusions <- do.call(rbind, rows)
  }
  if (anyDuplicated(paste(exclusions$partido, exclusions$ccaa))) {
    stop(label, ".support_exclusions contains duplicates.", call. = FALSE)
  }
  if (!is.null(parties)) {
    unknown <- setdiff(exclusions$partido, as.character(parties))
    if (length(unknown)) {
      stop(
        label, ".support_exclusions contains unknown parties: ",
        paste(unknown, collapse = ", "), call. = FALSE
      )
    }
  }
  list(
    random_min_ccaa = random_min_ccaa,
    support_exclusions = exclusions
  )
}

mrp_paths <- function(config) {
  raw <- config$paths$raw_root
  profile <- config$run$inference
  processed_base <- config$paths$processed_root
  models_base <- config$paths$models_root
  processed <- file.path(processed_base, profile)
  models <- file.path(models_base, profile)
  output <- config$paths$output_root
  source <- config$run$poststrat_source
  checkpoint_root <- file.path(models_base, "checkpoints", profile)

  list(
    raw_root = raw,
    raw_vote = file.path(raw, "cis-longitudinal_23.parquet"),
    raw_turnout = file.path(raw, "cis-longitudinal_23.parquet"),
    raw_previous = file.path(raw, "3420.sav"),
    raw_poststrat = if (source == "censo") {
      file.path(raw, "CensoPersonas_2021.parquet")
    } else {
      file.path(raw, "epa")
    },
    raw_results = file.path(raw, "resultados_cer_provincias.csv"),
    raw_candidates = file.path(raw, "candidaturas.csv"),
    raw_seats = file.path(raw, "nseats.xlsx"),
    raw_degurba = file.path(raw, "EU-27-LAU-2025-NUTS-2024.xlsx"),
    processed_root = processed,
    frame = file.path(processed, "poststrat_frame.rds"),
    survey = file.path(processed, "vote_survey.rds"),
    models_root = models_base,
    model_turnout = file.path(models, "turnout.rds"),
    model_recall = file.path(models, "recall.rds"),
    model_vote = file.path(models, "vote.rds"),
    checkpoint_turnout = file.path(checkpoint_root, "turnout.rds"),
    checkpoint_recall = file.path(checkpoint_root, "recall.rds"),
    checkpoint_vote = file.path(checkpoint_root, "vote.rds"),
    output_root = output,
    sims = file.path(output, "sims_est.rds"),
    docx = file.path(output, "estimacion.docx"),
    xlsx = file.path(output, "estimacion.xlsx"),
    availability_report = file.path(output, "informe_candidaturas.md"),
    recall_report = file.path(output, "informe_recuerdo.md"),
    metadata = file.path(output, "run_metadata.rds"),
    diagnostics_root = file.path(output, "diagnostics")
  )
}

mrp_inference_profile <- function(name) {
  name <- mrp_scalar_choice(name, c("fast", "full"), "run.inference")
  common <- list(
    backend = "cmdstanr",
    seed = 1234L,
    vote_sampling = list(
      adapt_delta = 0.9,
      max_treedepth = 10L,
      chains = 4L
    )
  )
  if (name == "fast") {
    return(c(common, list(
      name = "fast",
      algorithm = "meanfield",
      chains = 1L,
      cores = 1L,
      prediction_draws = 200L,
      checks_ndraws = 20L,
      require_mcmc = FALSE
    )))
  }
  c(common, list(
    name = "full",
    algorithm = "sampling",
    chains = 4L,
    cores = 4L,
    prediction_draws = 500L,
    checks_ndraws = 100L,
    require_mcmc = TRUE
  ))
}

mrp_model_budget <- function(model, profile) {
  if (profile$name == "fast") {
    return(switch(model,
      turnout = list(iter = 2000L, warmup = 0L),
      recall = list(iter = 5000L, warmup = 0L),
      vote = list(iter = 10000L, warmup = 0L),
      stop("Unknown model budget: ", model, call. = FALSE)
    ))
  }
  switch(model,
    turnout = list(iter = 1500L, warmup = 750L),
    recall = list(iter = 2000L, warmup = 400L),
    vote = list(iter = 2000L, warmup = 400L),
    stop("Unknown model budget: ", model, call. = FALSE)
  )
}

mrp_model_control <- function(model, profile) {
  model <- mrp_scalar_choice(
    model, c("turnout", "recall", "vote"), "model"
  )
  if (profile$name == "fast") {
    return(list(
      adapt_engaged = FALSE,
      eta = 0.05,
      eval_elbo = 100L,
      tol_rel_obj = 0.001
    ))
  }
  control <- list(adapt_delta = 0.8, max_treedepth = 10L)
  if (model == "vote") {
    control <- profile$vote_sampling[c("adapt_delta", "max_treedepth")]
  }
  control
}

mrp_model_chains <- function(model, profile) {
  model <- mrp_scalar_choice(
    model, c("turnout", "recall", "vote"), "model"
  )
  if (profile$name == "full" && model == "vote") {
    return(as.integer(profile$vote_sampling$chains))
  }
  profile$chains
}

mrp_model_cores <- function(model, profile) {
  min(as.integer(profile$cores), mrp_model_chains(model, profile))
}

mrp_model_fit_spec <- function(model, profile) {
  budget <- mrp_model_budget(model, profile)
  list(
    backend = profile$backend,
    chains = mrp_model_chains(model, profile),
    iter = budget$iter,
    warmup = budget$warmup,
    seed = profile$seed,
    algorithm = profile$algorithm,
    control = mrp_model_control(model, profile)
  )
}

mrp_load_config <- function(path = "config/mrp.yml") {
  mrp_require_packages(c("yaml", "digest"))
  if (!file.exists(path)) {
    stop("Configuration file not found: ", path, call. = FALSE)
  }
  config <- yaml::read_yaml(path)
  mrp_assert_names(
    config,
    c("run", "domain", "paths", "schemas", "backtests"),
    "top-level configuration"
  )
  mrp_assert_names(
    config$run,
    c(
      "poststrat_source", "recent_studies", "inference", "calibration",
      "national_calibration", "temporal", "vote_sampling"
    ),
    "run"
  )
  config$run$temporal <- mrp_temporal_spec(config$run$temporal)
  config$run$vote_sampling <- mrp_vote_sampling_spec(
    config$run$vote_sampling
  )
  mrp_assert_names(
    config$run$calibration,
    c("turnout", "recall"),
    "run.calibration"
  )
  mrp_assert_names(
    config$run$national_calibration,
    c("enabled", "draws_path", "strength", "otbl_strength"),
    "run.national_calibration",
    required = c("enabled", "draws_path", "strength")
  )
  mrp_assert_names(
    config$domain,
    c(
      "reference_party", "unavailable_offset", "availability_tolerance",
      "district_threshold", "expected_seats", "left_parties", "right_parties",
      "vote_geography"
    ),
    "domain"
  )
  config$domain$vote_geography <- mrp_vote_geography_spec(
    config$domain$vote_geography,
    parties = unlist(config$schemas$vote$voto_levels, use.names = FALSE)
  )
  mrp_assert_names(
    config$paths,
    c("raw_root", "processed_root", "models_root", "output_root"),
    "paths"
  )
  mrp_assert_names(
    config$backtests,
    c("elections", "bundles", "variants"),
    "backtests"
  )
  if (!identical(
    sort(as.character(unlist(config$backtests$elections))),
    sort(names(config$backtests$bundles))
  )) {
    stop("backtests.elections and backtests.bundles must match.", call. = FALSE)
  }
  variant_keys <- c("weight_scheme", "geo_effect", "recency_lambda")
  for (name in names(config$backtests$variants)) {
    mrp_assert_names(
      config$backtests$variants[[name]],
      variant_keys,
      paste0("backtests.variants.", name),
      required = c("weight_scheme", "geo_effect")
    )
  }

  config$run$poststrat_source <- mrp_scalar_choice(
    config$run$poststrat_source, c("censo", "epa"), "run.poststrat_source"
  )
  config$run$recent_studies <- mrp_scalar_number(
    config$run$recent_studies, "run.recent_studies", 1, Inf,
    integer = TRUE
  )
  config$run$inference <- mrp_scalar_choice(
    config$run$inference, c("fast", "full"), "run.inference"
  )
  config$run$calibration$turnout <- mrp_scalar_number(
    config$run$calibration$turnout, "run.calibration.turnout",
    .Machine$double.eps, 1 - .Machine$double.eps
  )
  config$run$calibration$recall <- mrp_scalar_number(
    config$run$calibration$recall, "run.calibration.recall", 0, 1
  )
  config$run$national_calibration$enabled <- mrp_scalar_logical(
    config$run$national_calibration$enabled,
    "run.national_calibration.enabled"
  )
  if (!is.character(config$run$national_calibration$draws_path) ||
    length(config$run$national_calibration$draws_path) != 1L ||
    is.na(config$run$national_calibration$draws_path) ||
    !nzchar(config$run$national_calibration$draws_path)) {
    stop("run.national_calibration.draws_path must be a path.", call. = FALSE)
  }
  if (isTRUE(config$run$national_calibration$enabled) &&
    !file.exists(config$run$national_calibration$draws_path)) {
    stop(
      "National calibration target file not found: ",
      config$run$national_calibration$draws_path,
      call. = FALSE
    )
  }
  config$run$national_calibration$strength <- mrp_scalar_number(
    config$run$national_calibration$strength,
    "run.national_calibration.strength", 0, 1
  )
  config$run$national_calibration$otbl_strength <- mrp_scalar_number(
    config$run$national_calibration$otbl_strength %||%
      config$run$national_calibration$strength,
    "run.national_calibration.otbl_strength",
    config$run$national_calibration$strength, 1
  )
  config$domain$unavailable_offset <- mrp_scalar_number(
    config$domain$unavailable_offset, "domain.unavailable_offset", -Inf, -1e-9
  )
  config$domain$availability_tolerance <- mrp_scalar_number(
    config$domain$availability_tolerance,
    "domain.availability_tolerance", .Machine$double.eps, 1
  )
  config$domain$district_threshold <- mrp_scalar_number(
    config$domain$district_threshold,
    "domain.district_threshold", 0, 1 - .Machine$double.eps
  )
  config$domain$expected_seats <- mrp_scalar_number(
    config$domain$expected_seats, "domain.expected_seats", 1, Inf,
    integer = TRUE
  )

  config$config_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  config$inference <- mrp_inference_profile(config$run$inference)
  config$inference$vote_sampling <- config$run$vote_sampling
  config$resolved_paths <- mrp_paths(config)
  class(config) <- c("mrp_config", class(config))
  config
}

mrp_require_paths <- function(paths, label = "input") {
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop("Missing ", label, ": ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(paths)
}

mrp_ensure_dirs <- function(config) {
  paths <- config$resolved_paths
  dirs <- c(
    paths$processed_root, paths$models_root, paths$output_root,
    paths$diagnostics_root, dirname(paths$frame), dirname(paths$survey),
    dirname(paths$model_turnout), dirname(paths$model_recall),
    dirname(paths$model_vote)
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

mrp_file_hash <- function(path) {
  if (dir.exists(path)) {
    files <- sort(list.files(path, recursive = TRUE, full.names = TRUE))
    return(digest::digest(vapply(files, digest::digest, character(1), file = TRUE)))
  }
  digest::digest(file = path, algo = "sha256")
}

mrp_signature_manifest <- function(config, kind = c("frame", "estimate"),
                                   extra = list()) {
  kind <- match.arg(kind)
  paths <- config$resolved_paths
  runtime_extra <- extra$runtime %||% extra
  inputs <- if (kind == "frame") {
    c(
      paths$raw_poststrat, paths$raw_previous, paths$raw_turnout,
      paths$raw_results, paths$raw_degurba,
      file.path(config$paths$raw_root, "CensoPersonas_2021.parquet")
    )
  } else {
    c(paths$raw_vote, paths$raw_candidates)
  }
  inputs <- unique(inputs)
  mrp_require_paths(inputs, paste(kind, "signature input"))
  ignored_extra <- c(
    "previous_schema", "turnout_schema", "vote_schema", "turnout_hash"
  )
  stage_extra <- runtime_extra[
    setdiff(names(runtime_extra), ignored_extra)
  ]
  payload <- if (kind == "frame") {
    list(
      cache_version = MRP_FRAME_CACHE_VERSION,
      config_version = MRP_CONFIG_VERSION,
      kind = kind,
      run = list(
        poststrat_source = config$run$poststrat_source,
        recent_studies = config$run$recent_studies,
        calibration = config$run$calibration
      ),
      inference = list(
        name = config$inference$name,
        prediction_draws = config$inference$prediction_draws,
        checks_ndraws = config$inference$checks_ndraws,
        turnout = mrp_model_fit_spec("turnout", config$inference),
        recall = mrp_model_fit_spec("recall", config$inference)
      ),
      domain = config$domain[c("left_parties", "right_parties")],
      schema = list(
        poststrat =
          config$schemas$poststrat[[config$run$poststrat_source]],
        previous =
          runtime_extra$previous_schema %||% config$schemas$previous,
        turnout =
          runtime_extra$turnout_schema %||% config$schemas$vote
      ),
      inputs = stats::setNames(
        vapply(inputs, mrp_file_hash, character(1)), inputs
      ),
      extra = stage_extra
    )
  } else {
    list(
      cache_version = MRP_ESTIMATE_CACHE_VERSION,
      config_version = MRP_CONFIG_VERSION,
      kind = kind,
      run = list(
        recent_studies = config$run$recent_studies,
        temporal = config$run$temporal,
        vote_sampling = config$run$vote_sampling
      ),
      inference = list(
        name = config$inference$name,
        checks_ndraws = config$inference$checks_ndraws,
        vote = mrp_model_fit_spec("vote", config$inference)
      ),
      domain = config$domain[
        c("reference_party", "unavailable_offset", "vote_geography")
      ],
      schema =
        runtime_extra$vote_schema %||% config$schemas$vote,
      inputs = stats::setNames(
        vapply(inputs, mrp_file_hash, character(1)), inputs
      ),
      frame = extra$frame,
      variant = extra$variant,
      extra = stage_extra
    )
  }
  list(
    signature = digest::digest(payload, algo = "sha256"),
    manifest = payload
  )
}

mrp_signature <- function(config, kind = c("frame", "estimate"), extra = list()) {
  mrp_signature_manifest(config, kind, extra)$signature
}

mrp_manifest_changes <- function(saved, expected) {
  if (is.null(saved) || is.null(expected)) {
    return(character())
  }
  keys <- union(names(saved), names(expected))
  keys[!vapply(keys, function(key) {
    identical(saved[[key]], expected[[key]])
  }, logical(1))]
}

mrp_cache_status <- function(path, signature, manifest = NULL) {
  if (!file.exists(path)) {
    return(list(valid = FALSE, reason = "missing"))
  }
  object <- tryCatch(readRDS(path), error = function(error) error)
  if (inherits(object, "error")) {
    return(list(
      valid = FALSE,
      reason = paste0("unreadable: ", conditionMessage(object))
    ))
  }
  if (!is.list(object) || is.null(object$signature)) {
    return(list(valid = FALSE, reason = "invalid cache object"))
  }
  if (!identical(object$signature, signature)) {
    changed <- mrp_manifest_changes(object$signature_manifest, manifest)
    reason <- if (length(changed)) {
      paste0("signature changed: ", paste(changed, collapse = ", "))
    } else {
      "signature changed"
    }
    return(list(valid = FALSE, reason = reason))
  }
  list(valid = TRUE, reason = "valid")
}

mrp_cache_valid <- function(path, signature, manifest = NULL) {
  isTRUE(mrp_cache_status(path, signature, manifest)$valid)
}

mrp_atomic_save_rds <- function(object, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(object, temporary)
  if (file.exists(path)) unlink(path)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically save ", path, call. = FALSE)
  }
  invisible(path)
}

mrp_validation_bundle <- function(config, election) {
  election <- mrp_scalar_choice(
    election, as.character(unlist(config$backtests$elections)), "election"
  )
  specification <- config$backtests$bundles[[election]]
  allowed <- c(
    "root", "previous", "turnout", "vote", "poststrat_source", "poststrat",
    "previous_results", "target_results", "candidates", "seats",
    "previous_schema", "turnout_schema", "vote_schema", "fit_overrides"
  )
  mrp_assert_names(
    specification, allowed, paste0("backtests.bundles.", election)
  )
  root <- file.path(config$paths$raw_root, specification$root)
  within_root <- function(name) file.path(root, specification[[name]])
  poststrat <- within_root("poststrat")
  if (!file.exists(poststrat)) {
    poststrat <- file.path(config$paths$raw_root, specification$poststrat)
  }
  overrides <- specification$fit_overrides
  if (length(overrides)) {
    overrides <- dplyr::bind_rows(overrides)
  } else {
    overrides <- NULL
  }
  list(
    election = election,
    root = root,
    previous = within_root("previous"),
    turnout = within_root("turnout"),
    vote = within_root("vote"),
    poststrat_source = specification$poststrat_source,
    poststrat = poststrat,
    previous_results = within_root("previous_results"),
    target_results = within_root("target_results"),
    candidates = within_root("candidates"),
    seats = within_root("seats"),
    previous_schema = within_root("previous_schema"),
    turnout_schema = within_root("turnout_schema"),
    vote_schema = within_root("vote_schema"),
    fit_overrides = overrides
  )
}
