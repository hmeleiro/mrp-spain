if (.Platform$OS.type == "windows" &&
    grepl("^C($|\\.)", Sys.getlocale("LC_CTYPE"))) {
  suppressWarnings(try(
    Sys.setlocale("LC_CTYPE", "Spanish_Spain.utf8"),
    silent = TRUE
  ))
}

source("R/config.R", encoding = "UTF-8")
source("R/data.R", encoding = "UTF-8")
source("R/availability.R", encoding = "UTF-8")
source("R/calibration.R", encoding = "UTF-8")
source("R/models.R", encoding = "UTF-8")
source("R/reporting.R", encoding = "UTF-8")
source("R/participation.R", encoding = "UTF-8")
source("R/vote-reports.R", encoding = "UTF-8")
source("R/pipeline.R", encoding = "UTF-8")
source("R/backtest.R", encoding = "UTF-8")

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

assert_error <- function(expression, pattern) {
  message <- tryCatch(
    {
      force(expression)
      NA_character_
    },
    error = function(error) conditionMessage(error)
  )
  assert(!is.na(message) && grepl(pattern, message), paste("Expected error:", pattern))
}

config <- mrp_load_config()
assert(config$run$poststrat_source %in% c("censo", "epa"), "Invalid source")
assert(MRP_CONFIG_VERSION == 4L, "Configuration schema version changed")
assert(
  MRP_FRAME_CACHE_VERSION == 1L &&
    MRP_ESTIMATE_CACHE_VERSION == 3L &&
    MRP_CHECKPOINT_VERSION == 2L,
  "Cache contract versions changed"
)
assert(
  mrp_inference_profile("fast")$prediction_draws == 200L &&
    mrp_inference_profile("full")$prediction_draws == 500L,
  "Inference profile prediction draws changed"
)
assert(
  identical(config$domain$district_threshold, 0.03),
  "Unexpected provincial electoral threshold"
)
assert(
  identical(
    mrp_model_budget("recall", mrp_inference_profile("full")),
    list(iter = 2000L, warmup = 400L)
  ),
  "Full recall sampling budget changed"
)
assert(
  identical(
    mrp_model_control("turnout", mrp_inference_profile("full")),
    list(adapt_delta = 0.8, max_treedepth = 10L)
  ) &&
    identical(
      mrp_model_control("recall", mrp_inference_profile("full")),
      list(adapt_delta = 0.8, max_treedepth = 10L)
    ) &&
    identical(
      mrp_model_control("vote", mrp_inference_profile("full")),
      list(adapt_delta = 0.9, max_treedepth = 10L)
    ),
  "Full sampler controls must raise adapt_delta only for vote"
)
assert(
  identical(
    config$run$temporal,
    list(unit = "month", structure = "none")
  ),
  "Production temporal defaults changed"
)
assert(
  identical(
    config$run$vote_sampling,
    list(adapt_delta = 0.9, max_treedepth = 10L, chains = 4L)
  ),
  "Vote sampling defaults changed"
)
assert(
  config$domain$vote_geography$random_min_ccaa == 5L &&
    identical(
      config$domain$vote_geography$support_exclusions,
      data.frame(
        partido = "PNV", ccaa = "15", stringsAsFactors = FALSE
      )
    ),
  "Vote geography defaults changed"
)
assert_error(
  mrp_vote_geography_spec(list(
    random_min_ccaa = 2, support_exclusions = list()
  )),
  "random_min_ccaa"
)
assert_error(
  mrp_vote_geography_spec(list(
    random_min_ccaa = 5, support_exclusions = list(), unknown = TRUE
  )),
  "Unknown .*vote_geography"
)
assert_error(
  mrp_vote_geography_spec(list(
    random_min_ccaa = 5,
    support_exclusions = list(list(partido = "PNV", ccaa = "20"))
  )),
  "ccaa"
)
assert_error(
  mrp_vote_geography_spec(
    list(
      random_min_ccaa = 5,
      support_exclusions = list(
        list(partido = "PNV", ccaa = "15"),
        list(partido = "PNV", ccaa = "15")
      )
    ),
    parties = c("PSOE", "PNV")
  ),
  "duplicates"
)
assert_error(
  mrp_vote_geography_spec(
    list(
      random_min_ccaa = 5,
      support_exclusions = list(list(partido = "UNKNOWN", ccaa = "15"))
    ),
    parties = c("PSOE", "PNV")
  ),
  "unknown parties"
)
assert_error(
  mrp_vote_sampling_spec(list(
    adapt_delta = 1, max_treedepth = 10, chains = 4
  )),
  "adapt_delta"
)
assert_error(
  mrp_vote_sampling_spec(list(
    adapt_delta = 0.9, max_treedepth = 16, chains = 4
  )),
  "max_treedepth"
)
assert_error(
  mrp_vote_sampling_spec(list(
    adapt_delta = 0.9, max_treedepth = 10, chains = 1
  )),
  "chains"
)
custom_sampling_profile <- mrp_inference_profile("full")
custom_sampling_profile$vote_sampling <- list(
  adapt_delta = 0.97, max_treedepth = 12L, chains = 2L
)
assert(
  identical(
    mrp_model_control("vote", custom_sampling_profile),
    list(adapt_delta = 0.97, max_treedepth = 12L)
  ) &&
    mrp_model_chains("vote", custom_sampling_profile) == 2L &&
    mrp_model_cores("vote", custom_sampling_profile) == 2L &&
    mrp_model_chains("turnout", custom_sampling_profile) == 4L &&
    mrp_model_control("turnout", custom_sampling_profile)$adapt_delta == 0.8,
  "Vote sampling overrides leaked into auxiliary models"
)
tracking_config <- mrp_load_config("config/mrp_tracking_29N.yml")
flash_config <- mrp_load_config("config/mrp_flash_rdl.yml")
assert(
  identical(
    tracking_config$run$temporal,
    list(unit = "day", structure = "linear_varying")
  ) &&
    identical(
      flash_config$run$temporal,
      list(unit = "month", structure = "none")
    ),
  "Temporal profile configuration changed"
)
assert(
  "FA" %in% tracking_config$domain$left_parties &&
    !"Frente Amplio" %in% tracking_config$domain$left_parties,
  "Tracking block configuration must use the canonical FA party code"
)
assert_error(
  mrp_temporal_spec(list(unit = "hour", structure = "none")),
  "temporal.unit"
)
assert_error(
  mrp_temporal_spec(list(unit = "day", structure = "random_walk")),
  "temporal.structure"
)
assert_error(
  mrp_temporal_spec(list(unit = "day", structure = "none", extra = TRUE)),
  "Unknown .*temporal"
)

daily_dates <- as.Date(c(
  "2026-10-01", "2026-10-02", "2026-10-04", "2026-10-05", "2026-10-08"
))
daily_temporal <- list(unit = "day", structure = "linear_varying")
daily_weighted <- mrp_add_vote_weights(
  data.frame(fecha_campo = daily_dates, ponde = 1),
  "sample", temporal = daily_temporal
)
daily_info <- attr(daily_weighted, "temporal")
assert(
  identical(
    daily_weighted$periodo_indice - daily_weighted$periodo_indice[1],
    c(0L, 1L, 3L, 4L, 7L)
  ) &&
    identical(daily_weighted$periodo_edad, c(7L, 6L, 4L, 3L, 0L)) &&
    isTRUE(all.equal(
      daily_weighted$periodo_tendencia,
      c(-1, -6 / 7, -4 / 7, -3 / 7, 0)
    )) &&
    daily_info$n_periods == 5L && daily_info$span == 7L,
  "Daily temporal features lost calendar gaps or normalization"
)
assert(
  identical(
    mrp_temporal_terms(daily_info),
    list(fixed = "periodo_tendencia", group = "(1 | periodo)")
  ) &&
    identical(
      mrp_temporal_terms(list(structure = "none")),
      list(fixed = character(), group = character())
    ),
  "Temporal formula terms changed"
)
daily_recency <- mrp_add_vote_weights(
  data.frame(fecha_campo = daily_dates, ponde = 1),
  "sample_recency", recency_lambda = 0.1,
  temporal = list(unit = "day", structure = "none")
)
assert(
  isTRUE(all.equal(
    daily_recency$peso_modelo,
    exp(-0.1 * c(7, 6, 4, 3, 0))
  )),
  "Daily recency weighting ignored calendar gaps"
)
assert_error(
  mrp_add_vote_weights(
    data.frame(fecha_campo = daily_dates[1:4], ponde = 1),
    "sample", temporal = daily_temporal
  ),
  "at least 5 distinct day periods"
)
assert_error(
  mrp_add_vote_weights(
    data.frame(fecha_campo = c(daily_dates[1:4], as.Date(NA)), ponde = 1),
    "sample", temporal = daily_temporal
  ),
  "complete survey dates"
)
monthly_dates <- as.Date(c("2026-01-15", "2026-02-15", "2026-03-15"))
monthly_weighted <- mrp_add_vote_weights(
  data.frame(fecha_campo = monthly_dates, ponde = 1),
  "sample_recency", recency_lambda = 0.35,
  temporal = list(unit = "month", structure = "none")
)
assert(
  identical(monthly_weighted$periodo_edad, 2:0) &&
    isTRUE(all.equal(
      monthly_weighted$peso_modelo,
      exp(-0.35 * (2:0))
    )),
  "Monthly recency weighting changed"
)
assert_error(
  mrp_add_vote_weights(
    data.frame(fecha_campo = c(monthly_dates[1:2], as.Date(NA)), ponde = 1),
    "sample_recency", temporal = list(unit = "month", structure = "none")
  ),
  "complete survey dates"
)
no_temporal_dates <- mrp_add_vote_weights(
  data.frame(fecha_campo = c(as.Date(NA), monthly_dates[1]), ponde = 1),
  "sample", temporal = list(unit = "month", structure = "none")
)
assert(nrow(no_temporal_dates) == 2L, "Disabled temporal structure required dates")
prediction_temporal <- mrp_add_prediction_temporal(
  data.frame(cell = 1:2), daily_info
)
assert(
  all(as.character(prediction_temporal$periodo) == daily_info$last_period) &&
    all(prediction_temporal$periodo_tendencia == 0) &&
    identical(levels(prediction_temporal$periodo), daily_info$periods),
  "Temporal prediction was not placed at the latest observed period"
)
fast_config <- config
fast_config$run$inference <- "fast"
fast_config$inference <- mrp_inference_profile("fast")
fast_config$resolved_paths <- mrp_paths(fast_config)
full_config <- config
full_config$run$inference <- "full"
full_config$inference <- mrp_inference_profile("full")
full_config$resolved_paths <- mrp_paths(full_config)
assert(
  !identical(fast_config$resolved_paths$frame, full_config$resolved_paths$frame) &&
    grepl("[/\\\\]fast[/\\\\]", fast_config$resolved_paths$frame) &&
    grepl("[/\\\\]full[/\\\\]", full_config$resolved_paths$frame) &&
    !identical(
      fast_config$resolved_paths$model_vote,
      full_config$resolved_paths$model_vote
    ) &&
    identical(
      dirname(fast_config$resolved_paths$checkpoint_turnout),
      file.path(config$paths$models_root, "checkpoints", "fast")
    ),
  "Production caches must be namespaced by inference profile"
)
assert(!"abnl" %in% names(config$run$calibration), "ABNL calibration still exists")
assert(
  identical(config$run$calibration$recall, 0.6),
  "Unexpected recall blend default"
)
assert(
  is.logical(config$run$national_calibration$enabled) &&
    length(config$run$national_calibration$enabled) == 1L,
  "National calibration enabled flag is invalid"
)

# Exercise the YAML boundary independently of the production target file.
national_yaml <- yaml::read_yaml("config/mrp.yml")
national_yaml$run$national_calibration$enabled <- FALSE
national_yaml$run$national_calibration$strength <- .2
national_yaml_path <- tempfile(fileext = ".yml")
for (value in list(NULL, .2, .8, 1)) {
  national_yaml$run$national_calibration["otbl_strength"] <- list(value)
  yaml::write_yaml(national_yaml, national_yaml_path)
  loaded <- mrp_load_config(national_yaml_path)$run$national_calibration
  assert(
    identical(loaded$otbl_strength, value %||% .2),
    "OTBL intensity was not resolved from YAML"
  )
}
national_yaml$run$national_calibration$otbl_strength <- NULL
yaml::write_yaml(national_yaml, national_yaml_path)
assert(
  mrp_load_config(national_yaml_path)$run$national_calibration$otbl_strength == .2,
  "Omitted OTBL intensity did not inherit strength"
)
for (value in list(-.1, .1, 1.1, Inf, NaN, NA_real_, "0.8", c(.4, .8))) {
  national_yaml$run$national_calibration$otbl_strength <- value
  yaml::write_yaml(national_yaml, national_yaml_path)
  assert_error(mrp_load_config(national_yaml_path), "otbl_strength")
}
national_yaml$run$national_calibration$otbl_strength <- .8
national_yaml$run$national_calibration$strength <- NULL
yaml::write_yaml(national_yaml, national_yaml_path)
assert_error(mrp_load_config(national_yaml_path), "Missing.*strength")
unlink(national_yaml_path)

report_estimate <- data.frame(
  partido = c("PP", "OTBL"),
  pct_m = c(40, 5), pct_lwr = c(38, 4), pct_upr = c(42, 6),
  seats_m = c(150L, 0L), seats_lwr = c(145L, 0L),
  seats_upr = c(155L, 0L)
)
report_previous <- data.frame(
  partido = c("PP", "OTBL"),
  pct_prev = c(35, 4), seats_prev = c(140L, 0L)
)
report_rows <- mrp_report_rows(report_estimate, report_previous)
assert(
  identical(
    names(report_rows),
    c(
      "partido", "pct_prev", "seats_prev", "pct_est", "seats_est",
      "pct_change", "seats_change"
    )
  ) &&
    !any(grepl("votos", names(report_rows), fixed = TRUE)),
  "National report exposed absolute vote columns"
)
assert(
  report_rows$seats_est[report_rows$partido == "OTBL"] == "-" &&
    report_rows$seats_change[report_rows$partido == "OTBL"] == "-",
  "OTBL report placeholders changed"
)
report_rows <- report_rows |>
  dplyr::arrange(partido == "OTBL")
assert(
  identical(tail(report_rows$partido, 1), "OTBL"),
  "OTBL is not the last party in the national report"
)
block_sims <- data.frame(
  draw = rep(1:2, each = 3),
  provincia = "01",
  partido = rep(c("PP", "PSOE", "OTBL"), 2),
  votos = c(60, 30, 10, 40, 50, 10),
  seats = c(2, 1, 0, 1, 2, 0)
)
block_right <- mrp_block_summary(block_sims, "PP", "Derecha")
assert(
  identical(block_right$pct_m, 50),
  "Block percentage was not calculated over all national votes"
)

threshold_votes <- data.frame(
  draw = 1L,
  provincia = "01",
  partido = c("PP", "EDGE", "BELOW", "OTBL"),
  votos = c(64.5, 3, 2.5, 30),
  voto_pct = c(.645, .03, .025, .30)
)
eligible <- mrp_filter_seat_eligible(threshold_votes, .03)
assert(
  identical(eligible$partido, c("PP", "EDGE")),
  "District threshold must include exactly 3% and exclude values below it"
)
assert(
  !"OTBL" %in% eligible$partido,
  "Blank and other votes must enter the denominator but not seat allocation"
)
assert_error(
  mrp_filter_seat_eligible(threshold_votes, 1),
  "District threshold"
)

bundle_2023 <- mrp_validation_bundle(config, "2023")
assert(
  identical(
    basename(bundle_2023$turnout),
    "cis-longitudinal-pre23J.parquet"
  ),
  "2023 turnout must use the same tracking source as vote"
)
fast_backtest_config <- mrp_backtest_config(
  fast_config, bundle_2023, "production"
)
full_backtest_config <- mrp_backtest_config(
  full_config, bundle_2023, "production"
)
assert(
  grepl(
    "[/\\\\]validation[/\\\\]2023[/\\\\]fast[/\\\\]",
    fast_backtest_config$resolved_paths$frame
  ) &&
    grepl(
      "[/\\\\]validation[/\\\\]2023[/\\\\]full[/\\\\]",
      full_backtest_config$resolved_paths$frame
    ) &&
    !identical(
      fast_backtest_config$resolved_paths$model_recall,
      full_backtest_config$resolved_paths$model_recall
    ) &&
    grepl(
      "[/\\\\]checkpoints[/\\\\]full[/\\\\]",
      full_backtest_config$resolved_paths$checkpoint_vote
    ),
  "Backtest caches must be namespaced without moving checkpoint roots"
)
assert(length(config$backtests$variants) == 3L, "Backtest must have three variants")
assert(
  identical(names(config$backtests$variants), c("production", "recency", "geography")),
  "Unexpected backtest variants"
)
parsed_backtest <- mrp_parse_backtest_args(c(
  "2023", "--variant=production,geography", "--rebuild"
))
assert(
  identical(parsed_backtest$election, "2023") &&
    identical(parsed_backtest$variants, c("production", "geography")) &&
    isTRUE(parsed_backtest$rebuild),
  "Backtest CLI variant parsing failed"
)
selected_variants <- mrp_select_backtest_variants(
  config, c("production", "geography")
)
assert(
  identical(names(selected_variants), c("production", "geography")),
  "Backtest variant selection failed"
)
assert_error(
  mrp_select_backtest_variants(config, "unknown"),
  "Unknown backtest variant"
)
mapped_collision <- mrp_apply_column_map(
  data.frame(codigo_ccaa = "01", ccaa = "old", retained = 1),
  list(column_map = list(codigo_ccaa = "ccaa"))
)
assert(
  !anyDuplicated(names(mapped_collision)) &&
    identical(mapped_collision$ccaa, "01") &&
    identical(mapped_collision$retained, 1),
  "Column mapping did not resolve an existing canonical-name collision"
)

bad <- unclass(config)
bad$run$unknown <- TRUE
assert_error(
  mrp_assert_names(
    bad$run,
    c(
      "poststrat_source", "recent_studies", "inference", "calibration",
      "national_calibration", "temporal", "vote_sampling"
    ),
    "run"
  ),
  "Unknown run"
)

parties <- c("PP", "PSOE", "PNV")
grid <- tidyr::expand_grid(
  provincia = sprintf("%02d", 1:52),
  partido = parties
)
grid$se_presenta <- TRUE
grid$se_presenta[grid$partido == "PNV" & grid$provincia != "48"] <- FALSE
candidates <- mrp_validate_candidates(grid, parties)
assert(nrow(candidates) == 156L, "Candidate grid size changed")
assert(sum(!candidates$se_presenta) == 51L, "Regional availability changed")
assert_error(
  mrp_validate_candidates(grid[-1, ], parties),
  "not the exact expected"
)

survey <- data.frame(
  provincia = factor(c("01", "48", "01")),
  voto = factor(c("PP", "PNV", "PNV"), levels = parties)
)
filtered <- mrp_filter_survey_availability(survey, candidates)
assert(nrow(filtered$data) == 2L, "Availability filter kept an invalid response")
assert(filtered$diagnostics$discarded == 1L, "Discard count is wrong")

offsets <- mrp_add_availability_offsets(
  data.frame(provincia = c("01", "48")),
  candidates,
  parties,
  unavailable_offset = -1e6
)
assert(
  identical(offsets$choice_offset_PNV, c(-1e6, 0)),
  "Availability offsets are wrong"
)

geo_crosswalk <- data.frame(
  provincia = c("01", "20", "31", "48", "02", "03", "04", "05", "06"),
  ccaa = c("16", "16", "15", "16", "01", "02", "03", "04", "05")
)
geo_parties <- c(
  "PSOE", "ONE", "TWO", "FOUR", "FIVE", "PNV", "EH_Bildu"
)
geo_candidates <- tidyr::expand_grid(
  provincia = geo_crosswalk$provincia,
  partido = geo_parties
) |>
  dplyr::left_join(geo_crosswalk, by = "provincia") |>
  dplyr::mutate(
    se_presenta = dplyr::case_when(
      partido == "PSOE" ~ TRUE,
      partido == "ONE" ~ ccaa == "01",
      partido == "TWO" ~ ccaa %in% c("15", "16"),
      partido == "FOUR" ~ ccaa %in% sprintf("%02d", 1:4),
      partido == "FIVE" ~ ccaa %in% sprintf("%02d", 1:5),
      partido %in% c("PNV", "EH_Bildu") ~ ccaa %in% c("15", "16"),
      TRUE ~ FALSE
    )
  ) |>
  dplyr::select(provincia, partido, se_presenta)
geo_config <- mrp_vote_geography_spec(
  list(
    random_min_ccaa = 5,
    support_exclusions = list(list(partido = "PNV", ccaa = "15"))
  ),
  parties = geo_parties
)
geo_prepared <- mrp_prepare_vote_geography(
  geo_crosswalk, geo_candidates, geo_crosswalk, geo_parties,
  "PSOE", geo_config
)
geo_spec <- geo_prepared$specification
assert(
  geo_spec$ONE$strategy == "none" &&
    !length(geo_spec$ONE$terms) &&
    geo_spec$TWO$strategy == "fixed" &&
    length(geo_spec$TWO$contrast_columns) == 1L &&
    geo_spec$FOUR$strategy == "fixed" &&
    length(geo_spec$FOUR$contrast_columns) == 3L &&
    geo_spec$FIVE$strategy == "random" &&
    identical(
      levels(geo_prepared$data[[geo_spec$FIVE$group_column]]),
      sprintf("%02d", 1:5)
    ),
  "Party-specific CCAA strategy thresholds changed"
)
assert(
  identical(geo_spec$PNV$original_support, c("15", "16")) &&
    identical(geo_spec$PNV$modeled_support, "16") &&
    geo_spec$PNV$strategy == "none" &&
    geo_candidates$se_presenta[
      geo_candidates$partido == "PNV" & geo_candidates$provincia == "31"
    ] &&
    geo_spec$EH_Bildu$strategy == "fixed" &&
    identical(geo_spec$EH_Bildu$reference_ccaa, "16") &&
    identical(geo_spec$EH_Bildu$contrast_ccaa, "15") &&
    geo_prepared$data[[geo_spec$EH_Bildu$contrast_columns]][
      geo_prepared$data$provincia == "31"
    ] == 1,
  "PNV or EH Bildu geography contract changed"
)
geo_prediction <- mrp_add_vote_geography(geo_crosswalk, geo_spec)
assert(
  identical(
    levels(geo_prediction[[geo_spec$FIVE$group_column]]),
    geo_spec$FIVE$modeled_support
  ) &&
    !anyNA(geo_prediction[[geo_spec$FIVE$group_column]]),
  "Prediction did not reconstruct party-specific geography"
)
all_excluded <- geo_config
all_excluded$support_exclusions <- data.frame(
  partido = "ONE", ccaa = "01", stringsAsFactors = FALSE
)
assert_error(
  mrp_vote_geography_terms(
    geo_candidates, geo_crosswalk, geo_parties, "PSOE", all_excluded
  ),
  "remove every CCAA for ONE"
)
geo_nonreference <- setdiff(geo_parties, "PSOE")
geo_rhs <- stats::setNames(lapply(geo_nonreference, function(party) {
  mrp_build_formula(
    "voto", "x", geo_prepared$terms[[party]]
  )
}), geo_nonreference)
geo_formula <- mrp_choice_formula(
  "voto", geo_rhs, geo_parties, "PSOE"
)
geo_formula_lines <- trimws(capture.output(print(geo_formula)))
geo_formula_text <- paste(geo_formula_lines, collapse = " ")
pnv_formula_line <- geo_formula_lines[grepl("^muPNV ~", geo_formula_lines)]
assert(
  grepl("muEHBildu.*geo_ccaa_EHBildu_15", geo_formula_text) &&
    length(pnv_formula_line) == 1L &&
    !grepl("geo_ccaa_PNV", pnv_formula_line) &&
    grepl("muFIVE.*\\(1 \\| geo_ccaa_FIVE\\)", geo_formula_text),
  "Category-specific RHS did not preserve party geography"
)
assert_error(
  mrp_choice_formula(
    "voto", geo_rhs[-1], geo_parties, "PSOE"
  ),
  "named for every non-reference party"
)

epred <- array(
  0,
  dim = c(1, 2, 3),
  dimnames = list(draw = "1", cell = c("1", "2"), partido = parties)
)
epred[1, 1, ] <- c(.6, .4, 0)
epred[1, 2, ] <- c(.5, .3, .2)
poststrat <- data.frame(provincia = c("01", "48"))
mass <- mrp_assert_forbidden_mass(epred, poststrat, candidates, 1e-10)
assert(mass$status == "PASS", "No-op guardrail failed")
epred[1, 1, 3] <- .01
assert_error(
  mrp_assert_forbidden_mass(epred, poststrat, candidates, 1e-10),
  "exceeds tolerance"
)
vote_guardrail <- data.frame(
  provincia = c("01", "01"),
  partido = c("PP", "PNV"),
  voto_pct = c(1, 0)
)
guardrail_candidates <- candidates[
  candidates$provincia == "01" & candidates$partido %in% c("PP", "PNV"),
]
assert(
  mrp_assert_forbidden_votes(
    vote_guardrail, guardrail_candidates, 1e-10
  )$status == "PASS",
  "Post-calibration availability guardrail failed"
)
vote_guardrail$voto_pct[2] <- .01
assert_error(
  mrp_assert_forbidden_votes(
    vote_guardrail, guardrail_candidates, 1e-10
  ),
  "exceeds tolerance"
)

cache_file <- tempfile(fileext = ".rds")
mrp_atomic_save_rds(
  list(
    signature = "a",
    signature_manifest = list(run = 1L, schema = "a"),
    value = 1
  ),
  cache_file
)
assert(
  mrp_cache_valid(
    cache_file, "a", list(run = 1L, schema = "a")
  ),
  "Valid signature was not reused"
)
assert(!mrp_cache_valid(cache_file, "b"), "Changed signature was reused")
cache_miss <- mrp_cache_status(
  cache_file, "b", list(run = 2L, schema = "a")
)
assert(
  !cache_miss$valid && grepl("run", cache_miss$reason, fixed = TRUE),
  "Cache mismatch did not report the changed manifest component"
)
unlink(cache_file)

checkpoint_file <- tempfile(fileext = ".rds")
checkpoint_fit <- structure(list(saved = TRUE), class = "brmsfit")
mrp_save_fit_checkpoint(
  checkpoint_file, "fit-a", "test", checkpoint_fit, "test warning",
  manifest = list(model_name = "test")
)
loaded_checkpoint <- mrp_read_fit_checkpoint(checkpoint_file, "fit-a")
assert(
  inherits(loaded_checkpoint$model, "brmsfit") &&
    identical(loaded_checkpoint$checkpoint_version, 2L) &&
    identical(
      loaded_checkpoint$signature_manifest,
      list(model_name = "test")
    ) &&
    identical(loaded_checkpoint$model_name, "test") &&
    identical(loaded_checkpoint$warnings, "test warning"),
  "Completed fit checkpoint was not reusable"
)
assert(
  is.null(mrp_read_fit_checkpoint(checkpoint_file, "fit-b")),
  "Mismatched fit checkpoint signature was reused"
)
unlink(checkpoint_file)

legacy_checkpoint_file <- tempfile(fileext = ".rds")
mrp_atomic_save_rds(
  list(
    checkpoint_version = 1L,
    signature = "legacy-fit",
    model_name = "test",
    completed_at = "2026-01-01 UTC",
    model = checkpoint_fit,
    warnings = character()
  ),
  legacy_checkpoint_file
)
legacy_checkpoint <- mrp_read_fit_checkpoint(
  legacy_checkpoint_file, "fit-v2",
  legacy_signature = "legacy-fit"
)
assert(
  isTRUE(legacy_checkpoint$needs_migration),
  "Exactly matching version-1 checkpoint was not accepted for migration"
)
unlink(legacy_checkpoint_file)

checkpoint_data <- data.frame(
  y = c(0, 1, 0, 1),
  x = factor(c("a", "a", "b", "b")),
  unused = 1:4
)
checkpoint_formula <- brms::bf(y ~ x)
checkpoint_family <- brms::bernoulli()
checkpoint_prior <- mrp_priors(
  checkpoint_formula, checkpoint_data, checkpoint_family
)
checkpoint_profile <- mrp_inference_profile("full")
checkpoint_budget <- mrp_model_budget("turnout", checkpoint_profile)
checkpoint_arguments <- list(
  formula = checkpoint_formula,
  data = checkpoint_data,
  family = checkpoint_family,
  prior = checkpoint_prior,
  backend = checkpoint_profile$backend,
  chains = checkpoint_profile$chains,
  cores = checkpoint_profile$cores,
  iter = checkpoint_budget$iter,
  warmup = checkpoint_budget$warmup,
  seed = checkpoint_profile$seed,
  algorithm = checkpoint_profile$algorithm,
  refresh = 100,
  control = mrp_model_control("turnout", checkpoint_profile)
)
checkpoint_signature <- mrp_fit_checkpoint_signature(
  "turnout", checkpoint_formula, checkpoint_data, checkpoint_family,
  checkpoint_prior, checkpoint_arguments
)
base_checkpoint_formula <- y ~ x
base_checkpoint_prior <- mrp_priors(
  base_checkpoint_formula, checkpoint_data, checkpoint_family
)
base_arguments <- checkpoint_arguments
base_arguments$formula <- base_checkpoint_formula
base_arguments$prior <- base_checkpoint_prior
assert(
  identical(
    checkpoint_signature,
    mrp_fit_checkpoint_signature(
      "turnout", base_checkpoint_formula, checkpoint_data,
      checkpoint_family, base_checkpoint_prior, base_arguments
    )
  ),
  "Equivalent formula and brmsformula representations changed the checkpoint signature"
)
irrelevant_checkpoint_data <- checkpoint_data
irrelevant_checkpoint_data$unused <- 11:14
attr(irrelevant_checkpoint_data$x, "label") <- "irrelevant label"
irrelevant_arguments <- checkpoint_arguments
irrelevant_arguments$data <- irrelevant_checkpoint_data
irrelevant_prior <- mrp_priors(
  checkpoint_formula, irrelevant_checkpoint_data, checkpoint_family
)
irrelevant_arguments$prior <- irrelevant_prior
assert(
  identical(
    checkpoint_signature,
    mrp_fit_checkpoint_signature(
      "turnout", checkpoint_formula, irrelevant_checkpoint_data,
      checkpoint_family, irrelevant_prior, irrelevant_arguments
    )
  ),
  "Unused columns or labels changed the version-2 checkpoint signature"
)
changed_checkpoint_data <- checkpoint_data
changed_checkpoint_data$x[1] <- "b"
changed_arguments <- checkpoint_arguments
changed_arguments$data <- changed_checkpoint_data
changed_prior <- mrp_priors(
  checkpoint_formula, changed_checkpoint_data, checkpoint_family
)
changed_arguments$prior <- changed_prior
assert(
  !identical(
    checkpoint_signature,
    mrp_fit_checkpoint_signature(
      "turnout", checkpoint_formula, changed_checkpoint_data,
      checkpoint_family, changed_prior, changed_arguments
    )
  ),
  "A used model value did not change the checkpoint signature"
)

signature_root <- tempfile("signature-inputs-")
dir.create(signature_root)
signature_paths <- file.path(
  signature_root,
  c(
    "poststrat", "previous", "turnout", "results", "degurba",
    "CensoPersonas_2021.parquet", "vote", "candidates", "seats"
  )
)
invisible(file.create(signature_paths))
signature_config <- config
signature_config$paths$raw_root <- signature_root
signature_config$resolved_paths$raw_poststrat <- signature_paths[1]
signature_config$resolved_paths$raw_previous <- signature_paths[2]
signature_config$resolved_paths$raw_turnout <- signature_paths[3]
signature_config$resolved_paths$raw_results <- signature_paths[4]
signature_config$resolved_paths$raw_degurba <- signature_paths[5]
signature_config$resolved_paths$raw_vote <- signature_paths[7]
signature_config$resolved_paths$raw_candidates <- signature_paths[8]
signature_config$resolved_paths$raw_seats <- signature_paths[9]
frame_signature <- mrp_signature(signature_config, "frame")
full_signature_config <- signature_config
full_signature_config$run$inference <- "full"
full_signature_config$inference <- mrp_inference_profile("full")
frame_manifest <- mrp_signature_manifest(
  full_signature_config, "frame"
)$manifest
estimate_manifest <- mrp_signature_manifest(
  full_signature_config, "estimate",
  extra = list(
    frame = frame_signature,
    variant = list(weight_scheme = "sample", geo_effect = "ccaa")
  )
)$manifest
assert(
  !("vote" %in% names(frame_manifest$inference)) &&
    identical(
      estimate_manifest$inference$vote$control$adapt_delta,
      0.9
    ),
  "Vote sampler controls leaked into the frame signature"
)
temporal_signature_config <- signature_config
temporal_signature_config$run$temporal <- list(
  unit = "day", structure = "linear_varying"
)
signature_extra <- list(
  frame = frame_signature,
  variant = list(weight_scheme = "sample", geo_effect = "ccaa")
)
assert(
  identical(
    frame_signature,
    mrp_signature(temporal_signature_config, "frame")
  ) &&
    !identical(
      mrp_signature(signature_config, "estimate", extra = signature_extra),
      mrp_signature(
        temporal_signature_config, "estimate", extra = signature_extra
      )
    ),
  "Temporal configuration did not isolate invalidation to estimation"
)
sampling_signature_config <- signature_config
sampling_signature_config$run$vote_sampling <- list(
  adapt_delta = 0.97, max_treedepth = 12L, chains = 2L
)
sampling_signature_config$inference$vote_sampling <-
  sampling_signature_config$run$vote_sampling
sampling_fit_spec <- mrp_model_fit_spec(
  "vote", sampling_signature_config$inference
)
assert(
  identical(
    frame_signature,
    mrp_signature(sampling_signature_config, "frame")
  ) &&
    !identical(
      mrp_signature(signature_config, "estimate", extra = signature_extra),
      mrp_signature(
        sampling_signature_config, "estimate", extra = signature_extra
      )
    ) &&
    sampling_fit_spec$chains == 2L &&
    identical(
      sampling_fit_spec$control,
      list(adapt_delta = 0.97, max_treedepth = 12L)
    ),
  "Vote sampling controls did not isolate invalidation to estimation"
)
geography_signature_config <- signature_config
geography_signature_config$domain$vote_geography$random_min_ccaa <- 4L
assert(
  identical(
    frame_signature,
    mrp_signature(geography_signature_config, "frame")
  ) &&
    !identical(
      mrp_signature(
        signature_config, "estimate", extra = signature_extra
      ),
      mrp_signature(
        geography_signature_config, "estimate", extra = signature_extra
      )
    ),
  "Vote geography must invalidate only the estimate signature"
)
national_config <- signature_config
national_config$run$national_calibration$strength <- 0.9
national_config$run$national_calibration$otbl_strength <- 1
assert(
  identical(
    frame_signature,
    mrp_signature(national_config, "frame")
  ),
  "National vote calibration invalidated the frame cache"
)
assert(
  identical(
    mrp_signature(signature_config, "estimate", extra = list(frame = frame_signature)),
    mrp_signature(national_config, "estimate", extra = list(frame = frame_signature))
  ) && identical(
    mrp_model_fit_spec("vote", signature_config$inference),
    mrp_model_fit_spec("vote", national_config$inference)
  ),
  "National intensities changed the vote cache signature or fit specification"
)
turnout_config <- signature_config
turnout_config$run$calibration$turnout <- 0.65
assert(
  !identical(
    frame_signature,
    mrp_signature(turnout_config, "frame")
  ),
  "Turnout calibration did not invalidate the frame cache"
)
unlink(signature_root, recursive = TRUE)

assert(
  identical(as.character(mrp_age_group(c(18, 24, 25, 65))), c("18-24", "18-24", "25-34", "65+")),
  "Age adapter changed"
)
assert(
  identical(
    mrp_group_recode(c("SP", "SU", "XX"), config$schemas$poststrat$epa$estudios_recode),
    c("2", "3", "1")
  ),
  "EPA recoding changed"
)

assert(
  identical(
    mrp_vote_recode("En blanco", config$schemas$vote$voto_recode),
    "OTBL"
  ),
  "Blank vote must remain a valid OTBL outcome"
)
assert(
  identical(
    mrp_vote_recode(
      c("SUMAR", "VOX", "No sabe todavía"),
      config$schemas$vote$voto_recode,
      auxiliary = TRUE
    ),
    c("Sumar", "Vox", "Indecisos")
  ),
  "Auxiliary vote normalization changed"
)
profile_yamls <- c(
  "config/mrp.yml",
  "config/mrp_tracking_29N.yml",
  "config/mrp_flash_rdl.yml"
)
canonical_eh_bildu <- mrp_normalize_electoral_party(
  c("EH Bildu", "EH BILDU")
)
assert(
  all(vapply(
    profile_yamls,
    function(path) {
      specification <- yaml::read_yaml(path)
      all(mrp_flat_recode(
        canonical_eh_bildu,
        specification$schemas$previous$results_recode
      ) == "Nac_izq")
    },
    logical(1)
  )),
  "Canonical EH Bildu results must be recoded as Nac_izq in every profile"
)
previous_results_file <- tempfile(fileext = ".csv")
readr::write_csv(
  data.frame(
    codigo_provincia = c("01", "01"),
    votos_blancos = c(10, 10),
    votos_nulos = c(5, 5),
    abstenciones = c(100, 100),
    partido_recode = c("EH Bildu", "PP"),
    votos = c(100, 200)
  ),
  previous_results_file
)
previous_targets <- mrp_previous_targets(
  previous_results_file,
  config$schemas$previous,
  c("PP", "Nac_izq", "OTBL", "ABNL")
)
assert(
  previous_targets$votos[
    previous_targets$recuerdo == "Nac_izq"
  ] == 100 &&
    previous_targets$votos[
      previous_targets$recuerdo == "OTBL"
    ] == 10,
  "Official EH Bildu votes fell through to the default OTBL category"
)
audit_base <- data.frame(
  provincia = factor(rep("01", 4)),
  recuerdo = factor(
    c("PP", "Nac_izq", "OTBL", "ABNL"),
    levels = c("PP", "Nac_izq", "OTBL", "ABNL")
  )
)
audit_counts <- matrix(
  rep(c(200, 100, 10, 105), each = 2),
  nrow = 2
)
audit_frame <- list(frame = list(
  base = audit_base,
  scenarios = list(official = list(n_imputado = audit_counts))
))
recall_audit <- mrp_recall_target_audit(
  audit_frame,
  previous_results_file,
  config$schemas$previous
)
assert(
  identical(recall_audit$status, "PASS") && recall_audit$maximum < 1e-12,
  "Official recall target audit missed matching margins"
)
bad_audit_frame <- audit_frame
bad_audit_frame$frame$scenarios$official$n_imputado[, ] <- matrix(
  rep(c(200, 0, 110, 105), each = 2),
  nrow = 2
)
assert_error(
  mrp_recall_target_audit(
    bad_audit_frame,
    previous_results_file,
    config$schemas$previous
  ),
  "Official recall target audit failed"
)
unlink(previous_results_file)
schema_2023 <- yaml::read_yaml(
  "data-raw/validation-data/2023/survey-spec-vote.yaml"
)
assert(
  identical(
    mrp_flat_recode("MP", schema_2023$recuerdo_recode),
    "UP_MP"
  ),
  "2019 MP recall must be recoded as UP_MP"
)
assert(
  identical(config$schemas$vote$participation$type, "ordinal_scale") &&
    identical(config$schemas$previous$participation$type, "binary_codes"),
  "Participation source types changed"
)

turnout_probability <- matrix(c(.2, .8, .3, .7), nrow = 2, byrow = TRUE)
turnout_population <- matrix(c(10, 30, 20, 20), nrow = 2, byrow = TRUE)
turnout <- mrp_calibrate_turnout_logit(
  turnout_probability, turnout_population, .6
)
assert(
  all(turnout$probability > 0 & turnout$probability < 1),
  "Turnout calibration left probability bounds"
)
turnout_rate <- rowSums(turnout$voters) / rowSums(turnout_population)
assert(
  max(abs(turnout_rate - .6)) < 1e-8,
  "Turnout calibration missed its national target"
)

turnout_analysis_base <- data.frame(
  ccaa = c("A", "A", "A", "A", "B", "B"),
  provincia = factor(
    c("01", "01", "02", "02", "03", "03"),
    levels = c("01", "02", "03")
  ),
  sexo = factor(
    c("Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer")
  ),
  edad_grupo = factor(
    c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  ),
  estudios = factor(c("1", "2", "3", "1", "2", "3")),
  sitlab2 = factor(c("1", "2", "3", "4", "5", "1")),
  recuerdo = factor(
    c("PP", "PSOE", "Vox", "ABNL", "PP", "ABNL"),
    levels = c("PP", "PSOE", "Vox", "ABNL")
  ),
  participacion_prev = rep(c(.60, .70, .80), each = 2),
  margen_der_izq_prev = rep(c(-.20, 0, .20), each = 2),
  pct_alquiler = rep(c(.10, .20, .35), each = 2),
  pct_mayores65 = rep(c(.30, .20, .10), each = 2),
  degurba1 = rep(c(.20, .50, .80), each = 2),
  degurba2 = rep(c(.60, .30, .10), each = 2),
  degurba3 = .20
)
turnout_analysis_probability <- matrix(
  c(
    .20, .35, .50, .65, .80, .90,
    .25, .40, .55, .70, .82, .92
  ),
  nrow = 2, byrow = TRUE
)
turnout_analysis_model_population <- matrix(
  c(
    10, 20, 30, 40, 50, 60,
    20, 20, 30, 30, 50, 60
  ),
  nrow = 2, byrow = TRUE
)
turnout_analysis_official_population <- matrix(
  c(
    15, 15, 35, 35, 55, 55,
    10, 30, 25, 35, 45, 65
  ),
  nrow = 2, byrow = TRUE
)
turnout_analysis_model_calibration <- mrp_calibrate_turnout_logit(
  turnout_analysis_probability,
  turnout_analysis_model_population,
  .60
)
turnout_analysis_official_calibration <- mrp_calibrate_turnout_logit(
  turnout_analysis_probability,
  turnout_analysis_official_population,
  .60
)
turnout_analysis_frame <- list(
  signature = "synthetic-turnout-frame",
  frame = list(
    base = turnout_analysis_base,
    n_imputado = (
      turnout_analysis_model_population +
        turnout_analysis_official_population
    ) / 2,
    p_vota = turnout_analysis_probability,
    n_votantes_esperados = (
      turnout_analysis_model_calibration$voters +
        turnout_analysis_official_calibration$voters
    ) / 2,
    scenarios = list(
      model = list(
        n_imputado = turnout_analysis_model_population,
        p_vota = turnout_analysis_probability,
        p_vota_cal = turnout_analysis_model_calibration$probability,
        n_votantes_esperados = turnout_analysis_model_calibration$voters,
        calibration_delta = turnout_analysis_model_calibration$delta
      ),
      official = list(
        n_imputado = turnout_analysis_official_population,
        p_vota = turnout_analysis_probability,
        p_vota_cal = turnout_analysis_official_calibration$probability,
        n_votantes_esperados = turnout_analysis_official_calibration$voters,
        calibration_delta = turnout_analysis_official_calibration$delta
      )
    ),
    recall_weight = .50
  )
)
turnout_analysis <- mrp_build_turnout_analysis(turnout_analysis_frame)
assert(
  identical(
    names(turnout_analysis$draws),
    c(
      "draw", "variable", "nivel", "escenario", "etapa",
      "poblacion", "votantes_esperados", "participacion",
      "peso_poblacion", "peso_votantes"
    )
  ),
  "Turnout draw aggregate schema changed"
)
mrp_assert_turnout_analysis_target(turnout_analysis, .60)
turnout_male_draw1 <- turnout_analysis$draws |>
  dplyr::filter(
    draw == 1L,
    variable == "sexo",
    nivel == "Hombre",
    escenario == "final",
    etapa == "raw"
  ) |>
  dplyr::pull(participacion)
turnout_final_population <- (
  turnout_analysis_model_population +
    turnout_analysis_official_population
) / 2
turnout_male_expected <- sum(
  turnout_final_population[1, turnout_analysis_base$sexo == "Hombre"] *
    turnout_analysis_probability[1, turnout_analysis_base$sexo == "Hombre"]
) / sum(
  turnout_final_population[1, turnout_analysis_base$sexo == "Hombre"]
)
assert(
  abs(turnout_male_draw1 - turnout_male_expected) < 1e-12,
  "Turnout analysis did not use population-weighted raw probabilities"
)
assert(
  all(turnout_analysis$summary$participacion_media >= 0 &
    turnout_analysis$summary$participacion_media <= 1),
  "Turnout summaries left the probability scale"
)
assert(
  all(is.na(
    turnout_analysis$correlations$correlacion[
      turnout_analysis$correlations$predictor == "degurba3"
    ]
  )),
  "Constant provincial predictors must have missing correlations"
)

turnout_analysis_with_tamuni <- turnout_analysis_frame
turnout_analysis_with_tamuni$frame$base$tamuni <- factor(
  c("pequeño", "grande", "pequeño", "grande", "pequeño", "grande")
)
turnout_tamuni <- mrp_build_turnout_analysis(turnout_analysis_with_tamuni)
assert(
  "tamuni" %in% turnout_tamuni$draws$variable,
  "Optional tamuni margins were not included in turnout analysis"
)
turnout_bad_dimensions <- turnout_analysis_frame
turnout_bad_dimensions$frame$p_vota <-
  turnout_bad_dimensions$frame$p_vota[, -1, drop = FALSE]
assert_error(
  mrp_build_turnout_analysis(turnout_bad_dimensions),
  "identical dimensions"
)
turnout_bad_calibrated_dimensions <- turnout_analysis_frame
turnout_bad_calibrated_dimensions$frame$scenarios$model$p_vota_cal <-
  turnout_bad_calibrated_dimensions$frame$scenarios$model$p_vota_cal[
    , -1,
    drop = FALSE
  ]
assert_error(
  mrp_build_turnout_analysis(turnout_bad_calibrated_dimensions),
  "identical dimensions"
)

turnout_output_root <- tempfile("turnout-analysis-")
dir.create(turnout_output_root)
turnout_test_config <- config
turnout_test_config$run$poststrat_source <- "test"
turnout_test_config$run$calibration$turnout <- .60
turnout_test_config$resolved_paths$diagnostics_root <- file.path(
  turnout_output_root, "diagnostics"
)
turnout_outputs <- mrp_write_turnout_analysis(
  turnout_analysis_frame,
  turnout_test_config,
  turnout_analysis_frame$signature
)
assert(
  all(file.exists(unlist(turnout_outputs, use.names = FALSE))),
  "Turnout analysis did not create every declared artifact"
)
turnout_summary_csv <- readr::read_csv(
  turnout_outputs$summary_csv,
  show_col_types = FALSE
)
assert(
  identical(
    names(turnout_summary_csv),
    c(
      "variable", "nivel", "escenario", "etapa",
      "participacion_media", "participacion_p20",
      "participacion_p50", "participacion_p80",
      "peso_poblacion_media", "peso_votantes_media",
      "delta_calibracion_pp"
    )
  ),
  "Turnout summary CSV schema changed"
)
turnout_bundle <- readRDS(turnout_outputs$draws_rds)
assert(
  all(c(
    "metadata", "draws", "correlations", "summary",
    "calibration_contributions", "sensitivity"
  ) %in%
    names(turnout_bundle)) &&
    identical(turnout_bundle$metadata$schema_version, 3L),
  "Turnout analysis RDS metadata is incomplete"
)
turnout_report <- readLines(turnout_outputs$markdown, warn = FALSE)
assert(
  any(grepl("participacion_recuerdo.png", turnout_report, fixed = TRUE)) &&
    any(grepl("participacion_calibracion.png", turnout_report, fixed = TRUE)) &&
    any(grepl("Sensibilidad", turnout_report, fixed = TRUE)) &&
    any(grepl("causales", turnout_report, fixed = TRUE)),
  "Turnout Markdown report is missing figures or methodological cautions"
)
unlink(turnout_output_root, recursive = TRUE)

scenario_model <- data.frame(
  draw = c(1L, 1L), provincia = c("01", "01"),
  partido = c("PP", "PSOE"), votos = c(60, 40), voto_pct = c(.6, .4)
)
scenario_official <- scenario_model
scenario_official$votos <- c(40, 60)
scenario_official$voto_pct <- c(.4, .6)
assert(
  identical(
    mrp_blend_recall_votes(scenario_model, scenario_official, 0)$votos,
    scenario_model$votos
  ),
  "Recall blend weight zero changed modeled votes"
)
assert(
  identical(
    mrp_blend_recall_votes(scenario_model, scenario_official, 1)$votos,
    scenario_official$votos
  ),
  "Recall blend weight one missed official votes"
)
assert(
  identical(
    mrp_blend_recall_votes(scenario_model, scenario_official, .5)$votos,
    c(50, 50)
  ),
  "Recall blend did not mix provincial votes before allocation"
)

report_sims <- tidyr::expand_grid(
  draw = 1:2,
  provincia = c("01", "02"),
  partido = c("PP", "PSOE", "OTBL")
) |>
  dplyr::mutate(
    votos = dplyr::case_when(
      partido == "OTBL" ~ 0,
      provincia == "01" & partido == "PP" ~ ifelse(draw == 1, 60, 55),
      provincia == "01" ~ ifelse(draw == 1, 40, 45),
      provincia == "02" & partido == "PP" ~ ifelse(draw == 1, 40, 45),
      TRUE ~ ifelse(draw == 1, 60, 55)
    ),
    voto_pct = votos / sum(votos),
    seats = as.integer(
      (provincia == "01" & partido == "PP") |
        (provincia == "02" & partido == "PSOE")
    ),
    .by = c(draw, provincia)
  )
report_national <- mrp_national_uncertainty(report_sims)
assert(
  nrow(report_national$summary) == 3L &&
    all(report_national$summary$prob_escanos[
      report_national$summary$partido == "OTBL"
    ] == 0),
  "National uncertainty report changed"
)
assert(
  identical(unname(MRP_REPORT_INTERVAL), c(.2, .8)) &&
    identical(names(MRP_REPORT_PARTY_COLORS), MRP_REPORT_FOCUS_PARTIES),
  "Report interval or national-party palette changed"
)
demographic_epred <- array(
  c(
    .8, .7, .6, .5, .2, .3, .4, .5,
    .2, .3, .4, .5, .8, .7, .6, .5
  ),
  dim = c(2, 4, 2),
  dimnames = list(draw = 1:2, cell = 1:4, partido = c("PP", "PSOE"))
)
demographic_frame <- data.frame(
  sexo = c("M", "M", "F", "F"),
  edad_grupo = c("J", "A", "J", "A")
)
demographic_weights <- matrix(1, nrow = 2, ncol = 4)
demographic_draws <- mrp_aggregate_epred_by_groups(
  demographic_epred, demographic_frame, demographic_weights,
  c("sexo", "edad_grupo")
)
assert(
  nrow(demographic_draws) == 16L &&
    abs(demographic_draws$voto_pct[
      demographic_draws$draw == 1L &
        demographic_draws$variable == "sexo" &
        demographic_draws$nivel == "M" &
        demographic_draws$partido == "PP"
    ] - .7) < 1e-12,
  "Demographic epred aggregation changed"
)
demographic_national <- demographic_draws |>
  dplyr::filter(variable == "sexo") |>
  dplyr::summarise(votos = sum(votos), .by = c(draw, partido)) |>
  dplyr::mutate(voto_pct = votos / sum(votos), .by = draw)
demographic_strength <- mrp_demographic_strength(
  demographic_draws |> dplyr::filter(variable == "sexo"),
  demographic_national
)
assert(
  abs(demographic_strength$sobreindice_medio[
    demographic_strength$nivel == "M" &
      demographic_strength$partido == "PP"
  ] - .15) < 1e-12,
  "Demographic strength over-index changed"
)
demographic_delta <- array(
  0,
  dim = c(2, 2, 2),
  dimnames = list(NULL, c("inicio", "actual"), c("PP", "PSOE"))
)
demographic_delta[, 1, "PP"] <- -log(2)
demographic_temporal <- mrp_demographic_temporal(
  demographic_draws |> dplyr::filter(variable == "sexo"),
  list(
    periods = c("inicio", "actual"),
    parties = c("PP", "PSOE"),
    delta = demographic_delta
  )
)
assert(
  nrow(demographic_temporal$summary) == 8L &&
    all(demographic_temporal$changes$prob_sube[
      demographic_temporal$changes$partido == "PP"
    ] == 1) &&
    all(demographic_temporal$changes$prob_sube[
      demographic_temporal$changes$partido == "PSOE"
    ] == 0),
  "Demographic temporal decomposition changed"
)
demographic_output_root <- file.path(tempdir(), "mrp-demographic-report-test")
demographic_paths <- mrp_vote_report_paths(list(
  diagnostics_root = demographic_output_root
))
mrp_write_demographic_report(
  list(
    strength = demographic_strength,
    evolution = demographic_temporal$summary,
    changes = demographic_temporal$changes
  ),
  list(
    periods = c("inicio", "actual"),
    first_period = "inicio",
    last_period = "actual"
  ),
  demographic_paths
)
assert(
  all(file.exists(c(
    demographic_paths$demographic_report,
    demographic_paths$demographic_strength_csv,
    demographic_paths$demographic_change_csv,
    demographic_paths$demographic_evolution_csv,
    demographic_paths$demographic_strength_plot,
    demographic_paths$demographic_change_plot,
    demographic_paths$demographic_evolution_plot
  ))),
  "Demographic report did not write every artifact"
)
unlink(demographic_output_root, recursive = TRUE)
report_seats <- mrp_last_seat_analysis(report_sims, threshold = .03)
assert(
  nrow(report_seats$summary) == 2L &&
    all(report_seats$summary$prob_asignacion_modal == 1) &&
    all(report_seats$summary$defensor_modal !=
          report_seats$summary$aspirante_modal) &&
    all(report_seats$draws$margen_relativo >= -1e-10) &&
    all(abs(
      report_seats$role_probability |>
        dplyr::summarise(
          total = sum(prob_involucrado), .by = provincia
        ) |>
        dplyr::pull(total) - 2
    ) < 1e-12),
  "Last-seat stability report changed"
)
threshold_sims <- data.frame(
  draw = 1L,
  provincia = "01",
  partido = c("PP", "PSOE", "OTBL"),
  votos = c(97, 3, 10),
  voto_pct = c(97, 3, 10) / 110,
  seats = c(1L, 0L, 0L)
)
threshold_battle <- mrp_last_seat_analysis(threshold_sims, threshold = .03)
assert(
  is.na(threshold_battle$draws$aspirante[[1]]),
  "Last-seat analysis must use the same valid-vote denominator as allocation"
)
report_blocks <- mrp_blocks_analysis(
  report_national$draws,
  list(right_parties = "PP", left_parties = "PSOE")
)
assert(
  nrow(report_blocks$summary) == 2L &&
    all(report_blocks$summary$escanos_media == 1),
  "Block report must aggregate seats draw by draw"
)
report_recall <- mrp_recall_analysis(
  list(
    weight = .75,
    national = data.frame(
      escenario = rep(c("model", "official"), each = 2),
      partido = rep(c("PP", "PSOE"), 2),
      voto_pct = c(.4, .6, .35, .65)
    ),
    contributions = data.frame(
      escenario = rep(c("model", "official"), each = 4),
      recuerdo = rep(rep(c("PP", "PSOE"), each = 2), 2),
      partido = rep(c("PP", "PSOE"), 4),
      contribucion_pct = c(.3, .2, .1, .4, .2, .3, .2, .3)
    )
  ),
  list(enabled = FALSE)
)
report_recall_mix <- report_recall$contributions |>
  dplyr::filter(
    escenario == "mix", recuerdo == "PP", partido == "PP"
  ) |>
  dplyr::pull(contribucion_pct)
assert(
  length(report_recall_mix) == 1L &&
    abs(report_recall_mix - .225) < 1e-12 &&
    nrow(report_recall$national) == 2L,
  "Recall report did not blend model and official contributions"
)
report_paths <- mrp_vote_report_paths(list(
  diagnostics_root = "diagnostics",
  recall_report = "informe_recuerdo.md"
))
assert(
  length(grep("informe_.*\\.md$", unlist(report_paths))) == 9L,
  "Vote report suite no longer declares nine dedicated Markdown files"
)
vote_output_root <- tempfile("vote-reporting-")
dir.create(vote_output_root)
vote_test_paths <- mrp_vote_report_paths(list(
  diagnostics_root = file.path(vote_output_root, "diagnostics"),
  recall_report = file.path(vote_output_root, "informe_recuerdo.md")
))
dir.create(vote_test_paths$root, recursive = TRUE)
mrp_write_estimation_report(report_national, vote_test_paths)
mrp_write_last_seat_report(report_seats, vote_test_paths)
mrp_write_blocks_report(report_blocks, vote_test_paths)
estimation_report_lines <- readLines(
  vote_test_paths$estimation_report, warn = FALSE
)
assert(
  all(file.exists(c(
    vote_test_paths$estimation_report,
    vote_test_paths$seat_report,
    vote_test_paths$blocks_report,
    vote_test_paths$national_vote_plot,
    vote_test_paths$seat_battle_party_plot,
    vote_test_paths$seat_battle_province_plot
  ))),
  "Core vote reports did not create their Markdown and figures"
)
assert(
  any(grepl("P20", estimation_report_lines, fixed = TRUE)) &&
    !any(grepl("| OTBL |", estimation_report_lines, fixed = TRUE)),
  "Vote report must use P20-P80 and focus its display on national parties"
)
unlink(vote_output_root, recursive = TRUE)

target_file <- tempfile(fileext = ".csv")
readr::write_csv(
  data.frame(
    draw = c(1L, 1L),
    partido = c("PP", "PSOE"),
    voto_pct = c(.4, .35)
  ),
  target_file
)
calibration_votes <- data.frame(
  draw = rep(1L, 6),
  provincia = rep(c("01", "02"), each = 3),
  partido = rep(c("PP", "PSOE", "OTBL"), 2),
  votos = c(60, 30, 10, 0, 70, 30)
)
calibration_votes$voto_pct <- ave(
  calibration_votes$votos, calibration_votes$provincia,
  FUN = function(x) x / sum(x)
)
calibrated <- mrp_calibrate_national_votes(
  calibration_votes,
  list(enabled = TRUE, draws_path = target_file, strength = 1)
)
assert_error(
  mrp_calibrate_national_votes(
    calibration_votes,
    list(
      enabled = TRUE,
      draws_path = paste0(target_file, "-missing"),
      strength = 1
    )
  ),
  "not found"
)
assert_error(
  mrp_calibrate_national_votes(
    calibration_votes,
    list(
      enabled = TRUE,
      draws_path = paste0(target_file, "-missing"),
      strength = 0
    )
  ),
  "not found"
)
national <- aggregate(
  calibrated$votes$votos,
  list(partido = calibrated$votes$partido),
  sum
)
national$pct <- national$x / sum(national$x)
assert(
  abs(national$pct[national$partido == "PP"] - .4) < 1e-8 &&
    abs(national$pct[national$partido == "PSOE"] - .35) < 1e-8,
  "National calibration missed party targets"
)
province_before <- aggregate(
  calibration_votes$votos,
  list(provincia = calibration_votes$provincia),
  sum
)
province_after <- aggregate(
  calibrated$votes$votos,
  list(provincia = calibrated$votes$provincia),
  sum
)
assert(
  max(abs(province_before$x - province_after$x)) < 1e-8,
  "National calibration changed provincial totals"
)
assert(
  calibrated$votes$votos[
    calibrated$votes$provincia == "02" & calibrated$votes$partido == "PP"
  ] == 0,
  "National calibration created mass in a structural zero"
)
# Legacy calls, explicit NULL and equal intensities must preserve vote outputs.
for (s in c(0, .2, 1)) {
  specification <- list(enabled = TRUE, draws_path = target_file, strength = s)
  legacy <- mrp_calibrate_national_votes(calibration_votes, specification)
  if (s == .2) {
    assert(max(abs(legacy$votes$votos - c(
      9.76048439174084, 64.035617344956, 26.2038982633031,
      32.3824330252415, 0, 67.6175669747585
    ))) < 1e-8, "Calibration changed the pre-reinforcement regression fixture")
  }
  for (u in list(NULL, s)) {
    specification["otbl_strength"] <- list(u)
    same <- mrp_calibrate_national_votes(calibration_votes, specification)
    assert(
      identical(legacy$votes, same$votes),
      "Default or equal OTBL intensity changed votes"
    )
  }
  if (s == 0) {
    assert(
      identical(legacy$votes, calibration_votes) &&
        legacy$diagnostics$status == "NOOP",
      "Zero intensities did not return the original prediction"
    )
  }
}
assert_error(
  mrp_calibrate_national_votes(calibration_votes, list(
    enabled = TRUE, draws_path = target_file, strength = .2, otbl_strength = .8
  )),
  "explicit OTBL target in every draw"
)
assert_error(
  mrp_calibrate_national_votes(
    calibration_votes[calibration_votes$partido != "OTBL", ],
    list(enabled = TRUE, draws_path = target_file, strength = .2, otbl_strength = .8)
  ),
  "OTBL to be modeled"
)
for (u in list(-1, .1, 2, Inf, NaN, NA_real_, "0.8", c(.4, .8))) {
  assert_error(mrp_calibrate_national_votes(calibration_votes, list(
    enabled = TRUE, draws_path = target_file, strength = .2, otbl_strength = u
  )), "otbl_strength")
}
disabled <- mrp_calibrate_national_votes(calibration_votes, list(
  enabled = FALSE, draws_path = paste0(target_file, "-absent"),
  strength = .2, otbl_strength = 1
))
assert(
  identical(disabled$votes, calibration_votes) &&
    is.null(disabled$diagnostics$target_hash),
  "Disabled calibration read targets or changed votes"
)

# Multiple target draws, two residual parties and a provincial structural zero.
otbl_targets <- data.frame(
  draw = rep(c(9L, 4L), each = 3),
  partido = rep(c("PP", "PSOE", "OTBL"), 2),
  voto_pct = c(.35, .45, .05, .3, .4, .1)
)
readr::write_csv(otbl_targets, target_file)
otbl_seed <- data.frame(
  provincia = rep(c("01", "02"), each = 5),
  partido = rep(c("PP", "PSOE", "OTBL", "SALF", "BNG"), 2),
  votos = c(50, 20, 20, 7.5, 2.5, 0, 60, 20, 15, 5)
)
otbl_votes <- dplyr::bind_rows(lapply(c(3L, 1L, 2L), function(draw) {
  dplyr::mutate(otbl_seed, draw = draw, voto_pct = votos / 100)
}))
otbl_base <- mrp_calibrate_national_votes(otbl_votes, list(
  enabled = TRUE, draws_path = target_file, strength = .2
))
previous_distance <- Inf
for (u in c(.2, .5, .8, 1)) {
  reinforced <- mrp_calibrate_national_votes(otbl_votes, list(
    enabled = TRUE, draws_path = target_file, strength = .2, otbl_strength = u
  ))
  margins <- reinforced$diagnostics$margins
  assert(
    identical(margins$base_pct, otbl_base$diagnostics$margins$base_pct),
    "OTBL intensity changed the general calibration stage"
  )
  otbl_margins <- margins[margins$category == "OTBL", ]
  if (u == .8) {
    assert(
      abs(otbl_margins$final_pct[1] - .07622516628430874) < 1e-12,
      "Reinforcement did not scale its additional intensity relative to strength"
    )
  }
  distance <- abs(otbl_margins$achieved_pct - otbl_margins$external_pct)
  assert(
    all(distance <= previous_distance + 1e-8),
    "OTBL did not approach its external target monotonically"
  )
  previous_distance <- distance
  assert(
    max(abs(margins$final_pct - margins$achieved_pct)) < 1e-8,
    "Raking missed the reinforced national margins"
  )
  for (draw in unique(margins$draw)) {
    other <- margins[margins$draw == draw & margins$category != "OTBL", ]
    assert(
      max(abs(other$final_pct / sum(other$final_pct) -
        other$base_pct / sum(other$base_pct))) < 1e-12,
      "OTBL reinforcement changed the composition of other categories"
    )
  }
  totals <- reinforced$votes |>
    dplyr::summarise(votos = sum(votos), pct = sum(voto_pct), .by = c(draw, provincia))
  assert(
    all(abs(totals$votos - 100) < 1e-8) && all(abs(totals$pct - 1) < 1e-12) &&
      all(is.finite(reinforced$votes$voto_pct)) &&
      all(reinforced$votes$votos >= 0) &&
      all(reinforced$votes$votos[reinforced$votes$provincia == "02" &
        reinforced$votes$partido == "PP"] == 0),
    "Reinforcement violated provincial totals, normalization or support"
  )
}
assert(max(previous_distance) < 1e-8, "Full OTBL intensity missed its target")
assert(
  identical(otbl_margins$target_draw, c("9", "4", "9")),
  "Calibration changed positional target-draw recycling"
)
assert(
  reinforced$diagnostics$method == "otbl_reinforcement_v1" &&
    reinforced$diagnostics$strength == .2 && reinforced$diagnostics$otbl_strength == 1 &&
    identical(reinforced$diagnostics$target_hash, digest::digest(file = target_file, algo = "sha256")) &&
    all(c("initial_pct", "external_pct", "base_pct", "final_pct", "achieved_pct") %in%
      names(margins)) && nrow(margins) == 12L &&
    nrow(reinforced$diagnostics$achieved) == 15L,
  "Calibration diagnostics are incomplete"
)
# This fixture has a 3:1 SALF/BNG split in both provinces, also preserved nationally.
residual_achieved <- reinforced$diagnostics$achieved
assert(
  all(abs(residual_achieved$achieved_pct[residual_achieved$partido == "SALF"] /
    residual_achieved$achieved_pct[residual_achieved$partido == "BNG"] - 3) < 1e-10),
  "Residual party proportions changed"
)
seat_file <- tempfile(fileext = ".csv")
readr::write_csv(data.frame(provincia = c("01", "02"), nseats = c(175L, 175L)), seat_file)
allocated <- mrp_allocate_votes(reinforced$votes, seat_file, list(
  district_threshold = .03, expected_seats = 350L
))
assert(
  all(tapply(allocated$seats, allocated$draw, sum) == 350L) &&
    all(allocated$seats[allocated$partido == "OTBL"] == 0L),
  "Reinforced votes violated seat allocation invariants"
)
unlink(seat_file)

only_otbl <- mrp_calibrate_national_votes(otbl_votes, list(
  enabled = TRUE, draws_path = target_file, strength = 0, otbl_strength = 1
))
only_margins <- only_otbl$diagnostics$margins
assert(
  only_otbl$diagnostics$status == "PASS" &&
    max(abs(only_margins$base_pct - only_margins$initial_pct)) < 1e-12 &&
    max(abs(only_margins$achieved_pct[only_margins$category == "OTBL"] -
      only_margins$external_pct[only_margins$category == "OTBL"])) < 1e-8,
  "General intensity zero skipped the OTBL-only calibration"
)

# Missing OTBL in only one draw must raise the specific reinforcement error.
readr::write_csv(otbl_targets[-6, ], target_file)
assert_error(mrp_calibrate_national_votes(otbl_votes, list(
  enabled = TRUE, draws_path = target_file, strength = .2, otbl_strength = 1
)), "explicit OTBL target in every draw")

# Known interpolation example, upward correction, boundary targets and no support.
example_targets <- data.frame(
  target_draw = "1", partido = c("PP", "PSOE", "OTBL"), voto_pct = c(.5, .47, .03)
)
example_current <- c(PP = .5, PSOE = .38, OTBL = .12)
example <- mrp_national_target_for_draw(
  example_targets, "1", names(example_current), example_current, 0, .8
)
assert(
  abs(example[["OTBL"]] - .048) < 1e-12 &&
    abs(example[["PP"]] / example[["PSOE"]] - .5 / .38) < 1e-12,
  "OTBL-only interpolation missed its analytical target"
)
example_targets$voto_pct <- c(.4, .3, .3)
upward <- vapply(c(0, .5, 1), function(u) {
  mrp_national_target_for_draw(
    example_targets, "1", names(example_current), example_current, 0, u
  )[["OTBL"]]
}, numeric(1))
assert(
  all(diff(upward) > 0) && tail(upward, 1) == .3,
  "Reinforcement did not approach an upward OTBL target"
)
for (endpoint in c(0, 1)) {
  readr::write_csv(data.frame(
    draw = 1L, partido = c("PP", "PSOE", "OTBL"),
    voto_pct = c((1 - endpoint) * .4, (1 - endpoint) * .6, endpoint)
  ), target_file)
  boundary <- mrp_calibrate_national_votes(calibration_votes, list(
    enabled = TRUE, draws_path = target_file, strength = 0, otbl_strength = 1
  ))
  assert(
    all(is.finite(boundary$votes$voto_pct)) &&
      abs(sum(boundary$votes$votos[boundary$votes$partido == "OTBL"]) / 200 - endpoint) < 1e-8,
    "Boundary OTBL target was not reached"
  )
}
empty_complement <- c(PP = 0, PSOE = 0, OTBL = 1)
assert_error(mrp_national_target_for_draw(
  example_targets, "1", names(empty_complement), empty_complement, 0, 1
), "empty complement")
example_targets$voto_pct <- c(0, 0, 1)
assert(identical(mrp_national_target_for_draw(
  example_targets, "1", names(empty_complement), empty_complement, 0, 1
), empty_complement), "An unchanged empty complement produced an undefined result")
no_otbl_support <- calibration_votes
no_otbl_support$votos[no_otbl_support$partido == "OTBL"] <- 0
assert_error(mrp_calibrate_national_votes(no_otbl_support, list(
  enabled = TRUE, draws_path = target_file, strength = 0, otbl_strength = 1
)), "no modeled support")
readr::write_csv(data.frame(
  draw = 1L, partido = c("PP", "PSOE", "OTBL"), voto_pct = c(.8, .1, .1)
), target_file)
assert_error(mrp_calibrate_national_votes(calibration_votes, list(
  enabled = TRUE, draws_path = target_file, strength = .99, otbl_strength = 1
)), "did not converge")
unlink(target_file)

observed <- mrp_observed_model_data(
  data.frame(response = factor(c("a", NA, "b")), province = c("01", "42", "03")),
  "response"
)
assert(
  nrow(observed) == 2L && !anyNA(observed$response) && !"42" %in% observed$province,
  "Model checks must exclude rows omitted because the response is missing"
)

set.seed(1)
diagnostic_draws <- posterior::as_draws_array(
  array(
    stats::rnorm(8000),
    dim = c(2000, 4, 1),
    dimnames = list(NULL, NULL, "theta")
  )
)
diagnostics <- mrp_model_diagnostics(
  diagnostic_draws,
  list(name = "full", algorithm = "sampling", require_mcmc = TRUE)
)
assert(
  is.finite(diagnostics$rhat_max) &&
    is.finite(diagnostics$ess_bulk_min) &&
    is.finite(diagnostics$ess_tail_min),
  "MCMC diagnostics did not produce finite summary metrics"
)

cat("All lightweight tests passed.\n")
