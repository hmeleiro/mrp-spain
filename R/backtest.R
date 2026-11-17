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

mrp_backtest_config <- function(config, bundle, variant_name) {
  out <- config
  out$run$poststrat_source <- bundle$poststrat_source
  out$run$recent_studies <- 1L
  poststrat_schema_path <- file.path(
    bundle$root, paste0("poststrat-spec-", bundle$poststrat_source, ".yaml")
  )
  if (file.exists(poststrat_schema_path)) {
    out$schemas$poststrat[[bundle$poststrat_source]] <-
      yaml::read_yaml(poststrat_schema_path)
  }
  profile <- out$run$inference
  processed_base <- file.path(
    config$paths$processed_root, "validation", bundle$election
  )
  models_base <- file.path(
    config$paths$models_root, "validation", bundle$election
  )
  processed <- file.path(processed_base, profile)
  models <- file.path(models_base, profile)
  output <- file.path(config$paths$output_root, "validation", bundle$election, variant_name)
  checkpoint_root <- file.path(models_base, "checkpoints", profile)
  out$resolved_paths <- list(
    raw_root = config$paths$raw_root,
    raw_vote = bundle$vote,
    raw_previous = bundle$previous,
    raw_turnout = bundle$turnout,
    raw_poststrat = bundle$poststrat,
    raw_results = bundle$previous_results,
    raw_candidates = bundle$candidates,
    raw_seats = bundle$seats,
    raw_degurba = file.path(config$paths$raw_root, "EU-27-LAU-2025-NUTS-2024.xlsx"),
    processed_root = processed,
    frame = file.path(processed, "poststrat_frame.rds"),
    survey = file.path(processed, variant_name, "vote_survey.rds"),
    models_root = models_base,
    model_turnout = file.path(models, "turnout.rds"),
    model_recall = file.path(models, "recall.rds"),
    model_vote = file.path(models, variant_name, "vote.rds"),
    checkpoint_turnout = file.path(checkpoint_root, "turnout.rds"),
    checkpoint_recall = file.path(checkpoint_root, "recall.rds"),
    checkpoint_vote = file.path(
      checkpoint_root, variant_name, "vote.rds"
    ),
    output_root = output,
    sims = file.path(output, "sims_est.rds"),
    docx = file.path(output, "estimacion.docx"),
    xlsx = file.path(output, "estimacion.xlsx"),
    availability_report = file.path(output, "informe_candidaturas.md"),
    recall_report = file.path(output, "informe_recuerdo.md"),
    metadata = file.path(output, "run_metadata.rds"),
    diagnostics_root = file.path(output, "diagnostics")
  )
  out
}

mrp_actual_vote <- function(path, parties) {
  raw <- mrp_read_data(path)
  blank <- raw |>
    dplyr::transmute(
      provincia = mrp_normalize_province(codigo_provincia),
      partido = "OTBL", votos = votos_blancos
    ) |>
    dplyr::distinct()
  dplyr::bind_rows(
    raw |>
      dplyr::transmute(
        provincia = mrp_normalize_province(codigo_provincia),
        partido_normalizado = mrp_normalize_electoral_party(partido_recode),
        partido = ifelse(partido_normalizado %in% parties, partido_normalizado, "OTBL"),
        votos = tidyr::replace_na(votos, 0)
      ) |>
      dplyr::select(-partido_normalizado),
    blank
  ) |>
    dplyr::summarise(votos = sum(votos), .by = c(provincia, partido)) |>
    tidyr::complete(
      provincia, partido = parties, fill = list(votos = 0)
    ) |>
    dplyr::mutate(actual_pct = votos / sum(votos), .by = provincia)
}

mrp_actual_seats <- function(path, parties) {
  raw <- mrp_read_data(path)
  raw |>
    dplyr::mutate(
      partido_normalizado = mrp_normalize_electoral_party(partido_recode)
    ) |>
    dplyr::transmute(
      provincia = mrp_normalize_province(codigo_provincia),
      partido = ifelse(partido_normalizado %in% parties, partido_normalizado, "OTBL"),
      seats_actual = tidyr::replace_na(representantes, 0L)
    ) |>
    dplyr::summarise(seats_actual = sum(seats_actual), .by = c(provincia, partido)) |>
    tidyr::complete(
      provincia, partido = parties, fill = list(seats_actual = 0L)
    )
}

mrp_backtest_metrics <- function(sims, target_path, label, ci = 0.95) {
  parties <- unique(as.character(sims$partido))
  actual_vote <- mrp_actual_vote(target_path, parties)
  actual_seats <- mrp_actual_seats(target_path, parties)
  lower <- (1 - ci) / 2
  upper <- 1 - lower
  vote_province <- sims |>
    dplyr::summarise(
      prediction = mean(voto_pct),
      lwr = stats::quantile(voto_pct, lower),
      upr = stats::quantile(voto_pct, upper),
      .by = c(provincia, partido)
    ) |>
    dplyr::left_join(actual_vote, by = c("provincia", "partido")) |>
    dplyr::mutate(
      abs_error = abs(prediction - actual_pct),
      covered = actual_pct >= lwr & actual_pct <= upr
    )
  vote_national_actual <- actual_vote |>
    dplyr::summarise(votos = sum(votos), .by = partido) |>
    dplyr::mutate(actual_pct = votos / sum(votos))
  vote_national <- sims |>
    dplyr::summarise(votos = sum(votos), .by = c(draw, partido)) |>
    dplyr::mutate(voto_pct = votos / sum(votos), .by = draw) |>
    dplyr::summarise(prediction = mean(voto_pct), .by = partido) |>
    dplyr::left_join(vote_national_actual, by = "partido") |>
    dplyr::mutate(abs_error = abs(prediction - actual_pct))
  seat_province <- sims |>
    dplyr::summarise(prediction = mean(seats), .by = c(provincia, partido)) |>
    dplyr::left_join(actual_seats, by = c("provincia", "partido")) |>
    dplyr::mutate(abs_error = abs(prediction - seats_actual))
  seat_national_actual <- actual_seats |>
    dplyr::summarise(seats_actual = sum(seats_actual), .by = partido)
  seat_national <- sims |>
    dplyr::summarise(seats = sum(seats), .by = c(draw, partido)) |>
    dplyr::summarise(prediction = mean(seats), .by = partido) |>
    dplyr::left_join(seat_national_actual, by = "partido") |>
    dplyr::mutate(abs_error = abs(prediction - seats_actual))
  list(
    label = label,
    summary = data.frame(
      model = label,
      vote_province_mae = mean(vote_province$abs_error),
      vote_national_mae = mean(vote_national$abs_error),
      seat_province_mae = mean(seat_province$abs_error),
      seat_national_mae = mean(seat_national$abs_error),
      vote_coverage = if (length(unique(sims$draw)) > 1L) {
        mean(vote_province$covered)
      } else {
        NA_real_
      }
    ),
    vote_province = vote_province,
    vote_national = vote_national,
    seat_province = seat_province,
    seat_national = seat_national
  )
}

mrp_allocate_baseline <- function(shares, seats_path, district_threshold) {
  predictions <- shares |>
    dplyr::mutate(draw = 1L, votos = voto_pct * 100000) |>
    dplyr::select(draw, provincia, partido, votos, voto_pct)
  seats <- mrp_read_data(seats_path) |>
    dplyr::mutate(provincia = mrp_normalize_province(provincia))
  allocated <- mrp_filter_seat_eligible(
    predictions, district_threshold
  ) |>
    dplyr::left_join(seats, by = "provincia") |>
    vota::fast_dhondt(
      cod_prov = provincia, sim = draw, partido = partido,
      votos_prov = votos, nseats = nseats, n_next = 0
    ) |>
    dplyr::count(draw, provincia, partido, name = "seats")
  predictions |>
    dplyr::left_join(allocated, by = c("draw", "provincia", "partido")) |>
    dplyr::mutate(seats = tidyr::replace_na(seats, 0L))
}

mrp_backtest_baselines <- function(survey, previous_results, target_results,
                                   seats_path, parties, district_threshold) {
  actual <- mrp_actual_vote(target_results, parties)
  provinces <- sort(unique(actual$provincia))
  national_survey <- survey |>
    dplyr::filter(!voto %in% c("ABNL", "Indecisos", "<18")) |>
    dplyr::mutate(partido = as.character(voto)) |>
    dplyr::filter(partido %in% parties) |>
    dplyr::summarise(weight = sum(ponde), .by = partido) |>
    tidyr::complete(partido = parties, fill = list(weight = 0)) |>
    dplyr::mutate(voto_pct = weight / sum(weight))
  weighted <- tidyr::expand_grid(provincia = provinces, partido = parties) |>
    dplyr::left_join(
      dplyr::select(national_survey, partido, voto_pct), by = "partido"
    )
  previous <- mrp_actual_vote(previous_results, parties)
  previous_national <- previous |>
    dplyr::summarise(votos = sum(votos), .by = partido) |>
    dplyr::mutate(previous_pct = votos / sum(votos))
  swing <- national_survey |>
    dplyr::left_join(previous_national, by = "partido") |>
    dplyr::transmute(
      partido, swing = voto_pct - tidyr::replace_na(previous_pct, 0)
    )
  uniform <- previous |>
    dplyr::left_join(swing, by = "partido") |>
    dplyr::mutate(voto_pct = pmax(actual_pct + tidyr::replace_na(swing, 0), 0)) |>
    dplyr::mutate(voto_pct = voto_pct / sum(voto_pct), .by = provincia) |>
    dplyr::select(provincia, partido, voto_pct)
  list(
    weighted_survey = mrp_allocate_baseline(
      weighted, seats_path, district_threshold
    ),
    uniform_swing = mrp_allocate_baseline(
      uniform, seats_path, district_threshold
    )
  )
}

mrp_write_backtest_report <- function(path, election, summary) {
  rows <- apply(summary, 1, function(row) {
    sprintf(
      "| %s | %.4f | %.4f | %.3f | %.3f | %s |",
      row[["model"]],
      as.numeric(row[["vote_province_mae"]]),
      as.numeric(row[["vote_national_mae"]]),
      as.numeric(row[["seat_province_mae"]]),
      as.numeric(row[["seat_national_mae"]]),
      ifelse(is.na(as.numeric(row[["vote_coverage"]])), "—",
             sprintf("%.3f", as.numeric(row[["vote_coverage"]])))
    )
  })
  writeLines(
    c(
      paste("# Backtest", election),
      "",
      "| Modelo | MAE voto prov. | MAE voto nac. | MAE escaños prov. | MAE escaños nac. | Cobertura voto |",
      "|---|---:|---:|---:|---:|---:|",
      rows
    ),
    path,
    useBytes = TRUE
  )
}

mrp_write_ablation_report <- function(path, election, summary,
                                      national_included) {
  rows <- apply(summary, 1, function(row) {
    sprintf(
      "| %s | %.4f | %.4f | %.3f | %.3f |",
      row[["model"]],
      as.numeric(row[["vote_province_mae"]]),
      as.numeric(row[["vote_national_mae"]]),
      as.numeric(row[["seat_province_mae"]]),
      as.numeric(row[["seat_national_mae"]])
    )
  })
  writeLines(
    c(
      paste("# Ablación", election),
      "",
      "| Etapa | MAE voto prov. | MAE voto nac. | MAE escaños prov. | MAE escaños nac. |",
      "|---|---:|---:|---:|---:|",
      rows,
      "",
      "`actual` usa recuerdo modelado y la propensión de participación sin el ajuste nacional logit.",
      "`+recuerdo_robusto` añade la mezcla de recuerdo; `+participacion` añade el ajuste logit.",
      "La etapa de indecisos se omite mientras no supere su puerta de entrada.",
      if (national_included) {
        "`+calibracion_nacional` usa los draws externos configurados."
      } else {
        "La calibración nacional se omite porque está desactivada o no tiene fuerza efectiva."
      }
    ),
    path,
    useBytes = TRUE
  )
}

mrp_select_backtest_variants <- function(config, requested = NULL) {
  available <- names(config$backtests$variants)
  if (is.null(requested)) return(config$backtests$variants)
  requested <- unique(trimws(as.character(requested)))
  if (!length(requested) || any(!nzchar(requested))) {
    stop("At least one backtest variant must be selected.", call. = FALSE)
  }
  unknown <- setdiff(requested, available)
  if (length(unknown)) {
    stop(
      "Unknown backtest variant(s): ", paste(unknown, collapse = ", "),
      ". Available variants: ", paste(available, collapse = ", "),
      call. = FALSE
    )
  }
  config$backtests$variants[requested]
}

mrp_parse_backtest_args <- function(args) {
  rebuild <- "--rebuild" %in% args
  variant_args <- args[grepl("^--variant=", args)]
  if (length(variant_args) > 1L) {
    stop("--variant may only be specified once.", call. = FALSE)
  }
  variants <- NULL
  if (length(variant_args)) {
    value <- sub("^--variant=", "", variant_args)
    variants <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
    if (!length(variants) || any(!nzchar(variants))) {
      stop("--variant requires one or more comma-separated names.", call. = FALSE)
    }
  }
  positional <- args[
    args != "--rebuild" & !grepl("^--variant=", args)
  ]
  if (length(positional) != 1L ||
      !positional %in% c("2023", "2019-10N", "all")) {
    stop(
      paste(
        "Usage: Rscript R/backtest.R {2023|2019-10N|all}",
        "[--variant=production|recency|geography[,..]] [--rebuild]"
      ),
      call. = FALSE
    )
  }
  list(election = positional, variants = variants, rebuild = rebuild)
}

run_backtest <- function(election = c("2023", "2019-10N"),
                         config_path = "config/mrp.yml", rebuild = FALSE,
                         variants = NULL) {
  config <- mrp_load_config(config_path)
  selected_variants <- mrp_select_backtest_variants(config, variants)
  elections <- if (identical(election, "all")) {
    c("2023", "2019-10N")
  } else {
    unique(vapply(election, function(x) {
      mrp_scalar_choice(x, c("2023", "2019-10N"), "election")
    }, character(1)))
  }
  results <- list()
  for (current in elections) {
    bundle <- mrp_validation_bundle(config, current)
    schemas <- list(
      previous = yaml::read_yaml(bundle$previous_schema),
      turnout = yaml::read_yaml(bundle$turnout_schema),
      vote = yaml::read_yaml(bundle$vote_schema)
    )
    extra <- list(
      election = current,
      previous_schema = schemas$previous,
      turnout_schema = schemas$turnout,
      vote_schema = schemas$vote,
      turnout_hash = mrp_file_hash(bundle$turnout)
    )
    current_variants <- selected_variants
    if (!is.null(bundle$fit_overrides)) {
      current_variants <- lapply(current_variants, function(x) {
        x$overrides <- bundle$fit_overrides
        x
      })
    }
    variant_results <- list()
    ablation_sims <- NULL
    last_config <- NULL
    for (name in names(current_variants)) {
      message("Backtest ", current, ": ", name)
      runtime <- mrp_backtest_config(config, bundle, name)
      run <- run_mrp(
        rebuild = rebuild,
        .config = runtime,
        .variant = current_variants[[name]],
        .previous_schema = schemas$previous,
        .turnout_schema = schemas$turnout,
        .vote_schema = schemas$vote,
        .signature_extra = extra,
        .report = identical(name, "production"),
        .ablation = identical(name, "production")
      )
      if (identical(name, "production")) ablation_sims <- run$ablation
      variant_results[[name]] <- mrp_backtest_metrics(
        run$sims, bundle$target_results, name
      )
      last_config <- runtime
    }
    survey <- readRDS(last_config$resolved_paths$survey)$data
    reference_variant <- names(variant_results)[1]
    parties <- unique(as.character(
      variant_results[[reference_variant]]$vote_province$partido
    ))
    baselines <- mrp_backtest_baselines(
      survey, bundle$previous_results, bundle$target_results,
      bundle$seats, parties, last_config$domain$district_threshold
    )
    baseline_metrics <- lapply(names(baselines), function(name) {
      mrp_backtest_metrics(baselines[[name]], bundle$target_results, name)
    })
    names(baseline_metrics) <- names(baselines)
    all_metrics <- c(variant_results, baseline_metrics)
    summary <- dplyr::bind_rows(lapply(all_metrics, `[[`, "summary"))
    root <- file.path(config$paths$output_root, "validation", current)
    dir.create(root, recursive = TRUE, showWarnings = FALSE)
    mrp_atomic_save_rds(all_metrics, file.path(root, "backtest_metrics.rds"))
    readr::write_csv(summary, file.path(root, "backtest_summary.csv"))
    mrp_write_backtest_report(
      file.path(root, "backtest_report.md"), current, summary
    )
    ablation_metrics <- NULL
    if (!is.null(ablation_sims)) {
      ablation_metrics <- lapply(names(ablation_sims), function(name) {
        mrp_backtest_metrics(
          ablation_sims[[name]], bundle$target_results, name
        )
      })
      names(ablation_metrics) <- names(ablation_sims)
      ablation_summary <- dplyr::bind_rows(
        lapply(ablation_metrics, `[[`, "summary")
      )
      mrp_atomic_save_rds(
        ablation_metrics, file.path(root, "ablation_metrics.rds")
      )
      readr::write_csv(
        ablation_summary, file.path(root, "ablation_summary.csv")
      )
      mrp_write_ablation_report(
        file.path(root, "ablation_report.md"),
        current,
        ablation_summary,
        "+calibracion_nacional" %in% names(ablation_sims)
      )
    }
    results[[current]] <- list(
      summary = summary,
      metrics = all_metrics,
      ablation = ablation_metrics
    )
  }
  invisible(results)
}

if (sys.nframe() == 0L) {
  arguments <- mrp_parse_backtest_args(commandArgs(trailingOnly = TRUE))
  run_backtest(
    arguments$election,
    rebuild = arguments$rebuild,
    variants = arguments$variants
  )
}
