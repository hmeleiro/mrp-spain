mrp_prepare_undecided_newdata <- function(survey, vote, provincial, domain) {
  variant <- vote$variant
  data <- survey |>
    dplyr::left_join(provincial, by = "provincia") |>
    mrp_add_vote_weights(
      variant$weight_scheme,
      variant$recency_lambda %||% 0.35,
      vote$temporal[c("unit", "structure")]
    )
  data$.voto_original <- as.character(data$voto)
  data$.voto_aux_original <- as.character(data$voto_aux)
  candidates <- mrp_apply_candidate_overrides(
    vote$candidates, variant$overrides %||% NULL
  )
  data <- mrp_add_availability_offsets(
    data, candidates, vote$parties, domain$reference_party,
    domain$unavailable_offset
  )
  model_data <- vote$model$data
  factor_columns <- names(model_data)[vapply(model_data, is.factor, logical(1))]
  factor_columns <- setdiff(factor_columns, "voto_aux")
  for (name in intersect(factor_columns, names(data))) {
    data[[name]] <- factor(
      as.character(data[[name]]),
      levels = levels(model_data[[name]]),
      ordered = is.ordered(model_data[[name]])
    )
  }
  data$voto_aux <- as.character(data$voto_aux)
  data
}

mrp_weighted_party_mass <- function(party, weight, parties) {
  out <- stats::setNames(numeric(length(parties)), parties)
  counts <- tapply(weight[party %in% parties], party[party %in% parties], sum)
  out[names(counts)] <- counts
  out
}

mrp_undecided_estimates <- function(survey, vote, provincial, domain,
                                    ndraws = 200L) {
  parties <- vote$parties
  data <- mrp_prepare_undecided_newdata(survey, vote, provincial, domain)
  decided <- data$.voto_original %in% parties
  undecided <- data$.voto_original == "Indecisos"
  if (!any(undecided)) stop("Survey contains no undecided respondents.", call. = FALSE)
  direct_mass <- mrp_weighted_party_mass(
    data$.voto_original[decided], data$ponde[decided], parties
  )
  direct_share <- direct_mass / sum(direct_mass)
  undecided_data <- data[undecided, , drop = FALSE]
  probability <- brms::posterior_epred(
    vote$model,
    newdata = undecided_data,
    ndraws = ndraws,
    re_formula = NULL,
    allow_new_levels = TRUE,
    sample_new_levels = "uncertainty"
  )
  probability <- mrp_prepare_epred(probability)
  if (!identical(dimnames(probability)[[3]], parties)) {
    stop("Undecided predictions do not match modeled parties.", call. = FALSE)
  }
  undecided_weight <- undecided_data$ponde
  model_mass <- matrix(
    rep(direct_mass, each = dim(probability)[1]),
    nrow = dim(probability)[1],
    dimnames = list(draw = seq_len(dim(probability)[1]), partido = parties)
  )
  for (party_index in seq_along(parties)) {
    party_probability <- probability[, , party_index, drop = FALSE]
    dim(party_probability) <- c(dim(probability)[1], dim(probability)[2])
    model_mass[, party_index] <- model_mass[, party_index] +
      rowSums(party_probability *
                rep(undecided_weight, each = dim(probability)[1]))
  }

  auxiliary <- undecided_data$.voto_aux_original
  resolved <- auxiliary %in% setdiff(parties, "OTBL")
  resolved_mass <- mrp_weighted_party_mass(
    auxiliary[resolved], undecided_weight[resolved], parties
  )
  unresolved_weight <- sum(undecided_weight[!resolved])
  sympathy_mass <- direct_mass + resolved_mass + unresolved_weight * direct_share

  assisted_mass <- matrix(
    rep(direct_mass + resolved_mass, each = dim(probability)[1]),
    nrow = dim(probability)[1],
    dimnames = list(draw = seq_len(dim(probability)[1]), partido = parties)
  )
  if (any(!resolved)) {
    unresolved_probability <- probability[, !resolved, , drop = FALSE]
    unresolved_weights <- undecided_weight[!resolved]
    for (party_index in seq_along(parties)) {
      party_probability <- unresolved_probability[, , party_index, drop = FALSE]
      dim(party_probability) <- c(dim(probability)[1], sum(!resolved))
      assisted_mass[, party_index] <- assisted_mass[, party_index] +
        rowSums(party_probability *
                  rep(unresolved_weights, each = dim(probability)[1]))
    }
  }
  normalize <- function(x) x / rowSums(x)
  direct_matrix <- matrix(
    rep(direct_share, each = dim(probability)[1]),
    nrow = dim(probability)[1],
    dimnames = dimnames(model_mass)
  )
  sympathy_matrix <- matrix(
    rep(sympathy_mass / sum(sympathy_mass), each = dim(probability)[1]),
    nrow = dim(probability)[1],
    dimnames = dimnames(model_mass)
  )
  matrices <- list(
    decided_only = direct_matrix,
    sympathy_only = sympathy_matrix,
    model_imputed = normalize(model_mass),
    sympathy_assisted = normalize(assisted_mass)
  )
  draws <- dplyr::bind_rows(lapply(names(matrices), function(method) {
    out <- as.data.frame.table(
      matrices[[method]], responseName = "voto_pct", stringsAsFactors = FALSE
    )
    names(out)[1:2] <- c("draw", "partido")
    out |>
      dplyr::transmute(
        method = method,
        draw = as.integer(draw),
        partido,
        voto_pct = as.numeric(voto_pct)
      )
  }))
  baseline <- draws |>
    dplyr::filter(method == "decided_only") |>
    dplyr::summarise(baseline_pct = mean(voto_pct), .by = partido)
  summary <- draws |>
    dplyr::summarise(
      pct = mean(voto_pct),
      lwr = stats::quantile(voto_pct, 0.025),
      upr = stats::quantile(voto_pct, 0.975),
      .by = c(method, partido)
    ) |>
    dplyr::left_join(baseline, by = "partido") |>
    dplyr::mutate(delta_pp = 100 * (pct - baseline_pct))
  list(
    draws = draws,
    summary = summary,
    diagnostics = list(
      undecided_weight_pct = 100 * sum(undecided_weight) / sum(data$ponde),
      sympathy_resolved_pct = 100 * sum(undecided_weight[resolved]) /
        sum(undecided_weight)
    )
  )
}

mrp_allocate_undecided_draws <- function(draws, baseline_sims, seats_path,
                                         district_threshold) {
  seats <- mrp_read_data(seats_path) |>
    dplyr::mutate(provincia = mrp_normalize_province(provincia))
  parties <- unique(as.character(draws$partido))
  baseline_draws <- sort(unique(baseline_sims$draw))
  prediction_draws <- sort(unique(draws$draw))
  predictions <- dplyr::bind_rows(lapply(
    unique(draws$method),
    function(method) {
      dplyr::bind_rows(lapply(seq_along(prediction_draws), function(position) {
        draw <- prediction_draws[position]
        baseline_draw <- baseline_draws[
          (position - 1L) %% length(baseline_draws) + 1L
        ]
        current <- baseline_sims[
          baseline_sims$draw == baseline_draw, , drop = FALSE
        ]
        provinces <- unique(as.character(current$provincia))
        seed <- xtabs(votos ~ provincia + partido, current)
        seed <- seed[provinces, parties, drop = FALSE]
        row_targets <- rowSums(seed)
        target <- draws |>
          dplyr::filter(method == !!method, draw == !!draw) |>
          dplyr::select(partido, voto_pct)
        target_share <- stats::setNames(target$voto_pct, target$partido)[parties]
        target_share <- target_share / sum(target_share)
        calibrated <- mrp_rake_matrix(
          seed, row_targets, sum(row_targets) * target_share
        )
        out <- as.data.frame.table(
          calibrated, responseName = "votos", stringsAsFactors = FALSE
        )
        names(out)[1:2] <- c("provincia", "partido")
        out |>
          dplyr::mutate(
            method = method,
            draw = draw,
            votos = as.numeric(votos),
            voto_pct = votos / sum(votos),
            .by = provincia
          ) |>
          dplyr::select(method, draw, provincia, partido, votos, voto_pct)
      }))
    }
  ))
  dplyr::bind_rows(lapply(unique(predictions$method), function(method) {
    current <- predictions[predictions$method == method, , drop = FALSE]
    allocated <- mrp_filter_seat_eligible(
      current, district_threshold
    ) |>
      dplyr::left_join(seats, by = "provincia") |>
      vota::fast_dhondt(
        cod_prov = provincia, sim = draw, partido = partido,
        votos_prov = votos, nseats = nseats, n_next = 0
      ) |>
      dplyr::count(draw, provincia, partido, name = "seats")
    current |>
      dplyr::left_join(allocated, by = c("draw", "provincia", "partido")) |>
      dplyr::mutate(seats = tidyr::replace_na(seats, 0L)) |>
      dplyr::select(method, draw, provincia, partido, votos, voto_pct, seats)
  }))
}

mrp_undecided_gate <- function(summaries, metrics,
                               impact_threshold_pp = 0.5,
                               mae_improvement_pp = 0.1,
                               degradation_limit = 1.05) {
  relevant <- summaries |>
    dplyr::filter(election == "2023", baseline_pct >= 0.03)
  impact <- relevant |>
    dplyr::summarise(
      max_impact_pp = max(abs(delta_pp)),
      .by = method
    )
  metric_table <- metrics |>
    dplyr::summarise(
      dplyr::across(
        c(vote_national_mae, vote_province_mae,
          seat_province_mae, seat_national_mae),
        mean
      ),
      .by = method
    )
  baseline <- metric_table[metric_table$method == "decided_only", , drop = FALSE]
  candidates <- metric_table |>
    dplyr::filter(method != "decided_only") |>
    dplyr::left_join(impact, by = "method") |>
    dplyr::mutate(
      impact_pass = max_impact_pp >= impact_threshold_pp,
      mae_gain_pp = 100 * (
        baseline$vote_national_mae[1] - vote_national_mae
      ),
      national_pass = mae_gain_pp >= mae_improvement_pp,
      degradation_pass =
        vote_province_mae <= baseline$vote_province_mae[1] * degradation_limit &
        seat_province_mae <= baseline$seat_province_mae[1] * degradation_limit &
        seat_national_mae <= baseline$seat_national_mae[1] * degradation_limit,
      promote = impact_pass & national_pass & degradation_pass
    )
  candidates
}
