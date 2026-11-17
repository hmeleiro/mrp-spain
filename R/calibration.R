mrp_validate_national_targets <- function(path, parties, require_otbl = FALSE) {
  if (!file.exists(path)) {
    stop("National calibration target file not found: ", path, call. = FALSE)
  }
  targets <- mrp_read_data(path)
  mrp_require_columns(targets, c("draw", "partido", "voto_pct"), "national targets")
  targets <- targets |>
    dplyr::transmute(
      target_draw = as.character(draw),
      partido = mrp_normalize_electoral_party(partido),
      voto_pct = suppressWarnings(as.numeric(voto_pct))
    )
  if (anyNA(targets) || any(!is.finite(targets$voto_pct)) ||
      any(targets$voto_pct < 0 | targets$voto_pct > 1)) {
    stop("National targets contain invalid values.", call. = FALSE)
  }
  if (anyDuplicated(targets[c("target_draw", "partido")])) {
    stop("National targets contain duplicate draw-party rows.", call. = FALSE)
  }
  unknown <- setdiff(unique(targets$partido), parties)
  if (length(unknown)) {
    stop(
      "National targets contain unmodeled parties: ",
      paste(unknown, collapse = ", "), call. = FALSE
    )
  }
  party_sets <- split(targets$partido, targets$target_draw)
  if (require_otbl && (!length(party_sets) ||
      any(!vapply(party_sets, function(x) "OTBL" %in% x, logical(1))))) {
    stop("OTBL reinforcement requires an explicit OTBL target in every draw.",
         call. = FALSE)
  }
  reference <- sort(unique(targets$partido))
  if (any(vapply(party_sets, function(x) !identical(sort(x), reference), logical(1)))) {
    stop("Every national target draw must contain the same parties.", call. = FALSE)
  }
  totals <- tapply(targets$voto_pct, targets$target_draw, sum)
  unlisted <- setdiff(parties, reference)
  if (any(totals > 1 + 1e-10) ||
      (!length(unlisted) && any(abs(totals - 1) > 1e-10)) ||
      (length(unlisted) && any(totals >= 1 - 1e-10))) {
    stop("National target shares leave an invalid residual mass.", call. = FALSE)
  }
  targets
}

mrp_rake_matrix <- function(seed, row_targets, column_targets,
                            tolerance = 1e-10, max_iterations = 1000L) {
  out <- as.matrix(seed)
  if (any(out < 0) || any(row_targets < 0) || any(column_targets < 0)) {
    stop("Raking inputs must be non-negative.", call. = FALSE)
  }
  total_scale <- max(1, sum(row_targets), sum(column_targets))
  if (abs(sum(row_targets) - sum(column_targets)) > tolerance * total_scale) {
    stop("Raking margins have different totals.", call. = FALSE)
  }
  positive_columns <- column_targets > tolerance
  if (any(positive_columns & colSums(out) <= tolerance)) {
    stop("A positive national target has no modeled support.", call. = FALSE)
  }
  for (iteration in seq_len(max_iterations)) {
    columns <- colSums(out)
    factors <- ifelse(column_targets <= tolerance, 0, column_targets / columns)
    factors[!is.finite(factors)] <- 0
    out <- sweep(out, 2, factors, `*`)
    rows <- rowSums(out)
    if (any(rows <= tolerance & row_targets > tolerance)) {
      stop("National calibration emptied a province.", call. = FALSE)
    }
    out <- sweep(out, 1, row_targets / rows, `*`)
    error <- max(
      abs(rowSums(out) - row_targets),
      abs(colSums(out) - column_targets)
    )
    if (error <= tolerance * max(1, sum(row_targets))) return(out)
  }
  stop("National calibration did not converge.", call. = FALSE)
}

mrp_national_target_for_draw <- function(targets, target_draw, parties,
                                         current_share, strength,
                                         otbl_strength = strength) {
  strength <- mrp_scalar_number(strength, "national_calibration.strength", 0, 1)
  otbl_strength <- mrp_scalar_number(
    otbl_strength %||% strength, "national_calibration.otbl_strength", strength, 1
  )
  selected <- targets[targets$target_draw == target_draw, , drop = FALSE]
  targeted <- selected$partido
  if (otbl_strength > strength &&
      (!"OTBL" %in% parties || !"OTBL" %in% targeted)) {
    stop("OTBL reinforcement requires modeled OTBL and an explicit OTBL target.",
         call. = FALSE)
  }
  unlisted <- setdiff(parties, targeted)
  target <- stats::setNames(selected$voto_pct, targeted)
  if (length(unlisted)) target <- c(target, residual = 1 - sum(target))
  current <- current_share[targeted]
  if (length(unlisted)) current <- c(current, residual = sum(current_share[unlisted]))
  mixed <- if (strength == 0) current else current^(1 - strength) * target^strength
  if (any(!is.finite(mixed)) || sum(mixed) <= 0) {
    stop("National calibration interpolation is undefined.", call. = FALSE)
  }
  base <- mixed / sum(mixed)
  if (otbl_strength == strength) return(base)

  # Reinforce the OTBL margin, preserving the composition of its complement.
  additional <- (otbl_strength - strength) / (1 - strength)
  otbl <- if (otbl_strength == 1) target[["OTBL"]] else
    (1 - additional) * base[["OTBL"]] + additional * target[["OTBL"]]
  other <- names(base) != "OTBL"
  other_mass <- sum(base[other])
  if (other_mass == 0 && otbl < 1) {
    stop("OTBL reinforcement requires positive mass in its empty complement.",
         call. = FALSE)
  }
  base[other] <- if (otbl == 1) 0 else base[other] * ((1 - otbl) / other_mass)
  base["OTBL"] <- otbl
  base
}

mrp_calibrate_national_votes <- function(votes, specification) {
  enabled <- isTRUE(specification$enabled)
  strength <- mrp_scalar_number(
    specification$strength, "national_calibration.strength", 0, 1
  )
  otbl_strength <- mrp_scalar_number(
    specification$otbl_strength %||% strength,
    "national_calibration.otbl_strength", strength, 1
  )
  diagnostics <- list(
    enabled = enabled,
    method = "otbl_reinforcement_v1",
    strength = strength,
    otbl_strength = otbl_strength,
    target_hash = NULL,
    status = "NOOP"
  )
  if (!enabled) {
    return(list(votes = votes, diagnostics = diagnostics))
  }
  parties <- unique(as.character(votes$partido))
  if (otbl_strength > strength && !"OTBL" %in% parties) {
    stop("OTBL reinforcement requires OTBL to be modeled.", call. = FALSE)
  }
  targets <- mrp_validate_national_targets(
    specification$draws_path, parties, require_otbl = otbl_strength > strength
  )
  diagnostics$target_hash <- mrp_file_hash(specification$draws_path)
  noop <- strength == 0 && otbl_strength == 0
  target_ids <- unique(targets$target_draw)
  prediction_draws <- sort(unique(votes$draw))
  calibrated <- vector("list", length(prediction_draws))
  achieved <- vector("list", length(prediction_draws))
  margins <- vector("list", length(prediction_draws))

  for (position in seq_along(prediction_draws)) {
    draw <- prediction_draws[position]
    target_draw <- target_ids[(position - 1L) %% length(target_ids) + 1L]
    current <- votes[votes$draw == draw, , drop = FALSE]
    provinces <- unique(as.character(current$provincia))
    matrix <- xtabs(votos ~ provincia + partido, current)
    matrix <- matrix[provinces, parties, drop = FALSE]
    province_totals <- rowSums(matrix)
    total <- sum(province_totals)
    current_share <- colSums(matrix) / total
    target_parties <- targets$partido[targets$target_draw == target_draw]
    unlisted <- setdiff(parties, target_parties)
    category_names <- c(target_parties, if (length(unlisted)) "residual")
    category_matrix <- matrix[, target_parties, drop = FALSE]
    if (length(unlisted)) {
      category_matrix <- cbind(
        category_matrix,
        residual = rowSums(matrix[, unlisted, drop = FALSE])
      )
    }
    base_share <- mrp_national_target_for_draw(
      targets, target_draw, parties, current_share, strength
    )
    target_share <- if (otbl_strength == strength) base_share else
      mrp_national_target_for_draw(
        targets, target_draw, parties, current_share, strength, otbl_strength
      )
    target_share <- target_share[category_names]
    raked <- if (noop) category_matrix else mrp_rake_matrix(
      category_matrix, province_totals, total * target_share
    )
    external <- targets$voto_pct[targets$target_draw == target_draw]
    if (length(unlisted)) external <- c(external, 1 - sum(external))
    margins[[position]] <- data.frame(
      draw = draw,
      target_draw = target_draw,
      category = category_names,
      initial_pct = as.numeric(colSums(category_matrix) / total),
      external_pct = external,
      base_pct = as.numeric(base_share[category_names]),
      final_pct = as.numeric(target_share),
      achieved_pct = as.numeric(colSums(raked) / total)
    )
    output <- matrix
    output[, target_parties] <- raked[, target_parties, drop = FALSE]
    if (length(unlisted)) {
      residual_base <- matrix[, unlisted, drop = FALSE]
      residual_total <- rowSums(residual_base)
      residual_share <- residual_base / residual_total
      residual_share[!is.finite(residual_share)] <- 0
      output[, unlisted] <- residual_share * raked[, "residual"]
    }
    long <- as.data.frame.table(output, responseName = "votos",
                                stringsAsFactors = FALSE)
    names(long)[1:2] <- c("provincia", "partido")
    long$draw <- draw
    long <- long |>
      dplyr::mutate(votos = as.numeric(votos)) |>
      dplyr::mutate(voto_pct = votos / sum(votos), .by = provincia) |>
      dplyr::select(draw, provincia, partido, votos, voto_pct)
    calibrated[[position]] <- long
    achieved[[position]] <- long |>
      dplyr::summarise(votos = sum(votos), .by = partido) |>
      dplyr::mutate(
        draw = draw,
        target_draw = target_draw,
        achieved_pct = votos / sum(votos)
      ) |>
      dplyr::select(draw, target_draw, partido, achieved_pct)
  }
  diagnostics$status <- if (noop) "NOOP" else "PASS"
  diagnostics$achieved <- dplyr::bind_rows(achieved)
  diagnostics$margins <- dplyr::bind_rows(margins)
  list(
    votes = if (noop) votes else dplyr::bind_rows(calibrated) |>
      dplyr::arrange(draw, provincia, partido),
    diagnostics = diagnostics
  )
}
