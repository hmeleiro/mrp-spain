mrp_normalize_province <- function(x) {
  x <- trimws(as.character(x))
  numeric <- !is.na(x) & grepl("^[0-9]{1,2}$", x)
  x[numeric] <- sprintf("%02d", as.integer(x[numeric]))
  x
}

mrp_normalize_party <- function(x) trimws(as.character(x))

mrp_as_logical <- function(x, label) {
  if (is.logical(x)) return(x)
  value <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(value))
  out[value %in% c("true", "t", "1")] <- TRUE
  out[value %in% c("false", "f", "0")] <- FALSE
  if (anyNA(out)) stop(label, " must contain only TRUE/FALSE.", call. = FALSE)
  out
}

mrp_validate_candidates <- function(x, parties, reference = "PP",
                                    provinces = sprintf("%02d", 1:52)) {
  if (is.character(x) && length(x) == 1L) x <- mrp_read_data(x)
  mrp_require_columns(x, c("provincia", "partido", "se_presenta"), "candidates")
  out <- x |>
    dplyr::transmute(
      provincia = mrp_normalize_province(provincia),
      partido = mrp_normalize_party(partido),
      se_presenta = mrp_as_logical(se_presenta, "candidates$se_presenta")
    )
  if (anyDuplicated(paste(out$provincia, out$partido))) {
    stop("Candidates contain duplicated province-party pairs.", call. = FALSE)
  }
  expected <- tidyr::expand_grid(provincia = provinces, partido = parties)
  keys <- paste(out$provincia, out$partido)
  expected_keys <- paste(expected$provincia, expected$partido)
  missing <- setdiff(expected_keys, keys)
  extra <- setdiff(keys, expected_keys)
  if (length(missing) || length(extra)) {
    stop(
      "Candidates are not the exact expected province-party grid",
      if (length(missing)) paste0("; missing=", paste(head(missing, 8), collapse = ", ")),
      if (length(extra)) paste0("; extra=", paste(head(extra, 8), collapse = ", ")),
      call. = FALSE
    )
  }
  out <- out[match(expected_keys, keys), , drop = FALSE]
  if (!all(out$se_presenta[out$partido == reference])) {
    stop("Reference party ", reference, " must be available in every province.",
         call. = FALSE)
  }
  support <- tapply(out$se_presenta, out$partido, any)
  if (any(!support)) {
    stop("Every modeled party must be available somewhere: ",
         paste(names(support)[!support], collapse = ", "), call. = FALSE)
  }
  out
}

mrp_apply_candidate_overrides <- function(candidates, overrides = NULL) {
  candidates$se_presenta_fit <- candidates$se_presenta
  if (is.null(overrides) || !nrow(overrides)) return(candidates)
  mrp_require_columns(overrides, c("provincia", "partido", "se_presenta"), "overrides")
  keys <- paste(candidates$provincia, candidates$partido)
  override_keys <- paste(
    mrp_normalize_province(overrides$provincia),
    mrp_normalize_party(overrides$partido)
  )
  positions <- match(override_keys, keys)
  if (anyNA(positions)) stop("Candidate overrides contain unknown pairs.", call. = FALSE)
  candidates$se_presenta_fit[positions] <-
    mrp_as_logical(overrides$se_presenta, "overrides$se_presenta")
  candidates
}

mrp_filter_survey_availability <- function(data, candidates,
                                           availability_col = "se_presenta_fit",
                                           outside_model = c("ABNL", "<18", "Indecisos")) {
  if (!availability_col %in% names(candidates)) availability_col <- "se_presenta"
  lookup <- candidates |>
    dplyr::select(provincia, partido, available = dplyr::all_of(availability_col))
  checked <- data |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      provincia = mrp_normalize_province(provincia),
      .party = mrp_normalize_party(voto)
    )
  modeled <- !checked$.party %in% outside_model
  known <- checked$.party %in% lookup$partido
  if (any(modeled & !known)) {
    bad <- checked[modeled & !known, c("provincia", ".party"), drop = FALSE]
    stop(
      "Survey contains modeled values absent from candidates: ",
      paste(unique(paste(bad$provincia, bad$.party)), collapse = ", "),
      call. = FALSE
    )
  }
  checked$available <- TRUE
  checked$available[modeled] <- lookup$available[
    match(
      paste(checked$provincia[modeled], checked$.party[modeled]),
      paste(lookup$provincia, lookup$partido)
    )
  ]
  if (anyNA(checked$available[modeled])) {
    stop("Survey contains province-party keys absent from candidates.", call. = FALSE)
  }
  discarded <- checked |>
    dplyr::filter(!available) |>
    dplyr::count(provincia, partido = .party, name = "n")
  kept <- checked |>
    dplyr::filter(available) |>
    dplyr::arrange(.row_id) |>
    dplyr::select(-.row_id, -.party, -available)
  list(
    data = kept,
    diagnostics = list(
      before = nrow(data),
      after = nrow(kept),
      discarded = nrow(data) - nrow(kept),
      discarded_by_pair = discarded
    )
  )
}

mrp_stan_party_names <- function(parties) {
  out <- gsub("\\.|_", "", make.names(parties, unique = TRUE))
  if (anyDuplicated(out)) stop("Party names are not uniquely Stan-safe.", call. = FALSE)
  stats::setNames(out, parties)
}

mrp_province_ccaa_crosswalk <- function(geography_data, candidates) {
  mrp_require_columns(
    geography_data, c("provincia", "ccaa"), "geography data"
  )
  crosswalk <- geography_data |>
    dplyr::transmute(
      provincia = mrp_normalize_province(provincia),
      ccaa = sprintf("%02d", as.integer(as.character(ccaa)))
    ) |>
    dplyr::distinct()
  if (anyNA(crosswalk$provincia) || anyNA(crosswalk$ccaa) ||
      anyDuplicated(crosswalk$provincia)) {
    stop(
      "Provincial data must map every province to exactly one CCAA.",
      call. = FALSE
    )
  }
  missing <- setdiff(unique(candidates$provincia), crosswalk$provincia)
  if (length(missing)) {
    stop(
      "Candidate provinces are absent from the province-CCAA crosswalk: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  crosswalk
}

mrp_vote_geography_terms <- function(candidates, geography_data, parties,
                                     reference, geography,
                                     availability_col = "se_presenta_fit") {
  if (!availability_col %in% names(candidates)) availability_col <- "se_presenta"
  crosswalk <- mrp_province_ccaa_crosswalk(geography_data, candidates)
  candidate_support <- candidates |>
    dplyr::left_join(crosswalk, by = "provincia")
  mrp_assert_complete(
    candidate_support, "ccaa", "candidate geography support"
  )
  stan_names <- mrp_stan_party_names(parties)
  exclusions <- geography$support_exclusions
  specs <- lapply(parties, function(party) {
    party_rows <- candidate_support[
      candidate_support$partido == party &
        candidate_support[[availability_col]],
      , drop = FALSE
    ]
    counts <- table(party_rows$ccaa)
    original_support <- sort(names(counts))
    excluded <- sort(unique(exclusions$ccaa[exclusions$partido == party]))
    modeled_support <- setdiff(original_support, excluded)
    if (!length(modeled_support)) {
      stop(
        "Geographic support exclusions remove every CCAA for ", party, ".",
        call. = FALSE
      )
    }
    modeled_counts <- counts[modeled_support]
    reference_candidates <- names(modeled_counts)[
      modeled_counts == max(modeled_counts)
    ]
    reference_ccaa <- sort(reference_candidates)[1]
    n_ccaa <- length(modeled_support)
    strategy <- if (party == reference) {
      "reference"
    } else if (n_ccaa == 1L) {
      "none"
    } else if (n_ccaa < geography$random_min_ccaa) {
      "fixed"
    } else {
      "random"
    }
    stan_party <- unname(stan_names[[party]])
    group_column <- if (strategy == "random") {
      paste0("geo_ccaa_", stan_party)
    } else {
      NA_character_
    }
    contrast_ccaa <- if (strategy == "fixed") {
      setdiff(modeled_support, reference_ccaa)
    } else {
      character()
    }
    contrast_columns <- if (length(contrast_ccaa)) {
      paste0("geo_ccaa_", stan_party, "_", contrast_ccaa)
    } else {
      character()
    }
    terms <- switch(
      strategy,
      random = paste0("(1 | ", group_column, ")"),
      fixed = contrast_columns,
      none = character(),
      reference = character()
    )
    list(
      party = party,
      original_support = original_support,
      exclusions = intersect(excluded, original_support),
      modeled_support = modeled_support,
      n_ccaa = n_ccaa,
      strategy = strategy,
      reference_ccaa = reference_ccaa,
      group_column = group_column,
      contrast_ccaa = contrast_ccaa,
      contrast_columns = contrast_columns,
      terms = terms
    )
  })
  stats::setNames(specs, parties)
}

mrp_add_vote_geography <- function(data, geography) {
  mrp_require_columns(data, "ccaa", "vote geography data")
  ccaa <- sprintf("%02d", as.integer(as.character(data$ccaa)))
  if (anyNA(ccaa)) {
    stop("Vote geography data contain invalid CCAA codes.", call. = FALSE)
  }
  out <- data
  for (spec in geography) {
    if (spec$strategy == "random") {
      values <- ifelse(
        ccaa %in% spec$modeled_support, ccaa, spec$reference_ccaa
      )
      out[[spec$group_column]] <- factor(
        values, levels = spec$modeled_support
      )
    } else if (spec$strategy == "fixed") {
      for (index in seq_along(spec$contrast_ccaa)) {
        out[[spec$contrast_columns[index]]] <- as.numeric(
          ccaa == spec$contrast_ccaa[index]
        )
      }
    }
  }
  out
}

mrp_prepare_vote_geography <- function(data, candidates, geography_data, parties,
                                       reference, geography,
                                       availability_col = "se_presenta_fit") {
  specification <- mrp_vote_geography_terms(
    candidates, geography_data, parties, reference, geography,
    availability_col
  )
  list(
    data = mrp_add_vote_geography(data, specification),
    specification = specification,
    terms = lapply(specification, `[[`, "terms")
  )
}

mrp_offset_map <- function(parties, reference = "PP") {
  if (!reference %in% parties) stop("Reference party is not modeled.", call. = FALSE)
  stan <- mrp_stan_party_names(parties)
  reference_row <- parties == reference
  data.frame(
    partido = parties,
    es_referencia = reference_row,
    dpar = ifelse(reference_row, NA, paste0("mu", unname(stan))),
    offset_col = ifelse(reference_row, NA, paste0("choice_offset_", unname(stan))),
    stringsAsFactors = FALSE
  )
}

mrp_add_availability_offsets <- function(data, candidates, parties,
                                         reference = "PP",
                                         unavailable_offset = -1e6,
                                         availability_col = "se_presenta_fit") {
  if (!availability_col %in% names(candidates)) availability_col <- "se_presenta"
  map <- mrp_offset_map(parties, reference)
  out <- data
  province <- mrp_normalize_province(data$provincia)
  keys <- paste(candidates$provincia, candidates$partido)
  for (index in which(!map$es_referencia)) {
    party <- map$partido[index]
    positions <- match(paste(province, party), keys)
    if (anyNA(positions)) stop("Internal candidate lookup failed.", call. = FALSE)
    available <- candidates[[availability_col]][positions]
    out[[map$offset_col[index]]] <- ifelse(available, 0, unavailable_offset)
  }
  attr(out, "offset_map") <- map
  out
}

mrp_choice_formula <- function(response, rhs, parties, reference = "PP",
                               weight_col = NULL, offset_map = NULL) {
  if (is.null(offset_map)) offset_map <- mrp_offset_map(parties, reference)
  nonreference <- offset_map$partido[!offset_map$es_referencia]
  rhs_text <- function(value) {
    if (inherits(value, "formula")) {
      return(paste(deparse(value[[3]], width.cutoff = 500L), collapse = " "))
    }
    as.character(value)
  }
  rhs_by_party <- if (is.list(rhs) && !inherits(rhs, "formula")) {
    if (is.null(names(rhs)) || any(!nzchar(names(rhs))) ||
        !setequal(names(rhs), nonreference)) {
      stop(
        "Party-specific RHS must be named for every non-reference party.",
        call. = FALSE
      )
    }
    stats::setNames(lapply(rhs[nonreference], rhs_text), nonreference)
  } else {
    stats::setNames(rep(list(rhs_text(rhs)), length(nonreference)), nonreference)
  }
  response_text <- if (is.null(weight_col)) {
    response
  } else {
    sprintf("%s | weights(%s)", response, weight_col)
  }
  main <- stats::as.formula(paste(response_text, "~ 1"), env = globalenv())
  category <- lapply(which(!offset_map$es_referencia), function(index) {
    party <- offset_map$partido[index]
    stats::as.formula(
      sprintf(
        "%s ~ %s + offset(%s)",
        offset_map$dpar[index], rhs_by_party[[party]],
        offset_map$offset_col[index]
      ),
      env = globalenv()
    )
  })
  brms::bf(
    main,
    flist = category,
    family = brms::categorical(refcat = reference)
  )
}

mrp_assert_observed_available <- function(data, candidates,
                                          availability_col = "se_presenta_fit") {
  result <- mrp_filter_survey_availability(data, candidates, availability_col)
  if (result$diagnostics$discarded) {
    stop("Model data contain unavailable observed responses.", call. = FALSE)
  }
  invisible(data)
}

mrp_prepare_epred <- function(epred) {
  if (!is.array(epred) || length(dim(epred)) != 3L) {
    stop("epred must be a draws x cells x parties array.", call. = FALSE)
  }
  if (is.null(dimnames(epred)[[3]])) stop("epred must retain party names.", call. = FALSE)
  if (is.null(dimnames(epred)[[1]])) dimnames(epred)[[1]] <- seq_len(dim(epred)[1])
  epred
}

mrp_forbidden_mass <- function(epred, poststrat, candidates) {
  epred <- mrp_prepare_epred(epred)
  parties <- dimnames(epred)[[3]]
  keys <- paste(candidates$provincia, candidates$partido)
  province <- mrp_normalize_province(poststrat$provincia)
  unavailable <- vapply(parties, function(party) {
    positions <- match(paste(province, party), keys)
    !candidates$se_presenta[positions]
  }, logical(nrow(poststrat)))
  mass <- matrix(0, nrow = dim(epred)[1], ncol = dim(epred)[2])
  for (party_index in seq_along(parties)) {
    if (any(unavailable[, party_index])) {
      mass[, unavailable[, party_index]] <- mass[, unavailable[, party_index], drop = FALSE] +
        epred[, unavailable[, party_index], party_index, drop = FALSE][, , 1]
    }
  }
  mass
}

mrp_assert_forbidden_mass <- function(epred, poststrat, candidates, tolerance) {
  mass <- mrp_forbidden_mass(epred, poststrat, candidates)
  maximum <- max(mass)
  if (!is.finite(maximum) || maximum > tolerance) {
    stop(
      sprintf("Unavailable candidacy mass %.3g exceeds tolerance %.3g.", maximum, tolerance),
      call. = FALSE
    )
  }
  list(maximum = maximum, tolerance = tolerance, status = "PASS")
}

mrp_assert_forbidden_votes <- function(votes, candidates, tolerance) {
  checked <- votes |>
    dplyr::left_join(
      dplyr::select(candidates, provincia, partido, se_presenta),
      by = c("provincia", "partido")
    )
  mrp_assert_complete(checked, "se_presenta", "calibrated candidacy grid")
  forbidden <- checked$voto_pct[!checked$se_presenta]
  maximum <- if (length(forbidden)) {
    max(abs(forbidden), na.rm = TRUE)
  } else {
    0
  }
  if (!is.finite(maximum)) maximum <- 0
  if (maximum > tolerance) {
    stop(
      sprintf(
        "Calibrated unavailable candidacy mass %.3g exceeds tolerance %.3g.",
        maximum, tolerance
      ),
      call. = FALSE
    )
  }
  list(maximum = maximum, tolerance = tolerance, status = "PASS")
}

mrp_aggregate_epred <- function(epred, poststrat, weight_matrix) {
  epred <- mrp_prepare_epred(epred)
  if (!is.matrix(weight_matrix) ||
      !identical(dim(weight_matrix), dim(epred)[1:2])) {
    stop("weight_matrix must match epred draws and cells.", call. = FALSE)
  }
  provinces <- unique(as.character(poststrat$provincia))
  parties <- dimnames(epred)[[3]]
  votes <- array(
    0,
    dim = c(dim(epred)[1], length(provinces), length(parties)),
    dimnames = list(draw = dimnames(epred)[[1]], provincia = provinces, partido = parties)
  )
  province_by_cell <- as.character(poststrat$provincia)
  for (province_index in seq_along(provinces)) {
    cells <- which(province_by_cell == provinces[province_index])
    weights <- weight_matrix[, cells, drop = FALSE]
    for (party_index in seq_along(parties)) {
      probability <- epred[, cells, party_index, drop = FALSE]
      dim(probability) <- c(dim(epred)[1], length(cells))
      votes[, province_index, party_index] <- rowSums(probability * weights)
    }
  }
  out <- as.data.frame.table(votes, responseName = "votos", stringsAsFactors = FALSE)
  names(out)[1:3] <- c("draw", "provincia", "partido")
  out |>
    dplyr::mutate(draw = as.integer(draw), votos = as.numeric(votos)) |>
    dplyr::mutate(voto_pct = votos / sum(votos), .by = c(draw, provincia))
}

mrp_aggregate_epred_by_recall <- function(epred, poststrat, weight_matrix) {
  epred <- mrp_prepare_epred(epred)
  if (!is.matrix(weight_matrix) ||
      !identical(dim(weight_matrix), dim(epred)[1:2])) {
    stop("weight_matrix must match epred draws and cells.", call. = FALSE)
  }
  recall <- as.character(poststrat$recuerdo)
  recall_levels <- unique(recall)
  parties <- dimnames(epred)[[3]]
  votes <- array(
    0,
    dim = c(dim(epred)[1], length(recall_levels), length(parties)),
    dimnames = list(
      draw = dimnames(epred)[[1]],
      recuerdo = recall_levels,
      partido = parties
    )
  )
  for (recall_index in seq_along(recall_levels)) {
    cells <- which(recall == recall_levels[recall_index])
    weights <- weight_matrix[, cells, drop = FALSE]
    for (party_index in seq_along(parties)) {
      probability <- epred[, cells, party_index, drop = FALSE]
      dim(probability) <- c(dim(epred)[1], length(cells))
      votes[, recall_index, party_index] <- rowSums(probability * weights)
    }
  }
  out <- as.data.frame.table(votes, responseName = "votos", stringsAsFactors = FALSE)
  names(out)[1:3] <- c("draw", "recuerdo", "partido")
  out |>
    dplyr::mutate(draw = as.integer(draw), votos = as.numeric(votos))
}

mrp_aggregate_epred_by_groups <- function(epred, poststrat, weight_matrix,
                                          variables) {
  epred <- mrp_prepare_epred(epred)
  if (!is.matrix(weight_matrix) ||
      !identical(dim(weight_matrix), dim(epred)[1:2])) {
    stop("weight_matrix must match epred draws and cells.", call. = FALSE)
  }
  variables <- intersect(unique(variables), names(poststrat))
  if (!length(variables)) return(data.frame())

  parties <- dimnames(epred)[[3]]
  output <- lapply(variables, function(variable) {
    values <- as.character(poststrat[[variable]])
    levels <- unique(values[!is.na(values)])
    votes <- array(
      0,
      dim = c(dim(epred)[1], length(levels), length(parties)),
      dimnames = list(
        draw = dimnames(epred)[[1]], nivel = levels, partido = parties
      )
    )
    for (level_index in seq_along(levels)) {
      cells <- which(!is.na(values) & values == levels[level_index])
      weights <- weight_matrix[, cells, drop = FALSE]
      for (party_index in seq_along(parties)) {
        probability <- epred[, cells, party_index, drop = FALSE]
        dim(probability) <- c(dim(epred)[1], length(cells))
        votes[, level_index, party_index] <- rowSums(probability * weights)
      }
    }
    out <- as.data.frame.table(
      votes, responseName = "votos", stringsAsFactors = FALSE
    )
    names(out)[1:3] <- c("draw", "nivel", "partido")
    out |>
      dplyr::mutate(
        draw = as.integer(draw),
        variable = variable,
        votos = as.numeric(votos)
      ) |>
      dplyr::mutate(
        voto_pct = votos / sum(votos),
        .by = c(draw, variable, nivel)
      ) |>
      dplyr::select(draw, variable, nivel, partido, votos, voto_pct)
  })
  dplyr::bind_rows(output)
}
