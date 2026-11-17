MRP_AGE_LEVELS <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")

mrp_read_data <- function(path, ...) {
  extension <- tolower(tools::file_ext(path))
  switch(
    extension,
    csv = readr::read_csv(path, show_col_types = FALSE, ...),
    parquet = arrow::read_parquet(path, ...),
    sav = haven::read_sav(path, ...),
    rds = readRDS(path),
    xlsx = readxl::read_xlsx(path, ...),
    stop("Unsupported input extension: ", extension, call. = FALSE)
  )
}

mrp_require_columns <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, " is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(data)
}

mrp_assert_complete <- function(data, columns, label) {
  mrp_require_columns(data, columns, label)
  counts <- vapply(data[columns], function(x) sum(is.na(x)), integer(1))
  bad <- counts[counts > 0]
  if (length(bad)) {
    stop(
      label, " contains missing values: ",
      paste(names(bad), bad, sep = "=", collapse = ", "),
      call. = FALSE
    )
  }
  invisible(data)
}

mrp_schema <- function(x) {
  if (is.character(x) && length(x) == 1L) {
    mrp_require_paths(x, "schema")
    return(yaml::read_yaml(x))
  }
  if (!is.list(x)) stop("Schema must be a mapping or YAML path.", call. = FALSE)
  x
}

mrp_apply_column_map <- function(data, schema) {
  mapping <- unlist(schema$column_map, use.names = TRUE)
  missing <- setdiff(names(mapping), names(data))
  if (length(missing)) {
    stop("Input is missing mapped columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  targets <- unname(mapping)
  if (anyDuplicated(targets)) {
    duplicated_targets <- unique(targets[duplicated(targets)])
    stop(
      "Column map contains duplicate targets: ",
      paste(duplicated_targets, collapse = ", "),
      call. = FALSE
    )
  }
  mapped <- data[names(mapping)]
  names(mapped) <- targets
  untouched <- setdiff(
    names(data),
    union(names(mapping), targets)
  )
  dplyr::bind_cols(
    mapped,
    data[untouched],
    .name_repair = "check_unique"
  )
}

mrp_label <- function(x) {
  if (inherits(x, "haven_labelled")) as.character(haven::as_factor(x)) else as.character(x)
}

mrp_normalize_electoral_party <- function(x) {
  value <- trimws(as.character(x))
  mapping <- c(
    "VOX" = "Vox",
    "SUMAR" = "Sumar",
    "EH Bildu" = "EH_Bildu",
    "EH BILDU" = "EH_Bildu",
    "EAJ-PNV" = "PNV",
    "EAJ/PNV" = "PNV",
    "CC-PNC" = "CCa",
    "CCa-NCa" = "CCa",
    "Nueva Canarias" = "NCa",
    "Na+ (UPN)" = "NA+",
    "Geroa Bai" = "PNV",
    "GBai" = "PNV",
    "GBAI" = "PNV"
  )
  matched <- value %in% names(mapping)
  value[matched] <- unname(mapping[value[matched]])
  value
}

mrp_flat_recode <- function(x, rule) {
  values <- mrp_label(x)
  remap <- unlist(rule$remap, use.names = TRUE)
  out <- unname(remap[values])
  out[is.na(out)] <- as.character(rule$default)
  out
}

mrp_vote_recode <- function(x, rule, auxiliary = FALSE) {
  values <- mrp_label(x)
  out <- mrp_flat_recode(values, rule)
  if (!auxiliary) return(out)

  canonical <- mrp_normalize_electoral_party(values)
  output_levels <- unique(as.character(unlist(rule$remap, use.names = FALSE)))
  canonical_match <- canonical %in% output_levels
  out[canonical_match] <- canonical[canonical_match]
  out[values %in% c("No sabe", "No sabe todavía", "N.C.")] <- "Indecisos"
  out[values %in% c("No votará", "No votaría", "Nulo", "Voto nulo")] <- "ABNL"
  out[values == "En blanco"] <- "OTBL"
  out
}

mrp_group_recode <- function(x, rule) {
  raw_values <- as.character(x)
  label_values <- mrp_label(x)
  default <- as.character(rule$default)
  out <- rep(default, length(label_values))
  groups <- rule[setdiff(names(rule), "default")]
  for (target in names(groups)) {
    members <- as.character(unlist(groups[[target]]))
    out[raw_values %in% members | label_values %in% members] <- target
  }
  out
}

mrp_named_recode <- function(x, rule, keep_unmapped = FALSE) {
  values <- as.character(x)
  mapping <- unlist(rule, use.names = TRUE)
  out <- unname(mapping[values])
  if (keep_unmapped) out[is.na(out)] <- values[is.na(out)]
  out
}

mrp_age_group <- function(age) {
  age <- suppressWarnings(as.numeric(age))
  cut(
    age,
    breaks = c(17, 24, 34, 44, 54, 64, Inf),
    labels = MRP_AGE_LEVELS,
    right = TRUE
  )
}

mrp_epa_age_group <- function(x) {
  dplyr::case_when(
    as.character(x) %in% c("16", "20") ~ "18-24",
    as.character(x) %in% c("25", "30") ~ "25-34",
    as.character(x) %in% c("35", "40") ~ "35-44",
    as.character(x) %in% c("45", "50") ~ "45-54",
    as.character(x) %in% c("55", "60") ~ "55-64",
    as.character(x) == "65" ~ "65+",
    TRUE ~ NA_character_
  )
}

mrp_coerce_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXt"))) return(as.Date(x))
  numeric <- suppressWarnings(as.numeric(x))
  out <- suppressWarnings(as.Date(as.character(x)))
  labelled <- is.na(out) & !is.na(numeric)
  out[labelled] <- as.Date(numeric[labelled], origin = "1970-01-01")
  out
}

mrp_matches_filter <- function(x, filter_value) {
  filter_text <- trimws(as.character(filter_value))
  raw_text <- trimws(as.character(x))
  label_text <- trimws(mrp_label(x))
  matches <- raw_text == filter_text | label_text == filter_text
  filter_number <- suppressWarnings(as.numeric(filter_value))
  if (!is.na(filter_number)) {
    number <- suppressWarnings(as.numeric(x))
    matches <- matches | (!is.na(number) & number == filter_number)
  }
  !is.na(matches) & matches
}

mrp_optional_terms <- function(data, terms) intersect(terms, names(data))

mrp_factor_levels <- function(values, reference) {
  factor(as.character(values), levels = union(reference, unique(as.character(values))))
}

mrp_build_formula <- function(response, terms, random_terms = NULL) {
  rhs <- c(terms, random_terms)
  rhs <- rhs[!is.na(rhs) & nzchar(rhs)]
  stats::as.formula(paste(response, "~", paste(rhs, collapse = " + ")))
}

mrp_provincial_predictors <- function(data, include_turnout = FALSE) {
  candidates <- c(
    "margen_der_izq_prev", "pct_alquiler", "pct_mayores65",
    "degurba1", "degurba2", "degurba3",
    if (include_turnout) "participacion_prev"
  )
  intersect(candidates, names(data))
}

mrp_prepare_provincial_data <- function(results_path, censo_path, degurba_path,
                                         previous_schema, domain) {
  schema <- mrp_schema(previous_schema)
  results <- mrp_read_data(results_path)
  mrp_require_columns(
    results,
    c(
      "codigo_provincia", "partido_recode", "votos", "votos_blancos",
      "votos_nulos", "abstenciones"
    ),
    "previous results"
  )
  extras <- results |>
    dplyr::select(codigo_provincia, votos_blancos, votos_nulos, abstenciones) |>
    dplyr::distinct() |>
    tidyr::pivot_longer(
      votos_blancos:abstenciones, names_to = "partido", values_to = "votos"
    ) |>
    dplyr::mutate(partido = ifelse(partido == "votos_blancos", "OTBL", "ABNL"))

  vote <- results |>
    dplyr::transmute(
      codigo_provincia = stringr::str_pad(codigo_provincia, 2, pad = "0"),
      partido = mrp_flat_recode(
        mrp_normalize_electoral_party(partido_recode),
        schema$results_recode
      ),
      votos
    ) |>
    dplyr::bind_rows(extras) |>
    dplyr::summarise(votos = sum(votos), .by = c(codigo_provincia, partido))

  turnout <- vote |>
    dplyr::mutate(tipo = ifelse(partido == "ABNL", "abstencion", "participacion")) |>
    dplyr::summarise(votos = sum(votos), .by = c(codigo_provincia, tipo)) |>
    dplyr::mutate(participacion_prev = votos / sum(votos), .by = codigo_provincia) |>
    dplyr::filter(tipo == "participacion") |>
    dplyr::select(codigo_provincia, participacion_prev)

  wide <- vote |>
    dplyr::filter(!partido %in% c("ABNL", "OTBL")) |>
    dplyr::mutate(pct = votos / sum(votos), .by = codigo_provincia) |>
    dplyr::select(codigo_provincia, partido, pct) |>
    tidyr::pivot_wider(
      names_from = partido, values_from = pct,
      names_glue = "voto_{partido}_prev", values_fill = 0
    ) |>
    dplyr::left_join(turnout, by = "codigo_provincia") |>
    dplyr::rename(provincia = codigo_provincia) |>
    janitor::clean_names()

  right <- intersect(paste0("voto_", tolower(domain$right_parties), "_prev"), names(wide))
  left <- intersect(paste0("voto_", tolower(domain$left_parties), "_prev"), names(wide))
  wide$margen_der_izq_prev <-
    rowSums(wide[right], na.rm = TRUE) - rowSums(wide[left], na.rm = TRUE)

  censo <- arrow::read_parquet(censo_path)
  alquiler <- censo |>
    dplyr::count(provincia = CPRO, TENEN_VIV) |>
    dplyr::mutate(pct_alquiler = n / sum(n), .by = provincia) |>
    dplyr::filter(TENEN_VIV == 3) |>
    dplyr::select(provincia, pct_alquiler)
  mayores <- censo |>
    dplyr::mutate(may65 = VAREDAD >= 65) |>
    dplyr::count(provincia = CPRO, may65) |>
    dplyr::mutate(pct_mayores65 = n / sum(n), .by = provincia) |>
    dplyr::filter(may65) |>
    dplyr::select(provincia, pct_mayores65)
  urban <- readxl::read_xlsx(degurba_path, sheet = "ES") |>
    janitor::clean_names() |>
    dplyr::mutate(provincia = substr(lau_code, 1, 2)) |>
    dplyr::summarise(pop = sum(population), .by = c(provincia, degurba)) |>
    dplyr::mutate(pop = pop / sum(pop), .by = provincia) |>
    tidyr::pivot_wider(names_from = degurba, values_from = pop, names_prefix = "degurba",
                       values_fill = 0)

  dplyr::full_join(wide, alquiler, by = "provincia") |>
    dplyr::full_join(urban, by = "provincia") |>
    dplyr::full_join(mayores, by = "provincia") |>
    dplyr::mutate(
      provincia = factor(stringr::str_pad(as.character(provincia), 2, pad = "0")),
      dplyr::across(where(is.numeric), ~ as.numeric(scale(.x)))
    )
}

mrp_prepare_poststrat <- function(path, schema) {
  schema <- mrp_schema(schema)
  if (schema$source == "epa") {
    files <- list.files(path, pattern = schema$input_pattern %||% "sav$",
                        full.names = TRUE)
    if (!length(files)) stop("No EPA files found in ", path, call. = FALSE)
    out <- purrr::map_dfr(files, function(file) {
      data <- mrp_apply_column_map(mrp_read_data(file, encoding = "ISO-8859-1"), schema)
      mrp_require_columns(
        data,
        c(
          "ccaa", "provincia", "sexo_raw", "edad_raw", "estudios_raw",
          "nacio_raw", "sidac_raw", "sidi_raw", "nbusca_raw", "peso_raw"
        ),
        "EPA"
      )
      data |>
        dplyr::filter(
          !as.character(edad_raw) %in% as.character(schema$filters$edad_exclude),
          as.character(nacio_raw) %in% as.character(schema$filters$nacio_include)
        ) |>
        dplyr::transmute(
          ccaa = mrp_named_recode(
            stringr::str_pad(as.character(ccaa), 2, pad = "0"),
            schema$ccaa_recode, keep_unmapped = TRUE
          ),
          provincia = stringr::str_pad(as.character(provincia), 2, pad = "0"),
          sexo = mrp_named_recode(sexo_raw, schema$sexo_recode),
          edad_grupo = factor(mrp_epa_age_group(edad_raw), levels = MRP_AGE_LEVELS),
          estudios = factor(mrp_group_recode(estudios_raw, schema$estudios_recode)),
          sitlab = factor(dplyr::case_when(
            sidac_raw == "1" ~ 1, sidac_raw == "2" ~ 2, sidi_raw == "01" ~ 3,
            sidi_raw == "03" ~ 4,
            nbusca_raw == "07" | sidi_raw %in% c("02", "05") ~ 5, TRUE ~ 99
          )),
          sitlab2 = factor(dplyr::case_when(
            sidac_raw %in% c("1", "2") ~ 1, sidi_raw == "01" ~ 3,
            nbusca_raw == "07" | sidi_raw %in% c("02", "05") ~ 2,
            sidi_raw == "03" ~ 4, TRUE ~ 5
          )),
          FACTOREL = suppressWarnings(as.numeric(peso_raw)) / length(files)
        )
    })
    mrp_assert_complete(
      out, c("ccaa", "provincia", "sexo", "edad_grupo", "estudios", "sitlab",
             "sitlab2", "FACTOREL"), "EPA poststrat"
    )
    return(out)
  }

  data <- mrp_apply_column_map(mrp_read_data(path), schema)
  province_codes <- infoelectoral::codigos_provincias |>
    dplyr::transmute(
      ccaa = stringr::str_pad(as.character(codigo_ccaa_ine), 2, pad = "0"),
      provincia = stringr::str_pad(as.character(codigo_provincia), 2, pad = "0")
    )
  out <- data |>
    dplyr::filter(
      as.character(nacio_raw) %in% as.character(schema$filters$nacio_include),
      suppressWarnings(as.numeric(edad_raw)) >= 18
    ) |>
    dplyr::mutate(
      provincia = stringr::str_pad(as.character(provincia), 2, pad = "0"),
      sexo = mrp_named_recode(sexo_raw, schema$sexo_recode),
      edad_grupo = factor(mrp_age_group(edad_raw), levels = MRP_AGE_LEVELS),
      tamuni = factor(ifelse(cmun == "991", 1, ifelse(cmun %in% c("992", "993"), 2, 3))),
      estudios = factor(mrp_group_recode(estudios_raw, schema$estudios_recode)),
      sitlab = factor(mrp_group_recode(sitlab_raw, schema$sitlab_recode)),
      sitlab2 = factor(mrp_group_recode(as.character(sitlab), schema$sitlab2_recode))
    ) |>
    dplyr::left_join(province_codes, by = "provincia") |>
    dplyr::count(
      sexo, edad_grupo, ccaa, provincia, tamuni, sitlab, sitlab2, estudios,
      name = "FACTOREL"
    )
  mrp_assert_complete(
    out, c("ccaa", "provincia", "sexo", "edad_grupo", "estudios", "sitlab",
           "sitlab2", "FACTOREL"), "Census poststrat"
  )
  out
}

mrp_prepare_previous_survey <- function(path, schema, recent_studies = NULL) {
  schema <- mrp_schema(schema)
  data <- mrp_apply_column_map(mrp_read_data(path), schema)
  if (!is.null(recent_studies) && "estudio" %in% names(data)) {
    studies <- tail(sort(unique(as.character(data$estudio))), recent_studies)
    data <- data[as.character(data$estudio) %in% studies, , drop = FALSE]
  }
  if (!"ponde" %in% names(data)) data$ponde <- 1
  mrp_require_columns(
    data,
    c("sexo", "edad", "provincia", "tamuni", "recuerdo", "estudios",
      "sitlab_raw", "participa_raw", "ponde"),
    "previous survey"
  )
  if (!is.null(schema$recuerdo_filter)) {
    data <- data[
      is.na(data$recuerdo) |
        !mrp_matches_filter(data$recuerdo, schema$recuerdo_filter),
      , drop = FALSE
    ]
  }
  recuerdo_original <- mrp_label(data$recuerdo)
  participation_type <- schema$participation$type %||% "binary_codes"
  if (!participation_type %in% c("binary_codes", "ordinal_scale")) {
    stop("participation.type must be binary_codes or ordinal_scale.", call. = FALSE)
  }
  if (participation_type == "ordinal_scale") {
    scale_min <- as.numeric(schema$participation$scale_min %||% 0)
    scale_max <- as.numeric(schema$participation$scale_max %||% 10)
    score <- suppressWarnings(as.numeric(mrp_label(data$participa_raw)))
    valid_score <- is.finite(score) & score >= scale_min & score <= scale_max
    data <- data[valid_score, , drop = FALSE]
    recuerdo_original <- recuerdo_original[valid_score]
    score <- score[valid_score]
    participation <- ordered(
      as.character(score),
      levels = as.character(seq.int(scale_min, scale_max))
    )
  } else {
    participated <- suppressWarnings(as.numeric(data$participa_raw)) %in%
      as.numeric(unlist(schema$participation$voted_codes))
    nonvote <- as.character(unlist(schema$participation$nonvote_labels))
    if (length(nonvote)) {
      participated[recuerdo_original %in% nonvote] <- FALSE
    }
    participation <- as.integer(participated)
  }

  out <- data |>
    dplyr::transmute(
      sexo = factor(mrp_label(sexo)),
      edad = suppressWarnings(as.numeric(edad)),
      edad_grupo = factor(mrp_age_group(edad), levels = MRP_AGE_LEVELS),
      provincia = factor(stringr::str_pad(as.character(provincia), 2, pad = "0")),
      tamuni = factor(mrp_group_recode(tamuni, schema$tamuni_recode)),
      recuerdo = factor(
        mrp_flat_recode(recuerdo, schema$recuerdo_recode),
        levels = unlist(schema$recuerdo_levels)
      ),
      estudios = factor(mrp_group_recode(estudios, schema$estudios_recode)),
      sitlab2 = factor(mrp_group_recode(sitlab_raw, schema$sitlab2_recode)),
      participa = participation,
      ponde = suppressWarnings(as.numeric(ponde))
    )
  mrp_assert_complete(
    out,
    c("sexo", "edad", "edad_grupo", "provincia", "tamuni", "recuerdo",
      "estudios", "sitlab2", "participa", "ponde"),
    "previous survey"
  )
  attr(out, "participation_type") <- participation_type
  out
}

mrp_prepare_vote_survey <- function(path, schema, recent_studies, candidates) {
  schema <- mrp_schema(schema)
  raw <- mrp_read_data(path)
  mrp_require_columns(raw, "estudio", "vote survey")
  raw$estudio <- as.character(raw$estudio)
  idv_source <- names(schema$column_map)[
    unlist(schema$column_map, use.names = FALSE) == "idv"
  ][1]
  raw <- raw[!is.na(raw[[idv_source]]), , drop = FALSE]
  studies <- tail(sort(unique(raw$estudio)), recent_studies)
  raw <- raw[raw$estudio %in% studies, , drop = FALSE]
  keep <- unique(c(names(schema$column_map), "estudio", "fecha", "fecha_campo"))
  data <- mrp_apply_column_map(raw[intersect(keep, names(raw))], schema)
  if (!"ponde" %in% names(data)) data$ponde <- 1
  if (!"voto_aux" %in% names(data)) data$voto_aux <- NA_character_
  date_names <- intersect(c("fecha", "fecha_campo"), names(data))
  dates <- if (length(date_names) > 0L) {
    mrp_coerce_date(data[[date_names[1]]])
  } else {
    rep(as.Date(NA), nrow(data))
  }

  out <- data |>
    dplyr::transmute(
      estudio = as.character(estudio),
      fecha_campo = dates,
      sexo = factor(mrp_label(sexo)),
      edad = suppressWarnings(as.numeric(edad)),
      edad_grupo = factor(mrp_age_group(edad), levels = MRP_AGE_LEVELS),
      recuerdo = factor(
        mrp_flat_recode(recuerdo, schema$recuerdo_recode),
        levels = unlist(schema$recuerdo_levels)
      ),
      voto = factor(
        mrp_vote_recode(idv, schema$voto_recode),
        levels = unlist(schema$voto_levels)
      ),
      voto_aux = factor(
        mrp_vote_recode(voto_aux, schema$voto_recode, auxiliary = TRUE),
        levels = unlist(schema$voto_levels)
      ),
      ccaa = factor(stringr::str_pad(as.character(ccaa), 2, pad = "0")),
      provincia = factor(stringr::str_pad(as.character(provincia), 2, pad = "0")),
      tamuni = factor(mrp_group_recode(tamuni, schema$tamuni_recode)),
      sitlab = factor(mrp_group_recode(sitlab_raw, schema$sitlab_recode)),
      sitlab2 = factor(mrp_group_recode(sitlab_raw, schema$sitlab2_recode)),
      estudios = factor(mrp_group_recode(estudios, schema$estudios_recode)),
      ponde = suppressWarnings(as.numeric(ponde))
    )

  required <- c(
    "sexo", "edad", "edad_grupo", "recuerdo", "voto", "ccaa", "provincia",
    "tamuni", "sitlab", "sitlab2", "estudios", "ponde"
  )
  complete <- stats::complete.cases(out[required])
  incomplete <- sum(!complete)
  out <- out[complete, , drop = FALSE]
  filtered <- mrp_filter_survey_availability(out, candidates)
  filtered$diagnostics$incomplete <- incomplete
  filtered$diagnostics$studies <- studies
  filtered
}
