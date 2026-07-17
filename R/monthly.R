# Monthly PM2.5 download and descriptive analysis for Rio de Janeiro

#' Download monthly PM2.5 means for Rio de Janeiro
#'
#' Downloads `df_mensal`, processes its monthly PM2.5 value into
#' `pm25_media`, validates the municipality-year-month panel, and preserves
#' parser, schema, truncation, query, and checksum provenance. Server-side year
#' and month filters are used when a subset is requested.
#'
#' @param anos Optional years to download.
#' @param meses Optional months (1 to 12) to download.
#' @param validar_completude If `TRUE`, report panel completeness.
#' @param require_complete If `TRUE`, require an explicit temporal domain and
#'   fail on an incomplete panel, schema mismatch, parser issue, duplicate key,
#'   or truncation evidence.
#' @param anos_esperados Optional explicit expected years. Defaults to `anos`.
#' @param meses_esperados Optional explicit expected months. When expected years
#'   are supplied, defaults to all 12 months.
#' @param periodo_inicio Optional first expected month as `YYYY-MM` or a Date.
#' @param periodo_fim Optional last expected month in the same format.
#' @param particionar If `TRUE`, download one server-side year-month partition
#'   at a time. It defaults to `require_complete`.
#' @param timeout HTTP timeout in seconds for each request.
#' @param delay Delay between partition requests.
#' @param usar_cache Reuse the RJ cache for a non-partitioned request.
#' @param snapshot Attach a reproducible snapshot to the result.
#' @param ... Additional arguments passed to `vigiar_baixar_rj()` or
#'   `vigiar_baixar_rj_completo()`.
#' @return A processed monthly PM2.5 tibble with a `periodo` Date column and
#'   structured completeness and quality attributes.
#' @export
vigiar_baixar_pm25_mensal_rj <- function(
  anos = NULL,
  meses = NULL,
  validar_completude = TRUE,
  require_complete = FALSE,
  anos_esperados = anos,
  meses_esperados = NULL,
  periodo_inicio = NULL,
  periodo_fim = NULL,
  particionar = require_complete,
  timeout = 180,
  delay = 0.2,
  usar_cache = FALSE,
  snapshot = FALSE,
  ...
) {
  anos <- .vigiar_monthly_integer_values(
    anos, "anos", 2000L, as.integer(format(Sys.Date(), "%Y"))
  )
  meses <- .vigiar_monthly_integer_values(meses, "meses", 1L, 12L)
  anos_esperados <- .vigiar_monthly_integer_values(
    anos_esperados, "anos_esperados", 2000L,
    as.integer(format(Sys.Date(), "%Y"))
  )
  meses_esperados <- .vigiar_monthly_integer_values(
    meses_esperados, "meses_esperados", 1L, 12L
  )
  start <- .vigiar_parse_periodo(periodo_inicio, "periodo_inicio")
  end <- .vigiar_parse_periodo(periodo_fim, "periodo_fim")
  if (xor(is.null(start), is.null(end))) {
    stop("'periodo_inicio' and 'periodo_fim' must be supplied together.",
         call. = FALSE)
  }
  if (!is.null(start) && start > end) {
    stop("'periodo_inicio' must not be after 'periodo_fim'.", call. = FALSE)
  }

  explicit_domain <- !is.null(anos_esperados) || !is.null(start)
  if (isTRUE(require_complete) && !explicit_domain) {
    stop(
      "require_complete = TRUE needs explicit expected years or period endpoints; ",
      "observed data cannot define their own completeness boundary.",
      call. = FALSE
    )
  }
  if (is.null(meses_esperados) && explicit_domain && is.null(start)) {
    meses_esperados <- meses %||% 1:12
  }

  selected_years <- anos %||% anos_esperados
  selected_months <- meses
  if (!is.null(start)) {
    dates <- seq(start, end, by = "month")
    selected_years <- sort(unique(as.integer(format(dates, "%Y"))))
  }

  dots <- list(...)
  if (isTRUE(particionar)) {
    partition_years <- selected_years
    if (is.null(partition_years)) {
      stop(
        "Partitioned monthly download needs 'anos', 'anos_esperados', or period endpoints.",
        call. = FALSE
      )
    }
    args <- c(list(
      tabela = "df_mensal",
      por = "mes",
      anos = partition_years,
      meses = selected_months %||% 1:12,
      timeout = timeout,
      delay = delay,
      validar_cobertura = FALSE,
      exigir_completo = FALSE,
      require_complete = require_complete,
      processar = TRUE,
      tipo = "mensal"
    ), dots)
    dados <- do.call(vigiar_baixar_rj_completo, args)
  } else {
    requested_filters <- list()
    if (!is.null(selected_years)) {
      requested_filters$ano <- selected_years
    }
    if (!is.null(selected_months)) {
      requested_filters$mes <- selected_months
    }
    dots$filtros <- .vigiar_combinar_filtros(
      dots$filtros,
      if (length(requested_filters) == 0L) NULL else requested_filters
    )
    args <- c(list(
      tabela = "df_mensal",
      timeout = timeout,
      validar_cobertura = FALSE,
      require_complete = FALSE,
      processar = TRUE,
      tipo = "mensal",
      usar_cache = usar_cache,
      snapshot = FALSE
    ), dots)
    dados <- do.call(vigiar_baixar_rj, args)
  }

  required <- c("cod_municipio", "codigo_ibge_6", "ano", "mes", "pm25_media")
  missing <- setdiff(required, names(dados))
  if (length(missing) > 0L) {
    stop(
      "Processed monthly PM2.5 data are missing required columns: ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  source_attributes <- .vigiar_data_attributes(dados)
  dados$ano <- as.integer(dados$ano)
  dados$mes <- as.integer(dados$mes)
  dados$pm25_media <- as.numeric(dados$pm25_media)
  dados$periodo <- as.Date(sprintf("%04d-%02d-01", dados$ano, dados$mes))
  keep <- rep(TRUE, nrow(dados))
  if (!is.null(selected_years)) {
    keep <- keep & dados$ano %in% selected_years
  }
  if (!is.null(selected_months)) {
    keep <- keep & dados$mes %in% selected_months
  }
  if (!is.null(start)) {
    keep <- keep & dados$periodo >= start & dados$periodo <= end
  }
  dados <- dados[keep, , drop = FALSE]
  dados <- .vigiar_restore_data_attributes(dados, source_attributes)

  schema <- .vigiar_rj_schema_assessment("df_mensal")
  completeness <- suppressWarnings(vigiar_rj_completude_tabela(
    dados,
    tabela = "df_mensal",
    require_complete = FALSE,
    anos_esperados = anos_esperados,
    meses_esperados = meses_esperados,
    periodo_inicio = periodo_inicio,
    periodo_fim = periodo_fim
  ))
  analysis <- suppressWarnings(vigiar_analisar_pm25_mensal_rj(
    dados,
    completude = completeness,
    schema_status = schema$status
  ))

  if (isTRUE(validar_completude)) {
    complete_groups <- sum(completeness$completo %in% TRUE)
    cli::cli_alert_info(
      "Monthly RJ panel: {complete_groups}/{nrow(completeness)} year-month groups complete."
    )
    if (complete_groups < nrow(completeness)) {
      warning(
        sprintf(
          "Monthly RJ panel is incomplete in %d year-month group(s).",
          nrow(completeness) - complete_groups
        ),
        call. = FALSE
      )
    }
  }

  if (isTRUE(require_complete)) {
    .vigiar_require_monthly(analysis)
  }

  attr(dados, "vigiar_tabela") <- "df_mensal"
  attr(dados, "vigiar_granularidade") <- "municipio_ano_mes"
  attr(dados, "vigiar_valor_pm25") <- "pm25_media"
  attr(dados, "vigiar_unidade_pm25") <- "ug/m3"
  attr(dados, "vigiar_rj_completude") <- completeness
  attr(dados, "vigiar_monthly_analysis") <- analysis
  attr(dados, "vigiar_schema_status") <- schema$status
  class(dados) <- unique(c(
    "vigiar_pm25_monthly", "vigiar_pm25", "vigiar_air_quality",
    "vigiar_tbl", class(dados)
  ))

  if (isTRUE(snapshot)) {
    attr(dados, "vigiar_snapshot") <- vigiar_snapshot(
      dados = dados, tabela = "df_mensal"
    )
  }
  dados
}

#' Analyse monthly PM2.5 data for Rio de Janeiro
#'
#' Produces descriptive summaries by time, municipality, and health region;
#' municipality-month completeness; period-specific data-quality metrics; and
#' the package diagnostic. It does not fit statistical, causal, predictive,
#' GAM, DLNM, or relative-risk models.
#'
#' @param dados Processed monthly PM2.5 data.
#' @param limite_plausibilidade Concentrations above this value are flagged for
#'   source and unit review, not automatically removed.
#' @param completude Optional precomputed `vigiar_rj_completude_tabela()` result.
#' @param schema_status Optional schema status (`pass`, `fail`, or `unknown`).
#' @return A structured `vigiar_monthly_analysis` list.
#' @export
vigiar_analisar_pm25_mensal_rj <- function(
  dados,
  limite_plausibilidade = 1000,
  completude = NULL,
  schema_status = attr(dados, "vigiar_schema_status") %||% "unknown"
) {
  required <- c("ano", "mes", "pm25_media")
  missing <- setdiff(required, names(dados))
  if (length(missing) > 0L) {
    stop(
      "Monthly analysis requires columns: ", paste(required, collapse = ", "),
      ". Missing: ", paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (length(limite_plausibilidade) != 1L ||
        !is.finite(limite_plausibilidade) || limite_plausibilidade <= 0) {
    stop("'limite_plausibilidade' must be one positive finite number.",
         call. = FALSE)
  }

  data <- tibble::as_tibble(dados)
  col_muni <- .vigiar_coluna_municipio(data)
  if (is.na(col_muni)) {
    stop("Monthly analysis requires a municipality code column.", call. = FALSE)
  }
  data$codigo_ibge_6 <- .vigiar_normalizar_codigo_municipio(data[[col_muni]])
  data$ano <- suppressWarnings(as.integer(data$ano))
  data$mes <- suppressWarnings(as.integer(data$mes))
  data$pm25_media <- suppressWarnings(as.numeric(data$pm25_media))
  registry_index <- match(data$codigo_ibge_6, RJ_MUNICIPIOS$codigo_ibge_6)
  data$municipio <- RJ_MUNICIPIOS$municipio[registry_index]
  data$regiao_saude <- RJ_MUNICIPIOS$regiao_saude[registry_index]

  if (is.null(completude)) {
    completude <- suppressWarnings(vigiar_rj_completude_tabela(
      data, tabela = "df_mensal"
    ))
  }
  missing_municipalities <- .vigiar_monthly_missing(completude)
  quality <- .vigiar_monthly_quality(data, limite_plausibilidade)
  diagnostic <- vigiar_diagnosticar_serie(
    data,
    col_muni = "codigo_ibge_6",
    col_ano = "ano",
    col_mes = "mes",
    col_pm25 = "pm25_media",
    escopo = "rj"
  )
  truncation_status <- attr(dados, "vigiar_truncation_status") %||%
    if (isTRUE(attr(dados, "vigiar_possivel_truncamento"))) "possible" else
      "no_evidence"
  parser_status <- attr(dados, "vigiar_parser_status") %||% "unknown"
  partition_report <- attr(dados, "vigiar_partition_report")
  if (is.null(partition_report)) {
    partition_report <- tibble::tibble()
  }
  n_failed_partitions <- if ("status" %in% names(partition_report)) {
    sum(partition_report$status == "failed", na.rm = TRUE)
  } else {
    0L
  }
  partition_status <- if (nrow(partition_report) == 0L) {
    "not_partitioned"
  } else if (n_failed_partitions > 0L) {
    "fail"
  } else {
    "pass"
  }
  quality_status <- quality$quality_status[[1]]
  panel_complete <- nrow(completude) > 0L && all(completude$completo %in% TRUE)
  domain_status <- unique(completude$overall_status %||% "unknown")[[1]]
  conclusion <- if (truncation_status %in% c(
    "possible", "probable", "confirmed", "unknown"
  )) {
    "truncated"
  } else if (identical(schema_status, "fail")) {
    "schema_changed"
  } else if (n_failed_partitions > 0L) {
    "failed"
  } else if (!panel_complete) {
    "partial"
  } else if (identical(quality_status, "fail")) {
    "failed"
  } else if (identical(schema_status, "pass") && identical(domain_status, "pass")) {
    "complete"
  } else {
    "complete_within_inferred_domain"
  }
  overall_status <- if (
    conclusion == "complete" && identical(quality_status, "pass")
  ) {
    "pass"
  } else if (
    conclusion %in% c("complete", "complete_within_inferred_domain") &&
      identical(quality_status, "warning")
  ) {
    "unknown"
  } else if (conclusion == "complete_within_inferred_domain") {
    "unknown"
  } else {
    "fail"
  }

  out <- list(
    summary = .vigiar_monthly_summary(data),
    temporal_domain = .vigiar_monthly_domain(data, completude),
    by_year = .vigiar_monthly_summary(data, "ano"),
    by_year_month = .vigiar_monthly_summary(data, c("ano", "mes")),
    by_calendar_month = .vigiar_monthly_summary(data, "mes"),
    by_municipality = .vigiar_monthly_summary(
      data, c("codigo_ibge_6", "municipio")
    ),
    by_health_region = .vigiar_monthly_summary(data, "regiao_saude"),
    by_health_region_year_month = .vigiar_monthly_summary(
      data, c("regiao_saude", "ano", "mes")
    ),
    completeness = completude,
    missing_municipalities = missing_municipalities,
    quality = quality,
    quality_by_year_month = .vigiar_monthly_quality_by_period(
      data, limite_plausibilidade
    ),
    diagnostic = diagnostic,
    parser_status = parser_status,
    schema_status = schema_status,
    truncation_status = truncation_status,
    partition_status = partition_status,
    n_failed_partitions = as.integer(n_failed_partitions),
    partition_report = partition_report,
    conclusion = conclusion,
    overall_status = overall_status,
    scientific_scope = paste(
      "Descriptive data audit only; no causal, GAM, DLNM, relative-risk,",
      "or predictive-model validity is assessed."
    )
  )
  class(out) <- "vigiar_monthly_analysis"
  out
}

#' @export
print.vigiar_monthly_analysis <- function(x, ...) {
  cat(sprintf(
    "<vigiar_monthly_analysis> conclusion=%s, overall=%s\n",
    x$conclusion, x$overall_status
  ))
  cat(sprintf(
    "Rows: %d | municipalities: %d | incomplete periods: %d | failed partitions: %d\n",
    x$summary$n_observacoes[[1]],
    x$summary$n_municipios[[1]],
    sum(x$completeness$completo %in% FALSE),
    x$n_failed_partitions
  ))
  invisible(x)
}

.vigiar_monthly_integer_values <- function(x, name, min_value, max_value) {
  if (is.null(x)) {
    return(NULL)
  }
  raw <- as.character(x)
  values <- suppressWarnings(as.integer(raw))
  exact <- !is.na(values) & grepl("^[0-9]+$", trimws(raw))
  if (any(!exact) || any(values < min_value | values > max_value)) {
    stop(
      sprintf("'%s' must contain integers from %d to %d.",
              name, min_value, max_value),
      call. = FALSE
    )
  }
  sort(unique(values))
}

.vigiar_monthly_summary <- function(data, groups = character()) {
  indices <- if (length(groups) == 0L) {
    list(all = seq_len(nrow(data)))
  } else {
    split(seq_len(nrow(data)), do.call(
      interaction,
      c(unclass(data[groups]), list(drop = TRUE, lex.order = TRUE))
    ))
  }
  rows <- lapply(indices, function(index) {
    block <- data[index, , drop = FALSE]
    values <- block$pm25_media
    valid <- values[is.finite(values)]
    row <- if (length(groups) == 0L) data.frame(.row = 1L) else
      as.data.frame(block[1, groups, drop = FALSE])
    row$n_observacoes <- nrow(block)
    row$n_validos <- length(valid)
    row$n_ausentes <- sum(is.na(values))
    row$pct_ausente <- if (nrow(block) == 0L) NA_real_ else
      100 * row$n_ausentes / nrow(block)
    row$n_municipios <- length(unique(block$codigo_ibge_6[!is.na(block$codigo_ibge_6)]))
    stats <- .vigiar_monthly_stats(valid)
    for (name in names(stats)) {
      row[[name]] <- stats[[name]]
    }
    row
  })
  out <- tibble::as_tibble(do.call(rbind.data.frame, rows))
  if (".row" %in% names(out)) {
    out$.row <- NULL
  }
  out
}

.vigiar_monthly_stats <- function(values) {
  if (length(values) == 0L) {
    return(list(
      media = NA_real_, mediana = NA_real_, desvio_padrao = NA_real_,
      p05 = NA_real_, p25 = NA_real_, p75 = NA_real_, p95 = NA_real_,
      minimo = NA_real_, maximo = NA_real_
    ))
  }
  quantiles <- stats::quantile(
    values, c(0.05, 0.25, 0.75, 0.95), na.rm = TRUE, names = FALSE
  )
  list(
    media = mean(values),
    mediana = stats::median(values),
    desvio_padrao = if (length(values) > 1L) stats::sd(values) else NA_real_,
    p05 = quantiles[[1]], p25 = quantiles[[2]],
    p75 = quantiles[[3]], p95 = quantiles[[4]],
    minimo = min(values), maximo = max(values)
  )
}

.vigiar_monthly_quality <- function(data, limit) {
  values <- data$pm25_media
  keys <- data[c("codigo_ibge_6", "ano", "mes")]
  n_duplicates <- sum(duplicated(keys) | duplicated(keys, fromLast = TRUE))
  n_negative <- sum(values < 0, na.rm = TRUE)
  n_zero <- sum(values == 0, na.rm = TRUE)
  n_high <- sum(values > limit, na.rm = TRUE)
  n_invalid_month <- sum(is.na(data$mes) | !data$mes %in% 1:12)
  n_invalid_year <- sum(is.na(data$ano) | data$ano < 2000L |
                          data$ano > as.integer(format(Sys.Date(), "%Y")))
  n_invalid_code <- sum(is.na(data$codigo_ibge_6) |
                          !data$codigo_ibge_6 %in% RJ_MUNICIPIOS$codigo_ibge_6)
  n_missing <- sum(is.na(values))
  status <- if (any(c(
    n_duplicates, n_negative, n_invalid_month, n_invalid_year, n_invalid_code
  ) > 0L)) {
    "fail"
  } else if (any(c(n_zero, n_high, n_missing) > 0L)) {
    "warning"
  } else {
    "pass"
  }
  tibble::tibble(
    quality_status = status,
    n_duplicate_key_rows = as.integer(n_duplicates),
    n_invalid_municipality_codes = as.integer(n_invalid_code),
    n_invalid_years = as.integer(n_invalid_year),
    n_invalid_months = as.integer(n_invalid_month),
    n_missing_pm25 = as.integer(n_missing),
    n_negative_pm25 = as.integer(n_negative),
    n_zero_pm25 = as.integer(n_zero),
    n_above_plausibility_limit = as.integer(n_high),
    plausibility_limit = as.numeric(limit)
  )
}

.vigiar_monthly_quality_by_period <- function(data, limit) {
  valid <- !is.na(data$ano) & !is.na(data$mes) & data$mes %in% 1:12
  periods <- unique(data[valid, c("ano", "mes"), drop = FALSE])
  periods <- periods[order(periods$ano, periods$mes), , drop = FALSE]
  if (nrow(periods) == 0L) {
    out <- .vigiar_monthly_quality(data[0, , drop = FALSE], limit)[0, ]
    out$ano <- integer()
    out$mes <- integer()
    return(out[c("ano", "mes", setdiff(names(out), c("ano", "mes")))])
  }
  rows <- lapply(seq_len(nrow(periods)), function(i) {
    keep <- data$ano == periods$ano[[i]] & data$mes == periods$mes[[i]]
    row <- .vigiar_monthly_quality(data[keep, , drop = FALSE], limit)
    row$ano <- periods$ano[[i]]
    row$mes <- periods$mes[[i]]
    row[c("ano", "mes", setdiff(names(row), c("ano", "mes")))]
  })
  dplyr::bind_rows(rows)
}

.vigiar_monthly_domain <- function(data, completeness) {
  valid <- !is.na(data$ano) & !is.na(data$mes) & data$mes %in% 1:12
  periods <- unique(data[valid, c("ano", "mes"), drop = FALSE])
  dates <- as.Date(sprintf("%04d-%02d-01", periods$ano, periods$mes))
  tibble::tibble(
    periodo_inicio = if (length(dates) == 0L) as.Date(NA) else min(dates),
    periodo_fim = if (length(dates) == 0L) as.Date(NA) else max(dates),
    n_anos = length(unique(periods$ano)),
    n_meses_calendario = length(unique(periods$mes)),
    n_periodos_observados = nrow(periods),
    n_periodos_esperados = nrow(completeness),
    n_periodos_completos = sum(completeness$completo %in% TRUE),
    n_periodos_incompletos = sum(completeness$completo %in% FALSE)
  )
}

.vigiar_monthly_missing <- function(completeness) {
  rows <- lapply(seq_len(nrow(completeness)), function(i) {
    codes <- completeness$codigos_ausentes[[i]]
    if (length(codes) == 0L) {
      return(NULL)
    }
    registry <- RJ_MUNICIPIOS[
      match(codes, RJ_MUNICIPIOS$codigo_ibge_6),
    ]
    registry$ano <- completeness$ano[[i]]
    registry$mes <- completeness$mes[[i]]
    registry[c(
      "ano", "mes", "codigo_ibge_6", "codigo_ibge_7", "municipio",
      "regiao_saude", "macrorregiao_saude"
    )]
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(tibble::tibble(
      ano = integer(), mes = integer(), codigo_ibge_6 = integer(),
      codigo_ibge_7 = integer(), municipio = character(),
      regiao_saude = character(), macrorregiao_saude = character()
    ))
  }
  tibble::as_tibble(do.call(rbind.data.frame, rows))
}

.vigiar_require_monthly <- function(analysis) {
  reasons <- character()
  if (analysis$n_failed_partitions > 0L) {
    reasons <- c(reasons, sprintf(
      "%d download partition(s) failed", analysis$n_failed_partitions
    ))
  }
  if (!identical(analysis$schema_status, "pass")) {
    reasons <- c(reasons, paste0("critical schema status is ", analysis$schema_status))
  }
  if (!identical(analysis$truncation_status, "no_evidence")) {
    reasons <- c(reasons, paste0("truncation status is ", analysis$truncation_status))
  }
  if (!identical(analysis$parser_status, "pass")) {
    reasons <- c(reasons, paste0("parser status is ", analysis$parser_status))
  }
  if (any(analysis$completeness$completo %in% FALSE)) {
    reasons <- c(reasons, sprintf(
      "%d municipality-month group(s) are incomplete",
      sum(analysis$completeness$completo %in% FALSE)
    ))
  }
  if (!identical(analysis$quality$quality_status[[1]], "pass")) {
    reasons <- c(reasons, paste0(
      "monthly quality status is ", analysis$quality$quality_status[[1]]
    ))
  }
  if (!identical(analysis$conclusion, "complete")) {
    reasons <- c(reasons, paste0("audit conclusion is ", analysis$conclusion))
  }
  if (length(reasons) > 0L) {
    stop(
      "Monthly RJ data are not verified complete: ",
      paste(unique(reasons), collapse = "; "), ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.vigiar_rj_partition_plan <- function(tabela, por, anos = NULL,
                                      meses = NULL, municipios = NULL) {
  schema <- .vigiar_env$esquema[[tabela]]
  if (is.null(schema)) {
    stop("Table is absent from the active VIGIAR schema: ", tabela,
         call. = FALSE)
  }
  years <- .vigiar_monthly_integer_values(
    anos, "anos", 2000L, as.integer(format(Sys.Date(), "%Y"))
  )
  months <- .vigiar_monthly_integer_values(meses, "meses", 1L, 12L)
  year_column <- intersect(c("ano", "ANO", "year"), names(schema))[1]
  month_column <- intersect(c("mes", "MES", "month"), names(schema))[1]

  temporal_filter <- function(year = NULL, month = NULL) {
    filters <- list()
    if (!is.null(year)) {
      if (is.na(year_column)) {
        stop("Table '", tabela, "' has no validated server-side year field.",
             call. = FALSE)
      }
      filters[[year_column]] <- .vigiar_partition_filter_value(
        schema[[year_column]], year
      )
    }
    if (!is.null(month)) {
      if (is.na(month_column)) {
        stop("Table '", tabela, "' has no validated server-side month field.",
             call. = FALSE)
      }
      filters[[month_column]] <- .vigiar_partition_filter_value(
        schema[[month_column]], month
      )
    }
    filters
  }

  if (por == "ano") {
    if (is.null(years)) {
      stop("Year partitioning requires explicit 'anos'.", call. = FALSE)
    }
    return(lapply(years, function(year) {
      list(
        label = sprintf("ano=%d", year),
        ano = year, mes = NA_integer_, codigo_ibge_6 = NA_integer_,
        filtros = temporal_filter(year = year)
      )
    }))
  }
  if (por == "mes") {
    if (is.null(years)) {
      stop("Month partitioning requires explicit 'anos'.", call. = FALSE)
    }
    months <- months %||% 1:12
    grid <- expand.grid(
      ano = years, mes = months, KEEP.OUT.ATTRS = FALSE
    )
    grid <- grid[order(grid$ano, grid$mes), , drop = FALSE]
    return(lapply(seq_len(nrow(grid)), function(i) {
      list(
        label = sprintf("ano=%d,mes=%02d", grid$ano[[i]], grid$mes[[i]]),
        ano = as.integer(grid$ano[[i]]),
        mes = as.integer(grid$mes[[i]]),
        codigo_ibge_6 = NA_integer_,
        filtros = temporal_filter(grid$ano[[i]], grid$mes[[i]])
      )
    }))
  }

  codes <- if (is.null(municipios)) {
    RJ_MUNICIPIOS$codigo_ibge_6
  } else {
    .vigiar_normalizar_codigo_municipio(municipios, formato = "6")
  }
  if (anyNA(codes) || any(!codes %in% RJ_MUNICIPIOS$codigo_ibge_6)) {
    stop("'municipios' must contain valid RJ municipality codes.",
         call. = FALSE)
  }
  codes <- sort(unique(codes))
  lapply(codes, function(code) {
    municipality_filter <- .vigiar_filtro_servidor_municipio(tabela, code)
    if (is.null(municipality_filter)) {
      stop(
        "Table '", tabela,
        "' has no validated server-side municipality field.",
        call. = FALSE
      )
    }
    list(
      label = sprintf("municipio=%06d", code),
      ano = NA_integer_, mes = NA_integer_, codigo_ibge_6 = code,
      filtros = .vigiar_combinar_filtros(
        municipality_filter,
        temporal_filter(year = years, month = months)
      )
    )
  })
}

.vigiar_partition_filter_value <- function(schema_column, value) {
  type <- tolower(.vigiar_schema_column_type(schema_column) %||% "")
  if (grepl("string|text", type)) as.character(value) else as.integer(value)
}

.vigiar_worst_truncation <- function(status) {
  rank <- c(
    no_evidence = 0L, unknown = 1L, possible = 2L,
    probable = 3L, confirmed = 4L
  )
  status[is.na(status) | !status %in% names(rank)] <- "unknown"
  names(rank[status])[[which.max(rank[status])]]
}
