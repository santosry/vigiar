# vigiar: monthly RJ PM2.5 download and analysis tests

library(testthat)
library(vigiar)

.monthly_codes <- function(n = 92L) {
  utils::head(vigiar_rj_municipios()$codigo_ibge_6, n)
}

.monthly_raw <- function(codes = .monthly_codes(), years = 2022L,
                         months = 1:2, value = 12) {
  out <- expand.grid(
    muni = as.integer(codes),
    ano = as.integer(years),
    mes = as.integer(months),
    KEEP.OUT.ATTRS = FALSE
  )
  out$UF <- "RJ"
  out$pm25 <- rep(value, nrow(out))
  out$LAT <- -22
  out$LON <- -43
  out <- tibble::as_tibble(out)
  attr(out, "vigiar_parser_status") <- "pass"
  attr(out, "vigiar_parser_issues") <- character()
  attr(out, "vigiar_truncation_status") <- "no_evidence"
  attr(out, "vigiar_possivel_truncamento") <- FALSE
  attr(out, "vigiar_response_rows") <- nrow(out)
  attr(out, "vigiar_server_filter") <- list(source = "mock")
  out
}

.monthly_session <- function(code, schema_mutator = NULL) {
  old_session <- .vigiar_env$sessao
  old_schema <- .vigiar_env$esquema
  on.exit({
    .vigiar_env$sessao <- old_session
    .vigiar_env$esquema <- old_schema
  }, add = TRUE)
  schema <- list(
    df_mensal = list(
      muni = list(nome = "muni", tipo = "integer"),
      UF = list(nome = "UF", tipo = "character"),
      ano = list(nome = "ano", tipo = "integer"),
      mes = list(nome = "mes", tipo = "integer"),
      pm25 = list(nome = "pm25", tipo = "numeric"),
      LAT = list(nome = "LAT", tipo = "numeric"),
      LON = list(nome = "LON", tipo = "numeric")
    )
  )
  if (!is.null(schema_mutator)) {
    schema <- schema_mutator(schema)
  }
  .vigiar_env$sessao <- list(
    model_id = 1L,
    api_url = "https://example.test",
    created_at = Sys.time()
  )
  .vigiar_env$esquema <- schema
  force(code)
}

.monthly_mock_download <- function(data, captured = NULL) {
  force(data)
  force(captured)
  function(...) {
    args <- list(...)
    if (!is.null(captured)) {
      captured$calls <- c(captured$calls %||% list(), list(args))
    }
    out <- data
    filters <- args$filtros %||% list()
    for (name in intersect(names(filters), names(out))) {
      out <- out[out[[name]] %in% filters[[name]], , drop = FALSE]
    }
    attrs <- .vigiar_data_attributes(data)
    out <- tibble::as_tibble(out)
    out <- .vigiar_restore_data_attributes(out, attrs)
    attr(out, "vigiar_response_rows") <- nrow(out)
    out
  }
}

test_that("monthly RJ helper downloads and processes monthly means", {
  raw <- .monthly_raw()
  captured <- new.env(parent = emptyenv())
  captured$calls <- list()

  .monthly_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = .monthly_mock_download(raw, captured),
      .package = "vigiar"
    )
    out <- vigiar_baixar_pm25_mensal_rj(
      anos = 2022L,
      meses = 1:2,
      particionar = FALSE
    )

    expect_s3_class(out, "vigiar_pm25_monthly")
    expect_equal(nrow(out), 184L)
    expect_true(all(c("pm25_media", "periodo") %in% names(out)))
    expect_type(out$pm25_media, "double")
    expect_s3_class(out$periodo, "Date")
    expect_equal(sort(unique(out$mes)), 1:2)
    expect_equal(length(unique(out$codigo_ibge_6)), 92L)
    expect_identical(attr(out, "vigiar_tabela"), "df_mensal")
    expect_identical(attr(out, "vigiar_granularidade"), "municipio_ano_mes")
    expect_identical(attr(out, "vigiar_parser_status"), "pass")
    expect_equal(attr(out, "vigiar_response_rows"), 184L)
    expect_identical(captured$calls[[1]]$filtros$ano, 2022L)
    expect_identical(captured$calls[[1]]$filtros$mes, 1:2)

    completeness <- attr(out, "vigiar_rj_completude")
    expect_s3_class(completeness, "vigiar_completeness")
    expect_equal(nrow(completeness), 2L)
    expect_true(all(completeness$completo))
  })
})

test_that("strict monthly helper partitions and verifies every requested month", {
  raw <- .monthly_raw()
  captured <- new.env(parent = emptyenv())
  captured$calls <- list()

  .monthly_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = .monthly_mock_download(raw, captured),
      .package = "vigiar"
    )
    out <- vigiar_baixar_pm25_mensal_rj(
      anos = 2022L,
      meses = 1:2,
      require_complete = TRUE,
      delay = 0
    )

    expect_equal(nrow(out), 184L)
    expect_length(captured$calls, 2L)
    expect_setequal(
      vapply(captured$calls, function(x) x$filtros$mes, integer(1)),
      1:2
    )
    report <- attr(out, "vigiar_partition_report")
    expect_s3_class(report, "tbl_df")
    expect_equal(report$status, c("success", "success"))
    expect_identical(attr(out, "vigiar_verification_status"),
                     "verified_complete")
    expect_identical(
      attr(out, "vigiar_monthly_analysis")$conclusion,
      "complete"
    )
  })
})

test_that("partitioned monthly download retries transient failures", {
  raw <- .monthly_raw()
  attempts <- new.env(parent = emptyenv())
  attempts$by_month <- list()
  base_download <- .monthly_mock_download(raw)
  retrying_download <- function(...) {
    args <- list(...)
    month <- as.character(args$filtros$mes[[1]])
    attempts$by_month[[month]] <- (attempts$by_month[[month]] %||% 0L) + 1L
    if (month == "1" && attempts$by_month[[month]] == 1L) {
      stop("transient partition failure")
    }
    do.call(base_download, args)
  }

  .monthly_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = retrying_download,
      .package = "vigiar"
    )
    out <- vigiar_baixar_pm25_mensal_rj(
      anos = 2022L,
      meses = 1:2,
      require_complete = TRUE,
      tentativas = 2L,
      delay = 0
    )

    report <- attr(out, "vigiar_partition_report")
    analysis <- attr(out, "vigiar_monthly_analysis")
    expect_equal(report$attempts, c(2L, 1L))
    expect_true(all(report$status == "success"))
    expect_identical(analysis$partition_status, "pass")
    expect_equal(analysis$n_failed_partitions, 0L)
    expect_identical(analysis$conclusion, "complete")
  })
})

test_that("strict monthly download never hides a failed partition", {
  raw <- .monthly_raw()
  base_download <- .monthly_mock_download(raw)
  failing_download <- function(...) {
    args <- list(...)
    if (identical(args$filtros$mes, 2L)) {
      stop("persistent partition failure")
    }
    do.call(base_download, args)
  }

  .monthly_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = failing_download,
      .package = "vigiar"
    )
    expect_error(
      vigiar_baixar_pm25_mensal_rj(
        anos = 2022L,
        meses = 1:2,
        require_complete = TRUE,
        tentativas = 2L,
        delay = 0
      ),
      "1 partition\\(s\\) failed"
    )
  })
})

test_that("strict monthly helper requires an explicit expected domain", {
  .monthly_session({
    expect_error(
      vigiar_baixar_pm25_mensal_rj(require_complete = TRUE),
      "explicit expected years"
    )
  })
})

test_that("strict monthly helper rejects one incomplete month", {
  raw <- .monthly_raw()
  raw <- raw[!(raw$muni == 330100L & raw$mes == 2L), , drop = FALSE]

  .monthly_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = .monthly_mock_download(raw),
      .package = "vigiar"
    )
    expect_error(
      suppressWarnings(vigiar_baixar_pm25_mensal_rj(
        anos = 2022L,
        meses = 1:2,
        require_complete = TRUE,
        delay = 0
      )),
      "not verified complete"
    )
  })
})

test_that("strict monthly helper rejects a changed critical schema", {
  raw <- .monthly_raw()
  mutate_schema <- function(schema) {
    schema$df_mensal$pm25$tipo <- "character"
    schema
  }

  .monthly_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = .monthly_mock_download(raw),
      .package = "vigiar"
    )
    expect_error(
      vigiar_baixar_pm25_mensal_rj(
        anos = 2022L,
        meses = 1:2,
        require_complete = TRUE,
        delay = 0
      ),
      "not verified complete"
    )
  }, schema_mutator = mutate_schema)
})

test_that("monthly analysis exposes robust summaries and quality evidence", {
  raw <- .monthly_raw(months = 1:3)
  processed <- suppressWarnings(process_pm25(raw, tipo = "mensal"))
  analysis <- vigiar_analisar_pm25_mensal_rj(processed)

  expect_s3_class(analysis, "vigiar_monthly_analysis")
  expect_s3_class(analysis$summary, "tbl_df")
  expect_s3_class(analysis$temporal_domain, "tbl_df")
  expect_s3_class(analysis$by_year, "tbl_df")
  expect_s3_class(analysis$by_year_month, "tbl_df")
  expect_s3_class(analysis$by_calendar_month, "tbl_df")
  expect_s3_class(analysis$by_municipality, "tbl_df")
  expect_s3_class(analysis$by_health_region, "tbl_df")
  expect_s3_class(analysis$by_health_region_year_month, "tbl_df")
  expect_s3_class(analysis$completeness, "vigiar_completeness")
  expect_s3_class(analysis$missing_municipalities, "tbl_df")
  expect_s3_class(analysis$quality, "tbl_df")
  expect_s3_class(analysis$quality_by_year_month, "tbl_df")
  expect_s3_class(analysis$partition_report, "tbl_df")
  expect_s3_class(analysis$diagnostic, "vigiar_diagnostic")
  expect_equal(analysis$summary$n_observacoes, 276L)
  expect_equal(analysis$summary$n_municipios, 92L)
  expect_equal(nrow(analysis$by_year_month), 3L)
  expect_equal(nrow(analysis$quality_by_year_month), 3L)
  expect_equal(analysis$temporal_domain$n_periodos_completos, 3L)
  expect_identical(analysis$quality$quality_status, "pass")
  expect_identical(analysis$partition_status, "not_partitioned")
  expect_equal(analysis$n_failed_partitions, 0L)
  expect_match(analysis$scientific_scope, "no causal")
})

test_that("monthly analysis rejects duplicate keys and impossible values", {
  raw <- .monthly_raw(months = 1L)
  raw$pm25[[1]] <- -1
  raw <- dplyr::bind_rows(raw, raw[1, ])
  processed <- suppressWarnings(process_pm25(raw, tipo = "mensal"))
  analysis <- suppressWarnings(vigiar_analisar_pm25_mensal_rj(processed))

  expect_identical(analysis$quality$quality_status, "fail")
  expect_equal(analysis$quality$n_negative_pm25, 2L)
  expect_equal(analysis$quality$n_duplicate_key_rows, 2L)
  expect_identical(analysis$conclusion, "failed")
  expect_identical(analysis$overall_status, "fail")
})

test_that("monthly analysis records failed partition evidence", {
  processed <- suppressWarnings(process_pm25(
    .monthly_raw(months = 1L), tipo = "mensal"
  ))
  attr(processed, "vigiar_partition_report") <- tibble::tibble(
    partition = c("ano=2022,mes=01", "ano=2022,mes=02"),
    status = c("success", "failed")
  )
  analysis <- suppressWarnings(vigiar_analisar_pm25_mensal_rj(processed))

  expect_identical(analysis$partition_status, "fail")
  expect_equal(analysis$n_failed_partitions, 1L)
  expect_identical(analysis$conclusion, "failed")
})

test_that("monthly series breaks are ordered by calendar month", {
  data <- data.frame(
    cod_municipio = rep(330100L, 3),
    ano = rep(2022L, 3),
    mes = c(3L, 1L, 2L),
    pm25 = c(5, 5, 50)
  )
  diag <- new_vigiar_diagnostic("df_mensal", data)
  diag <- vigiar_checar_quebra_serie(
    diag, data, "cod_municipio", "ano", "pm25", col_mes = "mes"
  )

  expect_equal(diag$metricas$n_quebras_serie, 1L)
  messages <- vapply(diag$resultados, `[[`, "", "mensagem")
  expect_true(any(grepl("consecutive months", messages)))
})

test_that("generic strict RJ download catches an entirely absent month", {
  raw <- .monthly_raw(months = c(1L, 3L))

  .monthly_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = .monthly_mock_download(raw),
      .package = "vigiar"
    )
    expect_error(
      suppressWarnings(vigiar_baixar_rj(
        "df_mensal", require_complete = TRUE
      )),
      "municipality-time group"
    )
  })
})

test_that("processing preserves download provenance attributes", {
  raw <- .monthly_raw(months = 1L)
  attr(raw, "vigiar_requested_scope") <- "uf:RJ"
  attr(raw, "vigiar_query_strategy") <- "single_semantic_query"
  out <- process_pm25(raw, tipo = "mensal")

  expect_identical(attr(out, "vigiar_parser_status"), "pass")
  expect_identical(attr(out, "vigiar_requested_scope"), "uf:RJ")
  expect_identical(attr(out, "vigiar_query_strategy"),
                   "single_semantic_query")
  expect_equal(attr(out, "vigiar_response_rows"), 92L)
})
