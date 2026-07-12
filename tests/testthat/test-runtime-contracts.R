test_that("structured logs expose stable events without session secrets", {
  .vigiar_env$log <- list()
  .vigiar_log(
    "INFO",
    "Connecting to https://user:password@example.org/path?token=secret",
    metadata = list(
      cookie = "session=secret",
      authorization = "Bearer secret",
      endpoint = "https://example.org/path?resourceKey=secret"
    ),
    event = "connect_start"
  )

  log <- vigiar_log()
  expect_identical(log$event, "connect_start")
  serialized <- paste(log$message, log$metadata_json)
  expect_false(grepl("password|session=secret|Bearer secret|token=secret", serialized))
  expect_true(grepl("REDACTED", serialized, fixed = TRUE))
})

test_that("retry attempts and exhaustion are recorded as events", {
  .vigiar_env$log <- list()
  expect_error(
    .vigiar_retry(
      stop("temporary failure"),
      max_tries = 2L,
      initial_delay = 0,
      context = "offline-test"
    ),
    "All 2 attempts failed"
  )
  expect_identical(vigiar_log()$event, c("retry", "retry_exhausted"))
})

test_that("benchmarks exclude warm-up and report robust reproducibility metrics", {
  old_session <- .vigiar_env$sessao
  old_schema <- .vigiar_env$esquema
  on.exit({
    .vigiar_env$sessao <- old_session
    .vigiar_env$esquema <- old_schema
  }, add = TRUE)
  .vigiar_env$sessao <- list(created_at = Sys.time())
  .vigiar_env$esquema <- list(df_anual = list(
    muni = list(tipo = "integer"),
    UF = list(tipo = "character"),
    ano = list(tipo = "integer"),
    Media_pm25 = list(tipo = "numeric")
  ))
  calls <- 0L
  testthat::local_mocked_bindings(
    vigiar_baixar = function(...) {
      calls <<- calls + 1L
      tibble::tibble(
        muni = c(330100L, 330455L),
        UF = "RJ",
        ano = 2022L,
        Media_pm25 = c(10, 12)
      )
    },
    .package = "vigiar"
  )

  result <- vigiar_benchmark(
    "df_anual",
    strategies = c("direct", "two_ended_sample"),
    repeticoes = 3L,
    warmup = 1L,
    limite = 100L
  )
  expect_s3_class(result, "vigiar_benchmark")
  expect_equal(calls, 12L)
  expect_true(all(result$measured_runs == 3L))
  expect_true(all(result$warmup_runs == 1L))
  expect_true(all(result$n_success == 3L))
  expect_true(all(result$n_failure == 0L))
  expect_true(all(result$success_rate == 1))
  expect_true(all(!is.na(result$checksums)))
  expect_true(all(vapply(
    result$environment,
    function(x) !inherits(try(jsonlite::fromJSON(x), silent = TRUE), "try-error"),
    logical(1)
  )))
})

test_that("legacy two-ended benchmark name is explicitly deprecated", {
  old_session <- .vigiar_env$sessao
  old_schema <- .vigiar_env$esquema
  on.exit({
    .vigiar_env$sessao <- old_session
    .vigiar_env$esquema <- old_schema
  }, add = TRUE)
  .vigiar_env$sessao <- list(created_at = Sys.time())
  .vigiar_env$esquema <- list(df_anual = list(ano = list(tipo = "integer")))
  testthat::local_mocked_bindings(
    vigiar_baixar = function(...) tibble::tibble(ano = 2022L),
    .package = "vigiar"
  )
  expect_warning(
    out <- vigiar_benchmark(
      "df_anual", strategies = "year_asc_desc",
      repeticoes = 1L, warmup = 0L
    ),
    "does not prove completeness"
  )
  expect_identical(out$strategy, "two_ended_sample")
})

test_that("health reports keep connection, schema, and canary states separate", {
  old_session <- .vigiar_env$sessao
  old_schema <- .vigiar_env$esquema
  on.exit({
    .vigiar_env$sessao <- old_session
    .vigiar_env$esquema <- old_schema
  }, add = TRUE)
  withr::local_envvar(VIGIAR_RUN_ONLINE_TESTS = "true")
  .vigiar_env$sessao <- list(created_at = Sys.time())
  .vigiar_env$esquema <- list(df_anual = list(ano = list(tipo = "integer")))
  testthat::local_mocked_bindings(
    vigiar_esquema_verificar_critico = function(...) list(),
    vigiar_benchmark_tabelas = function(...) tibble::tibble(status = "pass"),
    .package = "vigiar"
  )

  report <- vigiar_health_check(timeout = 1)
  expect_s3_class(report, "vigiar_health_report")
  expect_identical(report$status, "pass")
  expect_true(report$ok)
  expect_identical(report$schema$status, "pass")
  expect_identical(report$canary$status, "pass")
})

test_that("health strict mode rejects partial canary evidence", {
  old_session <- .vigiar_env$sessao
  old_schema <- .vigiar_env$esquema
  on.exit({
    .vigiar_env$sessao <- old_session
    .vigiar_env$esquema <- old_schema
  }, add = TRUE)
  withr::local_envvar(VIGIAR_RUN_ONLINE_TESTS = "true")
  .vigiar_env$sessao <- list(created_at = Sys.time())
  .vigiar_env$esquema <- list(df_anual = list(ano = list(tipo = "integer")))
  testthat::local_mocked_bindings(
    vigiar_esquema_verificar_critico = function(...) list(),
    vigiar_benchmark_tabelas = function(...) tibble::tibble(status = "partial"),
    .package = "vigiar"
  )
  expect_error(
    vigiar_health_check(timeout = 1, require_healthy = TRUE),
    "health status is 'unknown'"
  )
})

test_that("coverage and completeness expose programmatic S3 summaries", {
  registry <- vigiar_rj_municipios()
  data <- tibble::tibble(
    codigo_ibge = registry$codigo_ibge_6,
    ano = 2022L,
    mes = 1L
  )
  coverage <- vigiar_rj_cobertura(data)
  expect_s3_class(coverage, "vigiar_coverage")
  expect_identical(summary(coverage)$status, "complete")

  attr(data, "vigiar_tabela") <- "df_mensal"
  completeness <- vigiar_rj_completude_tabela(
    data,
    anos_esperados = 2022L,
    meses_esperados = 1L
  )
  expect_s3_class(completeness, "vigiar_completeness")
  expect_identical(summary(completeness)$overall, "pass")
})
