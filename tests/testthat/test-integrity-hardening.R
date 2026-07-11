# Reproducibility, cache, audit, and truncation hardening

.integrity_rj_panel <- function() {
  data.frame(
    cod_municipio = vigiar_rj_municipios()$codigo_ibge_6,
    ano = 2022L
  )
}

test_that("truncation assessment records status and evidence", {
  clean <- .vigiar_detectar_truncamento(data.frame(x = 1:10), tabela = "small")
  expect_identical(attr(clean, "vigiar_truncation_status"), "no_evidence")
  expect_false(attr(clean, "vigiar_possivel_truncamento"))

  threshold <- suppressWarnings(.vigiar_detectar_truncamento(
    data.frame(x = seq_len(28500L)),
    tabela = "large"
  ))
  expect_identical(attr(threshold, "vigiar_truncation_status"), "possible")
  expect_match(
    paste(attr(threshold, "vigiar_truncation_evidence"), collapse = " "),
    "heuristic"
  )

  limited <- suppressWarnings(.vigiar_detectar_truncamento(
    data.frame(x = 1:100),
    tabela = "limited",
    limite = 100L
  ))
  expect_identical(attr(limited, "vigiar_truncation_status"), "probable")

  confirmed_input <- data.frame(x = 1:2)
  attr(confirmed_input, "vigiar_response_metadata") <- list(has_more = TRUE)
  confirmed <- suppressWarnings(.vigiar_detectar_truncamento(
    confirmed_input,
    tabela = "continued"
  ))
  expect_identical(attr(confirmed, "vigiar_truncation_status"), "confirmed")
})

test_that("strict completeness rejects every material truncation state", {
  dados <- .integrity_rj_panel()
  for (status in c("possible", "probable", "confirmed", "unknown")) {
    candidate <- dados
    attr(candidate, "vigiar_truncation_status") <- status
    attr(candidate, "vigiar_possivel_truncamento") <- status != "no_evidence"
    expect_error(
      suppressWarnings(vigiar_rj_completude_tabela(
        candidate,
        tabela = "df_anual",
        require_complete = TRUE
      )),
      "truncation"
    )
  }
})

test_that("canonical checksums preserve precision and semantic identity", {
  base <- data.frame(
    b = c(2, 1),
    a = c("y", "x"),
    stringsAsFactors = FALSE
  )
  rows_reordered <- base[2:1, ]
  cols_reordered <- base[c("a", "b")]

  expect_identical(vigiar_checksum(base), vigiar_checksum(rows_reordered))
  expect_identical(vigiar_checksum(base), vigiar_checksum(cols_reordered))
  expect_false(identical(
    vigiar_checksum(base, mode = "ordered"),
    vigiar_checksum(rows_reordered, mode = "ordered")
  ))

  factor_data <- data.frame(x = factor(c("a", "b")))
  character_data <- data.frame(x = c("a", "b"))
  expect_identical(vigiar_checksum(factor_data), vigiar_checksum(character_data))
  expect_identical(
    vigiar_checksum(data.frame(x = 1:2)),
    vigiar_checksum(data.frame(x = c(1, 2)))
  )

  precise <- data.frame(x = 1)
  changed <- data.frame(x = 1 + 1e-12)
  expect_false(identical(vigiar_checksum(precise), vigiar_checksum(changed)))

  special <- data.frame(x = c(NA_real_, NaN, Inf, -Inf))
  special_changed <- data.frame(x = c(NaN, NA_real_, Inf, -Inf))
  expect_false(identical(vigiar_checksum(special, mode = "ordered"),
                         vigiar_checksum(special_changed, mode = "ordered")))
})

test_that("canonical checksums normalize time zones and keep dates distinct", {
  utc <- as.POSIXct("2022-01-01 12:00:00", tz = "UTC")
  sao_paulo <- as.POSIXct("2022-01-01 09:00:00", tz = "America/Sao_Paulo")
  expect_identical(
    vigiar_checksum(data.frame(time = utc)),
    vigiar_checksum(data.frame(time = sao_paulo))
  )
  expect_false(identical(
    vigiar_checksum(data.frame(day = as.Date("2022-01-01"))),
    vigiar_checksum(data.frame(day = as.Date("2022-01-02")))
  ))
})

test_that("snapshots store versioned data, schema, and metadata checksums", {
  snapshot <- vigiar_snapshot(
    data.frame(x = 1:3),
    tabela = "test",
    filtros = list(ano = 2022L)
  )
  expect_true(all(c(
    "data_checksum", "schema_checksum", "metadata_checksum",
    "canonicalization_version"
  ) %in% names(snapshot)))
  expect_identical(snapshot$checksum_sha256, snapshot$data_checksum)
  expect_true(vigiar_verificar_snapshot(snapshot))

  path <- tempfile(fileext = ".rds")
  vigiar_salvar_snapshot(snapshot, path)
  loaded <- vigiar_carregar_snapshot(path)
  expect_identical(loaded$canonicalization_version,
                   snapshot$canonicalization_version)
  expect_identical(loaded$metadata_checksum, snapshot$metadata_checksum)
})

test_that("cache keys include query, schema, and algorithm provenance", {
  old_session <- .vigiar_env$sessao
  old_schema <- .vigiar_env$esquema
  old_cache <- .vigiar_env$cache_dir
  on.exit({
    .vigiar_env$sessao <- old_session
    .vigiar_env$esquema <- old_schema
    .vigiar_env$cache_dir <- old_cache
  }, add = TRUE)
  .vigiar_env$sessao <- list(model_id = 1L, created_at = Sys.time())
  .vigiar_env$esquema <- list(test = list(
    ano = list(tipo = "integer"),
    value = list(tipo = "numeric")
  ))
  cache_dir <- tempfile("vigiar-cache-")
  dir.create(cache_dir)
  .vigiar_env$cache_dir <- cache_dir
  calls <- 0L

  testthat::local_mocked_bindings(
    vigiar_baixar = function(...) {
      calls <<- calls + 1L
      tibble::tibble(ano = 2022L, value = calls)
    },
    .package = "vigiar"
  )

  first <- vigiar_baixar_com_cache(
    "test", filtros = list(ano = 2022L), limite = 10L, ordenar_por = "ano"
  )
  second <- vigiar_baixar_com_cache(
    "test", filtros = list(ano = 2022L), limite = 10L, ordenar_por = "ano"
  )
  expect_equal(calls, 1L)
  expect_identical(attr(first, "vigiar_cache_status"), "miss")
  expect_identical(attr(second, "vigiar_cache_status"), "hit")

  vigiar_baixar_com_cache(
    "test", filtros = list(ano = 2021L), limite = 10L, ordenar_por = "ano"
  )
  vigiar_baixar_com_cache(
    "test", filtros = list(ano = 2022L), limite = 20L, ordenar_por = "ano"
  )
  vigiar_baixar_com_cache(
    "test", filtros = list(ano = 2022L), limite = 10L, ordenar_por = "value"
  )
  expect_equal(calls, 4L)

  .vigiar_env$esquema$test$value$tipo <- "character"
  vigiar_baixar_com_cache(
    "test", filtros = list(ano = 2022L), limite = 10L, ordenar_por = "ano"
  )
  expect_equal(calls, 5L)
})

test_that("audit unknown states never become a pass", {
  old_schema <- .vigiar_env$esquema
  on.exit({ .vigiar_env$esquema <- old_schema }, add = TRUE)
  .vigiar_env$esquema <- NULL

  audit <- vigiar_auditar(
    data.frame(value = 1),
    tabela = "unknown_table",
    verbose = FALSE
  )
  expect_false(audit$passed)
  expect_identical(audit$status, "unknown")
  expect_identical(audit$schema$status, "unknown")
  expect_identical(audit$ibge$status, "unknown")

  required <- vigiar_auditar(
    data.frame(ano = 2022L, value = 1),
    tabela = "df_anual",
    verbose = FALSE
  )
  expect_false(required$passed)
  expect_identical(required$status, "fail")
  expect_identical(required$coverage$status, "fail")
})

test_that("constant columns are anomalies rather than corruption failures", {
  result <- vigiar_compliance_check(
    data.frame(constant = rep("designed", 5), value = 1:5),
    profiles = "corrupcao",
    verbose = FALSE
  )
  expect_true(result$corrupcao$ok)
  expect_true("constant" %in% names(result$corrupcao$anomalies))
})

test_that("full schema comparison detects order without failing compatibility", {
  old_schema <- .vigiar_env$esquema
  on.exit({ .vigiar_env$esquema <- old_schema }, add = TRUE)
  locked <- list(a = list(tipo = "integer"), b = list(tipo = "numeric"))
  .vigiar_env$esquema <- list(test = locked[c("b", "a")])
  lock <- structure(list(
    locked_at = "2026-07-11",
    tabelas = "test",
    esquema = list(test = locked)
  ), class = "vigiar_schema_lock")

  diffs <- vigiar_esquema_verificar(lock, error = FALSE)
  expect_true("order_changes" %in% names(diffs))
  expect_identical(attr(diffs, "compatibility_status"), "pass")
  expect_identical(attr(diffs, "equality_status"), "fail")
})

test_that("logs redact credentials, cookies, tokens, and URL queries", {
  old_log <- .vigiar_env$log
  on.exit({ .vigiar_env$log <- old_log }, add = TRUE)
  .vigiar_env$log <- list()

  .vigiar_log(
    "INFO",
    "request token=topsecret https://example.test/path?resourceKey=secret",
    metadata = list(
      token = "topsecret",
      cookie = "session=private",
      headers = list(Authorization = "Bearer hidden"),
      url = "https://user:pass@example.test/path?token=private"
    )
  )
  log <- vigiar_log()
  exported <- paste(log$message, log$metadata_json, collapse = " ")
  expect_false(grepl("topsecret|private|hidden|user:pass", exported))
  expect_match(exported, "REDACTED")
})
