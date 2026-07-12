# vigiar: online integration tests
#
# These tests require internet access and a working VIGIAR dashboard.
# Run with:  VIGIAR_RUN_ONLINE_TESTS=true  R CMD check
# Or:        withr::local_envvar(VIGIAR_RUN_ONLINE_TESTS = "true")
#            devtools::test(filter = "online")

library(testthat)
library(vigiar)

online_tests <- identical(tolower(Sys.getenv("VIGIAR_RUN_ONLINE_TESTS")), "true")
if (!online_tests) {
  skip("Online tests disabled. Set VIGIAR_RUN_ONLINE_TESTS=true to run.")
}

test_that("online RJ audit validates table-grain completeness evidence", {
  vigiar_conectar()
  withr::defer(vigiar_desconectar(), testthat::teardown_env())

  strict <- identical(tolower(Sys.getenv("VIGIAR_REQUIRE_ONLINE_COMPLETENESS")), "true")
  tables <- c("df_anual", "df_mensal", "df_dias", "pop")
  report_dir <- Sys.getenv(
    "VIGIAR_ONLINE_REPORT_DIR",
    unset = file.path(tempdir(), "vigiar-online-rj-report")
  )
  expected_start <- Sys.getenv("VIGIAR_EXPECTED_PERIOD_START", unset = "")
  expected_end <- Sys.getenv("VIGIAR_EXPECTED_PERIOD_END", unset = "")
  expected_domains <- NULL
  if (nzchar(expected_start) && nzchar(expected_end)) {
    expected_domains <- stats::setNames(lapply(tables, function(x) list(
      periodo_inicio = expected_start,
      periodo_fim = expected_end
    )), tables)
  }

  audit <- vigiar_auditar_rj_online(
    tabelas = tables,
    salvar = TRUE,
    dir = report_dir,
    require_complete = strict,
    timeout = 240,
    dominios_esperados = expected_domains
  )

  expect_s3_class(audit, "tbl_df")
  expect_s3_class(audit, "vigiar_online_audit")
  expect_setequal(audit$tabela, tables)
  expect_true(all(audit$conclusion %in% c(
    "complete", "complete_within_inferred_domain",
    "complete_within_observed_domain", "partial", "truncated",
    "schema_changed", "schema_unverified", "failed"
  )))
  expect_false(any(audit$conclusion == "failed"))
  expect_true(all(!is.na(audit$checksum)))
  expect_true(all(nchar(audit$checksum) > 0))
  expect_true(all(audit$n_rows > 0))
  expect_true(all(audit$n_cols > 0))
  expect_true(all(audit$n_municipios_presentes > 0))
  expect_true(all(audit$n_municipios_presentes <= 92))
  expect_true(all(audit$n_municipios_esperados == 92))
  expect_true(all(audit$truncation_status %in% c(
    "no_evidence", "possible", "probable", "confirmed", "unknown"
  )))
  expect_true(all(audit$parser_status == "pass"))
  expect_true(all(audit$parser_issue_count == 0L))
  expect_true(all(!nzchar(audit$parser_issues)))
  expect_true(all(audit$truncation_status == "no_evidence"))
  expect_true(all(audit$schema_status %in% c("pass", "fail", "unknown")))
  expect_true(all(audit$schema_status == "pass"))
  expect_true(all(audit$spatial_coverage_status %in% c("complete", "incomplete")))
  expect_true(all(audit$panel_completeness_status %in% c(
    "complete", "incomplete", "unknown"
  )))
  expect_true(all(audit$overall_status %in% c("pass", "fail", "unknown")))
  expect_true(all(nchar(audit$verification_status) > 0L))

  complete_rows <- audit$conclusion == "complete"
  if (any(complete_rows)) {
    expect_false(is.null(expected_domains))
    expect_true(all(audit$overall_status[complete_rows] == "pass"))
    expect_true(all(audit$truncation_status[complete_rows] == "no_evidence"))
    expect_true(all(audit$schema_status[complete_rows] == "pass"))
    expect_true(all(audit$campos_dos_goytacazes_presente[complete_rows]))
  }
  missing_campos <- !audit$campos_dos_goytacazes_presente
  expect_false(any(audit$conclusion[missing_campos] == "complete"))
  truncated <- audit$truncation_status %in% c("possible", "probable", "confirmed")
  expect_true(all(audit$conclusion[truncated] == "truncated"))
  partial <- audit$conclusion == "partial"
  expect_true(all(audit$n_incomplete_groups[partial] > 0L))

  expect_equal(
    audit$completeness_grade[audit$tabela == "df_anual"],
    "municipio x ano"
  )
  expect_equal(
    audit$completeness_grade[audit$tabela == "df_mensal"],
    "municipio x ano x mes"
  )
  expect_true(
    audit$completeness_grade[audit$tabela == "df_dias"] %in%
      c("municipio x ano x mes", "municipio x ano")
  )
  expect_equal(
    audit$completeness_grade[audit$tabela == "pop"],
    "municipio x ano"
  )

  for (i in seq_len(nrow(audit))) {
    expect_s3_class(audit$cobertura_geral[[i]], "tbl_df")
    expect_s3_class(audit$completude_tabela[[i]], "tbl_df")
    expect_s3_class(audit$municipios_ausentes[[i]], "tbl_df")
    expect_gt(nrow(audit$completude_tabela[[i]]), 0)
    expect_equal(
      nrow(audit$municipios_ausentes[[i]]),
      audit$n_municipios_ausentes[[i]]
    )
  }

  out_dir <- attr(audit, "vigiar_audit_dir")
  expect_true(file.exists(file.path(out_dir, "manifest.csv")))
  expect_true(file.exists(file.path(out_dir, "manifest.json")))
  expect_true(file.exists(file.path(out_dir, "audit.rds")))

  message(
    "RJ online audit reports: ",
    normalizePath(out_dir, winslash = "/", mustWork = FALSE)
  )
})
