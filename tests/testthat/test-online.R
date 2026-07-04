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

  audit <- suppressWarnings(vigiar_auditar_rj_online(
    tabelas = tables,
    salvar = TRUE,
    dir = report_dir,
    require_complete = strict,
    timeout = 240
  ))

  expect_s3_class(audit, "tbl_df")
  expect_setequal(audit$tabela, tables)
  expect_true(all(audit$conclusion %in% c(
    "complete", "partial", "truncated", "schema_changed", "failed"
  )))
  expect_false(any(audit$conclusion == "failed"))
  expect_true(all(!is.na(audit$checksum)))
  expect_true(all(nchar(audit$checksum) > 0))
  expect_true(all(audit$n_rows > 0))
  expect_true(all(audit$n_cols > 0))
  expect_true(all(audit$n_municipios_presentes > 0))
  expect_true(all(audit$n_municipios_presentes <= 92))
  expect_true(all(audit$n_municipios_esperados == 92))

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
