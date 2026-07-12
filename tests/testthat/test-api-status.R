test_that("every public symbol has one explicit lifecycle state", {
  status <- vigiar_api_status()
  exports <- sort(getNamespaceExports("vigiar"))

  expect_s3_class(status, "tbl_df")
  expect_setequal(status$symbol, exports)
  expect_identical(anyDuplicated(status$symbol), 0L)
  expect_true(all(status$status %in% c("stable", "experimental", "deprecated")))
  expect_false(any(startsWith(status$symbol, ".")))
  expect_true(all(nzchar(status$guidance)))
})

test_that("scientifically sensitive and compatibility APIs are not mislabeled", {
  status <- vigiar_api_status()
  lookup <- setNames(status$status, status$symbol)

  expect_identical(lookup[["vigiar_auditar_rj_online"]], "experimental")
  expect_identical(lookup[["vigiar_baixar_rj_completo"]], "experimental")
  expect_identical(lookup[["vigiar_health_check"]], "experimental")
  expect_identical(lookup[["vigiar_rj_macrorregioes"]], "deprecated")
  expect_identical(lookup[["vigiar_baixar"]], "stable")
  expect_identical(lookup[["vigiar_rj_cobertura"]], "stable")
})

test_that("known internal implementation symbols are not exported", {
  exports <- getNamespaceExports("vigiar")
  internal <- c(
    ".vigiar_parse_dados",
    ".vigiar_construir_query",
    ".vigiar_detectar_truncamento",
    ".vigiar_normalizar_codigo_municipio",
    "RJ_MUNICIPIOS",
    "VIGIAR_RESOURCE_KEY"
  )
  expect_length(intersect(exports, internal), 0L)
})
