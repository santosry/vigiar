# vigiar: dictionary <-> live schema auditing

test_that("vigiar_tabelas_documentadas lists dictionary tables", {
  tabs <- vigiar_tabelas_documentadas()
  expect_type(tabs, "character")
  expect_true(length(tabs) > 0)
  expect_false(anyDuplicated(tabs) > 0)
})

test_that("vigiar_variaveis_nao_documentadas finds undocumented columns", {
  .vigiar_local_reset()
  .vigiar_env$esquema <- list(
    df_anual = list(ano = list(), coluna_nao_documentada = list())
  )

  res <- vigiar_variaveis_nao_documentadas()
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("tabela", "coluna", "problema"))
  expect_true("coluna_nao_documentada" %in% res$coluna)
})

test_that("vigiar_variaveis_nao_documentadas returns empty when fully documented", {
  .vigiar_local_reset()
  dict <- vigiar_dicionario()
  tab <- dict$table_id[1]
  cols <- dict$original_name[dict$table_id == tab]
  esquema <- stats::setNames(list(as.list(cols)), tab)
  .vigiar_env$esquema <- esquema

  res <- vigiar_variaveis_nao_documentadas()
  expect_equal(nrow(res), 0)
})

test_that("vigiar_variaveis_orfas detects orphan tables and columns", {
  .vigiar_local_reset()
  .vigiar_env$esquema <- list(df_anual = list(ano = list()))

  res <- vigiar_variaveis_orfas()
  expect_s3_class(res, "tbl_df")
  expect_true(any(res$coluna == "(tabela inteira)"))
})

test_that("dictionary audit helpers error without a session", {
  .vigiar_local_reset()
  expect_error(vigiar_variaveis_nao_documentadas(), "Nenhuma sessao ativa")
  expect_error(vigiar_variaveis_orfas(), "Nenhuma sessao ativa")
  expect_error(vigiar_validar_dicionario(), "Nenhuma sessao ativa")
  expect_error(vigiar_comparar_schema(), "Nenhuma sessao ativa")
})

test_that("vigiar_validar_dicionario reports coverage", {
  .vigiar_local_reset()
  .vigiar_env$esquema <- .vigiar_fake_esquema()

  res <- expect_output(
    vigiar_validar_dicionario(),
    "Cobertura do dicionario"
  )
  expect_true(all(c("undocumented", "orfas", "coverage_pct") %in% names(res)))
  expect_type(res$coverage_pct, "double")
})

test_that("vigiar_comparar_schema compares live and documented columns", {
  .vigiar_local_reset()
  .vigiar_env$esquema <- .vigiar_fake_esquema()

  res <- vigiar_comparar_schema()
  expect_s3_class(res, "tbl_df")
  expect_true(all(c(
    "tabela", "colunas_live", "colunas_dict",
    "documentadas", "nao_documentadas", "orfas", "coverage"
  ) %in% names(res)))
  expect_true("df_anual" %in% res$tabela)
})
