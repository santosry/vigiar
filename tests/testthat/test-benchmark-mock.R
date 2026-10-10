# vigiar: performance benchmarks with mocked downloads

test_that("vigiar_benchmark measures a strategy", {
  .vigiar_ready_session()
  local_mocked_bindings(
    vigiar_baixar = function(tabela, ...) data.frame(a = 1:5),
    .package = "vigiar"
  )
  res <- suppressMessages(
    vigiar_benchmark("df_anual", strategies = "direct", repeticoes = 1L, timeout = 5)
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(res$estrategia, "direct")
  expect_equal(res$taxa_sucesso, 1)
  expect_equal(res$n_linhas, 5)
})

test_that("vigiar_benchmark records failures without aborting", {
  .vigiar_ready_session()
  local_mocked_bindings(
    vigiar_baixar = function(tabela, ...) stop("falha simulada"),
    .package = "vigiar"
  )
  res <- suppressMessages(
    vigiar_benchmark("df_anual", strategies = "direct", repeticoes = 1L, timeout = 5)
  )
  expect_equal(res$taxa_sucesso, 0)
  expect_true(is.na(res$tempo_min))
})

test_that("vigiar_benchmark validates table and session", {
  .vigiar_local_reset()
  expect_error(vigiar_benchmark("df_anual"), "Nenhuma sessao ativa")

  .vigiar_ready_session()
  expect_error(vigiar_benchmark("nao_existe"), "nao encontrada")
})

test_that("vigiar_benchmark_tabelas summarises multiple tables", {
  .vigiar_ready_session()
  local_mocked_bindings(
    vigiar_baixar = function(tabela, ...) data.frame(a = 1:3),
    .package = "vigiar"
  )
  res <- suppressMessages(
    vigiar_benchmark_tabelas(c("df_anual", "df_muni"), repeticoes = 1L, timeout = 5)
  )
  expect_setequal(res$tabela, c("df_anual", "df_muni"))
  expect_true(all(res$status == "OK"))
})

test_that("vigiar_health_check returns a full report when online", {
  .vigiar_local_reset()
  local_mocked_bindings(
    vigiar_conectar = function(...) .vigiar_fake_sessao(),
    vigiar_tabelas = function() c("df_anual", "df_muni"),
    vigiar_benchmark_tabelas = function(...) tibble::tibble(tabela = "df_anual", status = "OK"),
    vigiar_validar_dicionario = function() list(coverage_pct = 90),
    .package = "vigiar"
  )
  res <- suppressMessages(vigiar_health_check(timeout = 5))
  expect_true(res$online)
  expect_equal(res$n_tables, 2L)
  expect_s3_class(res$benchmark, "tbl_df")
})

test_that(".vigiar_benchmark_minimal_columns maps known tables", {
  expect_equal(
    .vigiar_benchmark_minimal_columns("df_anual"),
    c("muni", "UF", "ano", "Media_pm25")
  )
  expect_null(.vigiar_benchmark_minimal_columns("tabela_desconhecida"))
})
