# vigiar: download orchestration with mocked network + parser

test_that("vigiar_tabelas and vigiar_esquema require a session", {
  .vigiar_local_reset()
  expect_error(vigiar_tabelas(), "Nenhuma sessao ativa")
  expect_error(vigiar_esquema(), "Nenhuma sessao ativa")
})

test_that("vigiar_tabelas lists schema tables", {
  .vigiar_ready_session()
  expect_setequal(vigiar_tabelas(), c("df_anual", "df_muni", "pop"))
})

test_that("vigiar_esquema prints one table or the whole schema", {
  .vigiar_ready_session()
  expect_output(vigiar_esquema("df_anual"), "Tabela: df_anual")
  expect_output(vigiar_esquema(), "colunas")
})

test_that("vigiar_baixar queries, parses and filters to RJ", {
  .vigiar_ready_session()
  local_mocked_bindings(
    .vigiar_executar_query = function(sessao, query, timeout) list(fake = TRUE),
    .vigiar_parse_dados = function(resposta, tabela) {
      tibble::tibble(
        muni = c(330455L, 355030L),
        ano = c(2022L, 2022L),
        Media_pm25 = c(1, 2)
      )
    },
    .package = "vigiar"
  )

  res <- suppressMessages(vigiar_baixar("df_anual"))
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)
  expect_equal(res$muni, 330455L)
  expect_true(nrow(vigiar_log()) >= 1)
})

test_that("vigiar_baixar skips the UF filter when uf = NULL", {
  .vigiar_ready_session()
  local_mocked_bindings(
    .vigiar_executar_query = function(sessao, query, timeout) list(fake = TRUE),
    .vigiar_parse_dados = function(resposta, tabela) {
      tibble::tibble(muni = c(330455L, 355030L), ano = 2022L)
    },
    .package = "vigiar"
  )
  res <- suppressMessages(vigiar_baixar("df_anual", uf = NULL))
  expect_equal(nrow(res), 2L)
})

test_that("vigiar_baixar warns about possible API truncation", {
  .vigiar_ready_session()
  local_mocked_bindings(
    .vigiar_executar_query = function(sessao, query, timeout) list(fake = TRUE),
    .vigiar_parse_dados = function(resposta, tabela) {
      tibble::tibble(muni = rep(330455L, 29000L), ano = 2022L)
    },
    .package = "vigiar"
  )
  expect_warning(
    suppressMessages(vigiar_baixar("df_anual", uf = NULL)),
    "limitou a resposta"
  )
})

test_that("vigiar_baixar validates session, table and status", {
  .vigiar_local_reset()
  expect_error(suppressMessages(vigiar_baixar("df_anual")), "Nenhuma sessao ativa")

  .vigiar_env$sessao <- .vigiar_fake_sessao()
  .vigiar_env$esquema <- .vigiar_fake_esquema()
  .vigiar_env$status_verificado <- FALSE
  expect_error(vigiar_baixar("df_anual"), "vigiar_status")

  .vigiar_env$status_verificado <- TRUE
  expect_error(vigiar_baixar("nao_existe"), "nao encontrada")
})

test_that("vigiar_baixar_tudo downloads a set of tables", {
  .vigiar_ready_session()
  local_mocked_bindings(
    vigiar_baixar = function(tabela, ...) tibble::tibble(tabela = tabela),
    .package = "vigiar"
  )
  res <- suppressMessages(
    vigiar_baixar_tudo(c("df_anual", "df_muni"), progress = FALSE, delay = 0)
  )
  expect_named(res, c("df_anual", "df_muni"))
  expect_equal(res$df_anual$tabela, "df_anual")
})

test_that("vigiar_baixar_tudo warns about unknown tables", {
  .vigiar_ready_session()
  local_mocked_bindings(
    vigiar_baixar = function(tabela, ...) tibble::tibble(tabela = tabela),
    .package = "vigiar"
  )
  expect_warning(
    suppressMessages(
      vigiar_baixar_tudo(c("df_anual", "nao_existe"), progress = FALSE, delay = 0)
    ),
    "nao encontradas"
  )
})

test_that("vigiar_baixar_tudo errors without a session", {
  .vigiar_local_reset()
  expect_error(vigiar_baixar_tudo(), "Nenhuma sessao ativa")
})

test_that("vigiar_baixar_principais downloads available key tables", {
  .vigiar_ready_session()
  local_mocked_bindings(
    vigiar_baixar = function(tabela, ...) tibble::tibble(tabela = tabela),
    .package = "vigiar"
  )
  res <- suppressMessages(vigiar_baixar_principais())
  expect_true(all(names(res) %in% c("df_anual", "df_muni", "pop")))
  expect_gt(length(res), 0)
})

test_that("vigiar_info joins the schema with the catalogue", {
  .vigiar_ready_session()
  info <- vigiar_info()
  expect_s3_class(info, "tbl_df")
  expect_true(all(c("tabela", "colunas", "descricao", "categoria") %in% names(info)))
  expect_true("df_anual" %in% info$tabela)
})

test_that("vigiar_checar_dados reports diagnostics", {
  dados <- data.frame(a = c(1, NA, 1), b = c("x", "y", "x"))
  expect_output(checks <- vigiar_checar_dados(dados, tabela = "t"), "Diagnostico")
  expect_equal(checks$n_rows, 3L)
  expect_equal(checks$n_cols, 2L)
  expect_true(checks$na_per_column[["a"]] == 1L)
  expect_equal(checks$duplicated_rows, 1L)
})

test_that("vigiar_diagnostico samples every table", {
  .vigiar_ready_session()
  local_mocked_bindings(
    vigiar_baixar = function(tabela, ...) {
      data.frame(muni = 330455L, ano = 2022L)
    },
    .package = "vigiar"
  )
  res <- suppressMessages(vigiar_diagnostico(amostra = 5))
  expect_setequal(names(res), names(.vigiar_env$esquema))
})

test_that(".vigiar_catalogo returns the table catalogue", {
  cat0 <- .vigiar_catalogo()
  expect_s3_class(cat0, "data.frame")
  expect_true(all(c("tabela", "descricao", "categoria") %in% names(cat0)))
  expect_gt(nrow(cat0), 20)
})
