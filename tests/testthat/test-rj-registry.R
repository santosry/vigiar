# vigiar: Rio de Janeiro municipality registry and aggregation

test_that("vigiar_rj_municipios returns the 92 official municipalities", {
  muni <- vigiar_rj_municipios()

  expect_s3_class(muni, "data.frame")
  expect_equal(nrow(muni), 92)
  expect_named(
    muni,
    c("codigo_ibge", "municipio", "macrorregiao_saude", "regiao_saude")
  )
  expect_false(anyDuplicated(muni$codigo_ibge) > 0)
  expect_false(anyNA(muni$codigo_ibge))
  expect_true(all(nzchar(muni$municipio)))

  # Internal range must be derived from the registry, never hard-coded.
  expect_equal(range(muni$codigo_ibge), c(330010L, 330620L))
})

test_that("vigiar_rj_macrorregioes and regioes_saude are sorted and unique", {
  macro <- vigiar_rj_macrorregioes()
  regioes <- vigiar_rj_regioes_saude()

  expect_type(macro, "character")
  expect_type(regioes, "character")
  expect_equal(length(macro), 9)
  expect_false(anyDuplicated(macro) > 0)
  expect_false(anyDuplicated(regioes) > 0)
  expect_equal(macro, sort(macro))
  expect_equal(regioes, sort(regioes))
})

test_that("vigiar_rj_resumo aggregates RJ municipalities", {
  dados <- data.frame(
    cod_municipio = c(330455L, 330455L, 330170L, 355030L),
    ano = c(2021L, 2022L, 2022L, 2022L),
    pm25_media = c(10, 12, 20, 99),
    stringsAsFactors = FALSE
  )

  res <- vigiar_rj_resumo(dados, agregacao = "municipio")

  expect_s3_class(res, "tbl_df")
  expect_true("n_municipios" %in% names(res))
  expect_true("pm25_media_media" %in% names(res))
  # Non-RJ municipality (355030) must be dropped.
  expect_false(any(res$n_municipios > 3))
  expect_equal(sum(res$n_municipios), 2L)
})

test_that("vigiar_rj_resumo supports macro-region aggregation", {
  dados <- data.frame(
    cod_municipio = c(330455L, 330170L),
    pm25_media = c(10, 20),
    stringsAsFactors = FALSE
  )
  res <- vigiar_rj_resumo(dados, agregacao = "macrorregiao")
  expect_true("macrorregiao_saude" %in% names(res))
})

test_that("vigiar_rj_resumo errors without a municipality column", {
  expect_error(
    vigiar_rj_resumo(data.frame(x = 1)),
    "codigo de municipio"
  )
})

test_that("vigiar_rj_resumo warns when there are no RJ rows", {
  dados <- data.frame(cod_municipio = 355030L, pm25_media = 1)
  expect_warning(res <- vigiar_rj_resumo(dados), "Nenhum municipio do RJ")
  expect_equal(nrow(res), 0)
})

test_that("vigiar_rj_series dispatches to resumo", {
  dados <- data.frame(cod_municipio = c(330455L, 330170L), pm25_media = c(1, 2))
  res <- vigiar_rj_series(dados, agregacao = "regiao_saude")
  expect_true("regiao_saude" %in% names(res))
})

test_that("vigiar_validar_rj flags codes outside RJ", {
  ok <- data.frame(cod_municipio = c(330455L, 330170L))
  expect_message(res <- vigiar_validar_rj(ok), "municipios do RJ")
  expect_true(res$valido)

  bad <- data.frame(cod_municipio = c(330455L, 355030L))
  expect_warning(res2 <- suppressMessages(vigiar_validar_rj(bad)), "NAO pertencem ao RJ")
  expect_false(res2$valido)
  expect_equal(res2$n_fora_rj, 1L)
})

test_that("vigiar_validar_rj handles empty data and missing column", {
  vazio <- data.frame(cod_municipio = integer(0))
  expect_message(res <- vigiar_validar_rj(vazio), "Nenhum municipio")
  expect_equal(res$n_total, 0L)

  expect_error(vigiar_validar_rj(data.frame(x = 1)), "codigo de municipio")
})

test_that(".vigiar_filtrar_rj filters by IBGE code and by UF", {
  por_codigo <- data.frame(muni = c(330455L, 355030L), valor = c(1, 2))
  expect_message(res <- .vigiar_filtrar_rj(por_codigo), "Filtro RJ")
  expect_equal(nrow(res), 1L)
  expect_equal(res$muni, 330455L)

  por_uf <- data.frame(UF = c("RJ", "SP"), valor = c(1, 2))
  expect_message(res2 <- .vigiar_filtrar_rj(por_uf), "Filtro RJ")
  expect_equal(nrow(res2), 1L)
})

test_that("vigiar_baixar_rj picks direct strategy for small tables", {
  .vigiar_local_reset()
  local_mocked_bindings(
    vigiar_baixar = function(tabela, ...) {
      data.frame(muni = c(330455L, 355030L), ano = 2022L)
    },
    .package = "vigiar"
  )

  res <- suppressMessages(vigiar_baixar_rj("df_muni"))
  expect_equal(nrow(res), 1L)
  expect_equal(res$muni, 330455L)
})

test_that("vigiar_baixar_rj year strategy merges ACS/DESC and deduplicates", {
  .vigiar_local_reset()
  local_mocked_bindings(
    vigiar_baixar = function(tabela, ordenar_por = NULL, direcao = "asc", ...) {
      if (identical(direcao, "asc")) {
        data.frame(muni = c(330455L, 355030L), ano = c(2000L, 2000L))
      } else {
        data.frame(muni = c(330455L), ano = c(2022L))
      }
    },
    .package = "vigiar"
  )

  res <- suppressMessages(vigiar_baixar_rj("df_anual", strategy = "year"))
  expect_true(nrow(res) >= 1L)
  expect_false(anyDuplicated(res) > 0)
})
