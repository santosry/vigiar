# vigiar: column standardisation and field validation edge cases

test_that("vigiar_padronizar_colunas uses the variable dictionary", {
  dados <- data.frame(muni = 1L, UF = "RJ", Media_pm25 = 1.5, check.names = FALSE)
  res <- vigiar_padronizar_colunas(dados, "df_anual")
  expect_true("cod_municipio" %in% names(res))
  expect_true("sigla_uf" %in% names(res))
})

test_that("vigiar_padronizar_colunas falls back to snake_case", {
  dados <- data.frame(
    `Nome do Municipio` = 1,
    `Ano Referencia` = 2,
    check.names = FALSE
  )
  res <- vigiar_padronizar_colunas(dados, "tabela_inexistente")
  expect_equal(names(res), c("nome_do_municipio", "ano_referencia"))
})

test_that("vigiar_validar_ibge validates the code range", {
  ok <- data.frame(cod_municipio = c(330455L, 355030L))
  expect_silent(vigiar_validar_ibge(ok))

  bad <- data.frame(cod_municipio = c(1L, 999999L))
  expect_warning(vigiar_validar_ibge(bad), "fora do intervalo")

  sem <- data.frame(x = 1)
  expect_identical(vigiar_validar_ibge(sem), sem)
})

test_that("vigiar_validar_datas validates ano and mes", {
  expect_silent(vigiar_validar_datas(data.frame(ano = 2022L, mes = 6L)))
  expect_warning(vigiar_validar_datas(data.frame(ano = 1999L)), "ano fora")
  expect_warning(vigiar_validar_datas(data.frame(mes = 13L)), "mes fora")
  expect_silent(vigiar_validar_datas(data.frame(x = 1)))
})

test_that("vigiar_validar_unidades validates PM2.5 range", {
  expect_silent(vigiar_validar_unidades(data.frame(pm25_media = c(0, 1000))))
  expect_warning(
    vigiar_validar_unidades(data.frame(pm25_media = c(-1, 5000))),
    "plausivel"
  )
  sem <- data.frame(x = 1)
  expect_identical(vigiar_validar_unidades(sem), sem)
})
