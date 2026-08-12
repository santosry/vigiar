# vigiar: casos de borda para cobertura de branch em diagnostic.R e compliance.R
#
# A classificacao de severidade, a deteccao de outliers e os perfis de
# compliance sao a logica de maior valor cientifico do pacote. Estes testes
# exercitam explicitamente os casos de borda em que a cobertura de branch
# importa para qualquer inferencia posterior.

library(testthat)
library(vigiar)

# ------------------------------------------------------------------------------
# diagnostic.R
# ------------------------------------------------------------------------------

test_that("serie com um unico ponto e diagnosticada como cobertura critica", {
  dados <- data.frame(
    cod_municipio = 330455L,
    sigla_uf = "RJ",
    ano = 2022L,
    pm25_media_anual = 18.3,
    stringsAsFactors = FALSE
  )

  diag <- vigiar_diagnosticar_serie(dados, escopo = "rj")

  expect_s3_class(diag, "vigiar_diagnostic")
  expect_equal(diag$metricas$n_municipios, 1)
  # 1/92 municipios => branch de cobertura pct < 5 => critico
  expect_equal(diag$severidade, "critico")
})

test_that("serie com variancia zero nao produz NaN nas metricas de PM2.5", {
  dados <- data.frame(
    cod_municipio = c(330455L, 330455L, 330455L),
    sigla_uf = "RJ",
    ano = c(2020L, 2021L, 2022L),
    pm25_media_anual = c(15, 15, 15),
    stringsAsFactors = FALSE
  )

  diag <- vigiar_diagnosticar_serie(dados, escopo = "rj")

  expect_equal(diag$metricas$pm25_dp, 0)
  expect_equal(diag$metricas$pm25_min, 15)
  expect_equal(diag$metricas$pm25_max, 15)
  # valores identicos => nenhuma quebra de serie
  expect_equal(diag$metricas$n_quebras_serie, 0)
})

test_that("municipios fora do cadastro IBGE de 92 municipios do RJ sao sinalizados", {
  dados <- data.frame(
    cod_municipio = c(355030L, 110001L),
    sigla_uf = c("SP", "RO"),
    ano = c(2022L, 2022L),
    pm25_media_anual = c(22.5, 15.7),
    stringsAsFactors = FALSE
  )

  diag <- vigiar_diagnosticar_serie(dados, escopo = "rj")

  # codigos validos no intervalo nacional, mas fora do cadastro RJ => problema
  problemas <- vapply(diag$resultados, function(x) x$severidade == "problema", logical(1))
  expect_true(any(problemas))
  # cobertura RJ: 0 dos 92 municipios => branch critico
  expect_equal(diag$metricas$rj_presentes, 0)
  expect_equal(diag$severidade, "critico")
})

test_that("datas fora do intervalo de cobertura VIGIAR sao sinalizadas", {
  dados <- data.frame(
    cod_municipio = 330455L,
    sigla_uf = "RJ",
    ano = c(1800L, 3000L),
    pm25_media_anual = c(18.3, 20.1),
    stringsAsFactors = FALSE
  )

  diag <- vigiar_diagnosticar_serie(dados, escopo = "rj")

  problemas <- vapply(diag$resultados, function(x) x$severidade == "problema", logical(1))
  expect_true(any(problemas))
})

# ------------------------------------------------------------------------------
# compliance.R
# ------------------------------------------------------------------------------

test_that("integridade detecta coluna 100% NA e coluna de valor unico", {
  dados <- data.frame(
    cod_municipio = c(330455L, 330455L),
    sigla_uf = c("RJ", "RJ"),
    ano = c(2022L, 2022L),
    vazia = c(NA_real_, NA_real_),
    constante = c(1, 1),
    stringsAsFactors = FALSE
  )

  res <- .vigiar_auditar_integridade(dados, "test", verbose = FALSE)

  expect_false(res$ok)
  expect_true(any(grepl("100% NA", res$details)))
  expect_true(any(grepl("valor unico", res$details)))
})

test_that("cobertura RJ reprova municipios fora do cadastro de 92 municipios", {
  dados <- data.frame(
    cod_municipio = c(355030L, 110001L),
    sigla_uf = c("SP", "RO"),
    stringsAsFactors = FALSE
  )

  res <- .vigiar_auditar_cobertura_rj(dados, verbose = FALSE)

  expect_false(res$ok)
  expect_equal(res$n_presentes, 0)
  expect_equal(res$n_faltantes, 92)
})

test_that("auditoria reprova datas fora do intervalo de cobertura VIGIAR", {
  dados <- data.frame(
    cod_municipio = 330455L,
    sigla_uf = "RJ",
    ano = c(1800L, 3000L),
    stringsAsFactors = FALSE
  )

  audit <- vigiar_auditar(dados, tabela = "test", verbose = FALSE)

  expect_false(audit$temporal$ok)
})
