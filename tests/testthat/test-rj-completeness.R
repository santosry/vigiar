# vigiar: offline tests for Rio de Janeiro completeness

library(testthat)
library(vigiar)

.rj_codes6 <- function(n = 92) {
  utils::head(vigiar_rj_municipios()$codigo_ibge_6, n)
}

.make_rj_data <- function(codes = .rj_codes6(), years = 2022L, months = NULL,
                          value = 12) {
  grid <- expand.grid(
    cod_municipio = as.integer(codes),
    ano = as.integer(years),
    KEEP.OUT.ATTRS = FALSE
  )
  if (!is.null(months)) {
    grid <- merge(
      grid,
      data.frame(mes = as.integer(months)),
      by = NULL
    )
  }
  grid$pm25_media_anual <- rep(value, nrow(grid))
  grid$sigla_uf <- rep("RJ", nrow(grid))
  tibble::as_tibble(grid)
}

.with_mock_vigiar_session <- function(code) {
  old_session <- .vigiar_env$sessao
  old_schema <- .vigiar_env$esquema
  on.exit({
    .vigiar_env$sessao <- old_session
    .vigiar_env$esquema <- old_schema
  }, add = TRUE)

  .vigiar_env$sessao <- list(
    model_id = 1L,
    api_url = "https://example.test",
    created_at = Sys.time()
  )
  .vigiar_env$esquema <- list(
    df_anual = list(
      muni = list(nome = "muni", tipo = "integer"),
      UF = list(nome = "UF", tipo = "character"),
      ano = list(nome = "ano", tipo = "integer"),
      Media_pm25 = list(nome = "Media_pm25", tipo = "numeric")
    ),
    tb_uf = list(
      UF = list(nome = "UF", tipo = "character"),
      ano = list(nome = "ano", tipo = "integer"),
      est = list(nome = "est", tipo = "numeric")
    )
  )
  force(code)
}

test_that("RJ registry has the expected 92 municipalities", {
  rj <- vigiar_rj_municipios()

  expect_equal(nrow(rj), 92)
  expect_equal(length(unique(rj$codigo_ibge_6)), 92)
  expect_equal(length(unique(rj$codigo_ibge_7)), 92)
  expect_equal(length(unique(rj$municipio)), 92)
  expect_false(any(is.na(rj$macrorregiao_saude)))
  expect_equal(length(vigiar_rj_macrorregioes()), 9)
  expect_true(330630L %in% rj$codigo_ibge_6)
  expect_false(330033L %in% rj$codigo_ibge_6)
})

test_that("municipality code normalization handles 6 and 7 digits safely", {
  expect_equal(.vigiar_normalizar_codigo_municipio(330455), 330455L)
  expect_equal(.vigiar_normalizar_codigo_municipio(3304557), 330455L)
  expect_equal(.vigiar_normalizar_codigo_municipio("330455"), 330455L)
  expect_equal(.vigiar_normalizar_codigo_municipio("3304557"), 330455L)
  expect_equal(.vigiar_normalizar_codigo_municipio(" 330455 "), 330455L)
  expect_equal(.vigiar_normalizar_codigo_municipio(330010), 330010L)
  expect_true(is.na(.vigiar_normalizar_codigo_municipio(3300107)))
  expect_equal(.vigiar_normalizar_codigo_municipio(330455, formato = "7"), 3304557L)
  expect_equal(.vigiar_normalizar_codigo_municipio(3304557, formato = "7"), 3304557L)
  expect_true(is.na(.vigiar_normalizar_codigo_municipio(NA)))
  expect_true(is.na(.vigiar_normalizar_codigo_municipio("abc")))
  expect_true(is.na(.vigiar_normalizar_codigo_municipio(999999)))
  expect_equal(.vigiar_normalizar_codigo_municipio(3550308), 355030L)
  expect_true(is.na(.vigiar_normalizar_codigo_municipio(9999999)))
  expect_true(is.na(.vigiar_normalizar_codigo_municipio("330455X")))
})

test_that("RJ coverage detects full and partial municipality sets", {
  full <- .make_rj_data()
  cov_full <- vigiar_rj_cobertura(full)

  expect_true(all(c("por", "codigos_ausentes") %in% names(cov_full)))
  expect_equal(cov_full$por, "geral")
  expect_equal(cov_full$n_municipios_presentes, 92)
  expect_equal(cov_full$n_ausentes, 0)
  expect_true(cov_full$completo)

  partial <- .make_rj_data(.rj_codes6(10))
  cov_partial <- vigiar_rj_cobertura(partial)

  expect_equal(cov_partial$n_municipios_presentes, 10)
  expect_equal(cov_partial$n_ausentes, 82)
  expect_false(cov_partial$completo)
  expect_length(cov_partial$municipios_ausentes[[1]], 82)
})

test_that("RJ coverage handles empty data and missing municipality columns", {
  empty <- .make_rj_data(integer(0))
  cov_empty <- vigiar_rj_cobertura(empty)
  expect_equal(cov_empty$n_municipios_presentes, 0)
  expect_equal(cov_empty$n_ausentes, 92)

  no_code <- data.frame(ano = 2022L, pm25 = 11)
  expect_error(vigiar_rj_cobertura(no_code), "Municipality code column")
  expect_warning(
    cov_no_code <- vigiar_rj_cobertura(no_code, exigir_coluna_municipio = FALSE),
    "Municipality code column"
  )
  expect_equal(cov_no_code$n_municipios_presentes, 0)
})

test_that("RJ coverage ignores outside-RJ codes and duplicates", {
  dados <- .make_rj_data(.rj_codes6(3))
  dados <- rbind(dados, dados[1, ], data.frame(
    cod_municipio = 3550308L,
    ano = 2022L,
    pm25_media_anual = 18,
    sigla_uf = "SP"
  ))

  cov <- vigiar_rj_cobertura(dados)
  expect_equal(cov$n_municipios_presentes, 3)
  expect_equal(cov$n_ausentes, 89)
})

test_that("RJ coverage works by year, month, and year-month", {
  one_year <- .make_rj_data(.rj_codes6(), years = 2020L)
  other_year <- .make_rj_data(.rj_codes6(10), years = 2021L)
  dados <- rbind(one_year, other_year)

  cov_year <- vigiar_rj_cobertura(dados, por = "ano")
  expect_equal(nrow(cov_year), 2)
  expect_true(cov_year$completo[cov_year$ano == 2020L])
  expect_false(cov_year$completo[cov_year$ano == 2021L])

  monthly <- .make_rj_data(.rj_codes6(5), years = 2022L, months = 1:2)
  cov_month <- vigiar_rj_cobertura(monthly, por = "mes")
  cov_year_month <- vigiar_rj_cobertura(monthly, por = "ano_mes")
  expect_equal(nrow(cov_month), 2)
  expect_equal(nrow(cov_year_month), 2)
  expect_equal(cov_year_month$n_municipios_presentes, c(5L, 5L))
})

test_that("RJ coverage works by macro-region and health region", {
  rj <- vigiar_rj_municipios()
  macro <- rj$macrorregiao_saude[[1]]
  macro_codes <- rj$codigo_ibge_6[rj$macrorregiao_saude == macro]
  dados <- .make_rj_data(utils::head(macro_codes, 1))

  cov_macro <- vigiar_rj_cobertura(dados, por = "macrorregiao")
  row_macro <- cov_macro[cov_macro$macrorregiao_saude == macro, ]
  expect_equal(row_macro$n_municipios_esperados, length(macro_codes))
  expect_equal(row_macro$n_municipios_presentes, 1L)
  expect_false(row_macro$completo)

  cov_region <- vigiar_rj_cobertura(dados, por = "regiao_saude")
  expect_s3_class(cov_region, "tbl_df")
  expect_true("regiao_saude" %in% names(cov_region))
})

test_that("absent municipality table is long and grouped", {
  dados <- .make_rj_data(.rj_codes6(10), years = c(2020L, 2021L))
  ausentes <- vigiar_rj_municipios_ausentes(dados, por = "ano")

  expect_s3_class(ausentes, "tbl_df")
  expect_true(all(c("nivel", "ano", "codigo_ibge_6", "municipio") %in% names(ausentes)))
  expect_equal(nrow(ausentes), 82 * 2)
})

test_that("RJ download mock filters RJ and attaches coverage attributes", {
  mock_data <- tibble::tibble(
    muni = c(3304557L, 3300100L, 3550308L),
    UF = c("RJ", "RJ", "SP"),
    ano = 2022L,
    Media_pm25 = c(12, 13, 20)
  )

  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) mock_data,
      .package = "vigiar"
    )

    expect_warning(
      out <- vigiar_baixar_rj("df_anual", validar_cobertura = TRUE),
      "partial RJ coverage"
    )

    expect_equal(nrow(out), 2)
    expect_true(all(out$codigo_ibge_6 %in% vigiar_rj_municipios()$codigo_ibge_6))
    expect_equal(attr(out, "vigiar_uf"), "RJ")
    expect_equal(attr(out, "vigiar_rj_n_municipios"), 2)
    expect_equal(attr(out, "vigiar_rj_n_esperado"), 92)
    expect_equal(attr(out, "vigiar_n_municipios_presentes"), 2)
    expect_equal(attr(out, "vigiar_n_municipios_esperados"), 92)
  })
})

test_that("RJ download handles non-municipal tables and possible truncation", {
  no_muni <- tibble::tibble(UF = "RJ", ano = 2022L, est = 1)

  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) no_muni,
      .package = "vigiar"
    )
    expect_error(
      vigiar_baixar_rj("tb_uf"),
      "no municipality code column"
    )
  })

  truncated <- .make_rj_data(.rj_codes6())
  attr(truncated, "vigiar_possivel_truncamento") <- TRUE
  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) truncated,
      .package = "vigiar"
    )
    expect_warning(
      out <- vigiar_baixar_rj("df_anual", validar_cobertura = FALSE),
      "Possible truncation"
    )
    expect_true(isTRUE(attr(out, "vigiar_possivel_truncamento")))
  })
})

test_that("RJ download completeness requirement fails and passes correctly", {
  partial <- .make_rj_data(.rj_codes6(10))
  complete <- .make_rj_data(.rj_codes6())

  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) partial,
      .package = "vigiar"
    )
    expect_error(
      suppressWarnings(vigiar_baixar_rj("df_anual", exigir_completo = TRUE)),
      "incomplete"
    )
  })

  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) complete,
      .package = "vigiar"
    )
    expect_no_error(vigiar_baixar_rj("df_anual", exigir_completo = TRUE))
  })
})

test_that("RJ snapshot preserves completeness metadata", {
  partial <- .make_rj_data(.rj_codes6(3))

  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) partial,
      .package = "vigiar"
    )
    out <- suppressWarnings(vigiar_baixar_rj("df_anual", snapshot = TRUE))
    snap <- attr(out, "vigiar_snapshot")

    expect_s3_class(snap, "vigiar_snapshot")
    expect_equal(attr(snap$dados, "vigiar_uf"), "RJ")

    tmp <- tempfile(fileext = ".rds")
    saveRDS(out, tmp)
    loaded <- readRDS(tmp)
    expect_equal(attr(loaded, "vigiar_uf"), "RJ")
    expect_equal(attr(loaded, "vigiar_n_municipios_presentes"), 3)
  })
})

test_that("RJ diagnostic recognizes complete, partial, absent, and truncated data", {
  complete <- .make_rj_data(.rj_codes6())
  diag_complete <- vigiar_diagnosticar_serie(complete, escopo = "rj")
  expect_equal(diag_complete$metricas$rj_presentes, 92)
  expect_equal(diag_complete$severidade, "ok")

  partial <- .make_rj_data(.rj_codes6(10))
  diag_partial <- vigiar_diagnosticar_serie(partial, escopo = "rj")
  expect_equal(diag_partial$metricas$rj_presentes, 10)
  expect_true(diag_partial$severidade %in% c("problema", "critico"))

  absent <- data.frame(
    cod_municipio = 3550308L,
    ano = 2022L,
    pm25_media_anual = 20
  )
  diag_absent <- vigiar_diagnosticar_serie(absent, escopo = "rj")
  expect_equal(diag_absent$metricas$rj_presentes, 0)
  expect_equal(diag_absent$severidade, "critico")

  truncated <- complete
  attr(truncated, "vigiar_possivel_truncamento") <- TRUE
  diag_trunc <- vigiar_diagnosticar_serie(truncated, escopo = "rj")
  msgs <- vapply(diag_trunc$resultados, `[[`, "", "mensagem")
  expect_true(any(grepl("Possible truncation", msgs)))
})

test_that("RJ PM2.5 plot returns a ggplot object when ggplot2 is installed", {
  skip_if_not_installed("ggplot2")

  dados <- .make_rj_data(.rj_codes6(5), years = 2020:2022)
  p <- vigiar_plot_pm25_rj(dados, por = "ano", valor = "pm25_media_anual")
  expect_s3_class(p, "ggplot")
})
