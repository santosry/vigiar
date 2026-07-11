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
    df_mensal = list(
      muni = list(nome = "muni", tipo = "integer"),
      UF = list(nome = "UF", tipo = "character"),
      ano = list(nome = "ano", tipo = "integer"),
      mes = list(nome = "mes", tipo = "integer"),
      pm25 = list(nome = "pm25", tipo = "numeric"),
      LAT = list(nome = "LAT", tipo = "numeric"),
      LON = list(nome = "LON", tipo = "numeric")
    ),
    df_dias = list(
      ID_MUNI = list(nome = "ID_MUNI", tipo = "integer"),
      ano = list(nome = "ano", tipo = "integer"),
      mes = list(nome = "mes", tipo = "integer"),
      n_dias = list(nome = "n_dias", tipo = "integer")
    ),
    df_dias_conama = list(
      ID_MUNI = list(nome = "ID_MUNI", tipo = "integer"),
      ano = list(nome = "ano", tipo = "integer"),
      mes = list(nome = "mes", tipo = "integer"),
      n_dias_conama = list(nome = "n_dias_conama", tipo = "integer")
    ),
    pop = list(
      muni = list(nome = "muni", tipo = "integer"),
      ano = list(nome = "ano", tipo = "integer"),
      pop = list(nome = "pop", tipo = "numeric"),
      UF = list(nome = "UF", tipo = "character")
    ),
    tb_uf = list(
      UF = list(nome = "UF", tipo = "character"),
      ano = list(nome = "ano", tipo = "integer"),
      est = list(nome = "est", tipo = "numeric")
    )
  )
  force(code)
}

test_that("Power BI server-side filters are encoded in SemanticQuery", {
  .with_mock_vigiar_session({
    query <- .vigiar_construir_query(
      "df_anual",
      colunas = c("muni", "UF", "ano"),
      modelo_id = .vigiar_env$sessao$model_id,
      filtros = list(UF = "RJ", muni = c(330100L, 330455L))
    )
    where <- query$queries[[1]]$Query$Commands[[1]]$
      SemanticQueryDataShapeCommand$Query$Where

    expect_length(where, 2)
    expect_equal(where[[1]]$Condition$In$Expressions[[1]]$Column$Property, "UF")
    expect_equal(where[[1]]$Condition$In$Values[[1]][[1]]$Literal$Value, "'RJ'")
    expect_equal(where[[2]]$Condition$In$Expressions[[1]]$Column$Property, "muni")
    expect_equal(
      vapply(where[[2]]$Condition$In$Values, function(x) x[[1]]$Literal$Value, character(1)),
      c("330100L", "330455L")
    )
  })
})

test_that("RJ downloads request server-side RJ filters", {
  mock_data <- tibble::tibble(
    muni = c(330455L, 330010L),
    UF = "RJ",
    ano = 2022L,
    Media_pm25 = c(12, 14)
  )
  captured <- new.env(parent = emptyenv())

  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) {
        captured$args <- list(...)
        mock_data
      },
      .package = "vigiar"
    )

    out <- suppressWarnings(vigiar_baixar_rj("df_anual", validar_cobertura = FALSE))

    expect_equal(captured$args$filtros$UF, "RJ")
    expect_equal(unique(out$UF), "RJ")
  })

  captured <- new.env(parent = emptyenv())
  dias_data <- tibble::tibble(
    ID_MUNI = c(330100L, 330475L),
    ano = 2022L,
    mes = 1L,
    n_dias = c(2L, 3L)
  )

  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) {
        captured$args <- list(...)
        dias_data
      },
      .package = "vigiar"
    )

    out <- suppressWarnings(vigiar_baixar_rj("df_dias", validar_cobertura = FALSE))

    expect_named(captured$args$filtros, "ID_MUNI")
    expect_length(captured$args$filtros$ID_MUNI, 92)
    expect_true(330100L %in% captured$args$filtros$ID_MUNI)
    expect_equal(nrow(out), 2)
  })
})

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

test_that("RJ registry matches the official IBGE municipality code reference", {
  ref_path <- system.file("extdata", "municipios_ibge_reference.csv", package = "vigiar")
  metadata_path <- system.file(
    "extdata", "municipios_ibge_reference_metadata.json", package = "vigiar"
  )
  expect_true(nzchar(ref_path))
  expect_true(nzchar(metadata_path))
  all_ibge <- utils::read.csv(ref_path, stringsAsFactors = FALSE)
  ibge <- all_ibge[all_ibge$sigla_uf == "RJ", ]
  rj <- vigiar_rj_municipios()

  expect_equal(nrow(all_ibge), 5571)
  expect_equal(nrow(ibge), 92)
  expect_setequal(rj$codigo_ibge_6, ibge$codigo_ibge_6)
  expect_setequal(rj$codigo_ibge_7, ibge$codigo_ibge_7)
  expect_setequal(rj$municipio, iconv(ibge$municipio, to = "ASCII//TRANSLIT"))
  expect_false(330033L %in% all_ibge$codigo_ibge_6)

  merged <- merge(rj, ibge, by = c("codigo_ibge_6", "codigo_ibge_7"))
  expect_equal(nrow(merged), 92)
  expect_equal(rj$municipio[rj$codigo_ibge_6 == 330100], "Campos dos Goytacazes")
  expect_equal(merged$codigo_ibge_7[merged$codigo_ibge_6 == 330100], 3301009)
  expect_equal(merged$codigo_ibge_7[merged$codigo_ibge_6 == 330475], 3304755)
  expect_equal(merged$codigo_ibge_7[merged$codigo_ibge_6 == 330500], 3305000)
  expect_equal(length(unique(rj$macrorregiao_saude)), 9)

  metadata <- jsonlite::fromJSON(metadata_path)
  checksum_connection <- file(ref_path, open = "rb")
  checksum <- paste(format(openssl::sha256(checksum_connection)), collapse = "")
  close(checksum_connection)
  expect_equal(metadata$n_municipalities, nrow(all_ibge))
  expect_equal(metadata$n_rj_municipalities, 92)
  expect_identical(metadata$sha256, checksum)
})

test_that("RJ registry matches the official SES-RJ health-region reference", {
  ref_path <- system.file("extdata", "rj_regioes_saude_sesrj_reference.csv", package = "vigiar")
  source_path <- system.file("extdata", "rj_official_sources.csv", package = "vigiar")
  expect_true(nzchar(ref_path))
  expect_true(nzchar(source_path))

  ses <- utils::read.csv(ref_path, stringsAsFactors = FALSE)
  sources <- utils::read.csv(source_path, stringsAsFactors = FALSE)
  rj <- vigiar_rj_municipios()
  region_counts <- table(rj$regiao_saude)

  expect_equal(nrow(ses), 9)
  expect_setequal(vigiar_rj_macrorregioes(), ses$regiao_saude_package)
  expect_setequal(vigiar_rj_regioes_saude(), ses$regiao_saude_package)
  expect_identical(rj$macrorregiao_saude, rj$regiao_saude)
  expect_equal(sum(ses$municipios_esperados), 92)
  expect_equal(
    as.integer(region_counts[ses$regiao_saude_package]),
    ses$municipios_esperados
  )
  expect_true(all(nzchar(ses$fonte_url)))
  expect_true(all(c("ibge_localidades", "sesrj_regionalizacao") %in% sources$source_id))
  expect_true(nzchar(attr(rj, "vigiar_rj_sources")))
})

test_that("Campos dos Goytacazes is a sentinel RJ municipality", {
  rj <- vigiar_rj_municipios()
  campos <- rj[rj$codigo_ibge_6 == 330100, ]

  expect_equal(nrow(campos), 1)
  expect_equal(campos$codigo_ibge_7, 3301009)
  expect_equal(campos$municipio, "Campos dos Goytacazes")
  expect_equal(campos$macrorregiao_saude, "Norte")
  expect_false(any(
    rj$municipio[rj$codigo_ibge_6 == 330100] %in%
      c("Carapebus", "Cambuci", "Cardoso Moreira")
  ))
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

test_that("processing and RJ joins use normalized municipality codes, not names", {
  raw <- data.frame(
    muni = 3301009L,
    UF = "RJ",
    ano = 2022L,
    Media_pm25 = 12,
    Municipio = "Carapebus",
    stringsAsFactors = FALSE
  )

  processed <- process_pm25(raw, tipo = "anual")
  expect_equal(processed$cod_municipio, 330100L)

  resumo <- vigiar_rj_resumo(processed, agregacao = "municipio")
  expect_equal(nrow(resumo), 1)
  expect_equal(resumo$municipio, "Campos dos Goytacazes")
})

test_that("RJ table completeness uses expected table grains", {
  annual <- rbind(
    .make_rj_data(.rj_codes6(), years = 2020L),
    .make_rj_data(.rj_codes6(10), years = 2021L)
  )
  cov_annual <- vigiar_rj_completude_tabela(annual, tabela = "df_anual")
  expect_equal(unique(cov_annual$grade), "municipio x ano")
  expect_equal(nrow(cov_annual), 2)
  expect_true(cov_annual$completo[cov_annual$ano == 2020L])
  expect_false(cov_annual$completo[cov_annual$ano == 2021L])

  monthly <- .make_rj_data(.rj_codes6(), years = 2022L, months = 1:2)
  monthly <- monthly[!(monthly$cod_municipio == .rj_codes6(1) & monthly$mes == 2L), ]
  cov_monthly <- vigiar_rj_completude_tabela(monthly, tabela = "df_mensal")
  expect_equal(unique(cov_monthly$grade), "municipio x ano x mes")
  expect_equal(nrow(cov_monthly), 2)
  expect_true(cov_monthly$completo[cov_monthly$mes == 1L])
  expect_false(cov_monthly$completo[cov_monthly$mes == 2L])
  expect_error(
    vigiar_rj_completude_tabela(monthly, tabela = "df_mensal", require_complete = TRUE),
    "incomplete"
  )

  daily <- .make_rj_data(.rj_codes6(), years = 2022L, months = 1:2)
  daily <- daily[!(daily$cod_municipio == .rj_codes6(1) & daily$mes == 2L), ]
  daily$pm25_media_periodo <- daily$pm25_media_anual
  daily$n_dias_criticos <- 1L
  cov_daily <- vigiar_rj_completude_tabela(daily, tabela = "df_dias")
  expect_equal(unique(cov_daily$grade), "municipio x ano x mes")
  expect_equal(nrow(cov_daily), 2)
  expect_true(cov_daily$completo[cov_daily$mes == 1L])
  expect_false(cov_daily$completo[cov_daily$mes == 2L])
  expect_error(
    vigiar_rj_completude_tabela(daily, tabela = "df_dias", require_complete = TRUE),
    "incomplete"
  )

  pop <- rbind(
    .make_rj_data(.rj_codes6(), years = 2020L),
    .make_rj_data(.rj_codes6(20), years = 2021L)
  )
  cov_pop <- vigiar_rj_completude_tabela(pop, tabela = "pop")
  expect_equal(unique(cov_pop$grade), "municipio x ano")
  expect_true(cov_pop$completo[cov_pop$ano == 2020L])
  expect_false(cov_pop$completo[cov_pop$ano == 2021L])
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
      "Truncation status"
    )
    expect_true(isTRUE(attr(out, "vigiar_possivel_truncamento")))
    expect_error(
      suppressWarnings(vigiar_baixar_rj("df_anual", require_complete = TRUE)),
      "truncation"
    )
  })
})

test_that("municipality download filters only by IBGE code and keeps metadata", {
  mixed <- tibble::tibble(
    muni = c(3301009L, 3300936L, 3300902L, 3301157L, 3550308L),
    UF = c("RJ", "RJ", "RJ", "RJ", "SP"),
    ano = 2022L,
    Media_pm25 = c(12, 13, 14, 15, 99)
  )
  captured <- new.env(parent = emptyenv())

  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) {
        captured$args <- list(...)
        mixed
      },
      .package = "vigiar"
    )
    out <- suppressWarnings(vigiar_baixar_municipio("df_anual", codigo_ibge = 330100))

    expect_equal(nrow(out), 1)
    expect_equal(unique(out$codigo_ibge_6), 330100L)
    expect_equal(attr(out, "vigiar_codigo_ibge_6"), 330100L)
    expect_equal(attr(out, "vigiar_codigo_ibge_7"), 3301009L)
    expect_equal(attr(out, "vigiar_municipio"), "Campos dos Goytacazes")
    expect_equal(attr(out, "vigiar_macrorregiao_saude"), "Norte")
    expect_identical(captured$args$filtros$muni, 330100L)
    expect_identical(
      attr(out, "vigiar_query_strategy"),
      "server_side_municipality_filter_with_local_verification"
    )
    expect_false(any(out$codigo_ibge_6 %in% c(330093L, 330090L, 330115L)))
  })

  mixed_truncated <- mixed
  attr(mixed_truncated, "vigiar_possivel_truncamento") <- TRUE
  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) mixed_truncated,
      .package = "vigiar"
    )
    expect_error(
      suppressWarnings(vigiar_baixar_municipio(
        "df_anual",
        codigo_ibge = 330100,
        require_complete = TRUE
      )),
      "truncation"
    )
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

test_that("RJ online audit saves complete artifacts with preserved metadata", {
  complete <- .make_rj_data(.rj_codes6(), years = c(2021L, 2022L))
  out_root <- tempfile("vigiar-rj-audit-")

  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) complete,
      .package = "vigiar"
    )

    audit <- vigiar_auditar_rj_online(
      "df_anual",
      salvar = TRUE,
      dir = out_root,
      require_complete = TRUE,
      dominios_esperados = list(
        df_anual = list(anos_esperados = 2021:2022)
      ),
      timestamp = as.POSIXct("2026-01-02 03:04:05", tz = "UTC")
    )

    report_dir <- attr(audit, "vigiar_audit_dir")
    expect_equal(audit$conclusion, "complete")
    expect_true(audit$campos_dos_goytacazes_presente)
    expect_true(file.exists(file.path(report_dir, "manifest.csv")))
    expect_true(file.exists(file.path(report_dir, "manifest.json")))
    expect_true(file.exists(file.path(report_dir, "audit.rds")))
    expect_true(file.exists(file.path(report_dir, "df_anual-coverage-table-grain.csv")))

    loaded <- readRDS(file.path(report_dir, "audit.rds"))
    expect_equal(attr(loaded, "vigiar_audit_dir"), report_dir)
    expect_equal(loaded$conclusion, "complete")

    manifest <- jsonlite::read_json(file.path(report_dir, "manifest.json"))
    expect_equal(manifest$tables[[1]]$summary$conclusion, "complete")
    expect_equal(manifest$tables[[1]]$summary$tabela, "df_anual")
  })
})

test_that("RJ online audit detects partial coverage and absent Campos", {
  partial <- .make_rj_data(.rj_codes6(10), years = 2022L)
  no_campos <- .make_rj_data(setdiff(.rj_codes6(), 330100L), years = 2022L)

  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) partial,
      .package = "vigiar"
    )

    audit <- suppressWarnings(vigiar_auditar_rj_online(
      "df_anual",
      salvar = FALSE
    ))
    expect_equal(audit$conclusion, "partial")
    expect_equal(audit$n_municipios_presentes, 10)
    expect_error(
      suppressWarnings(vigiar_auditar_rj_online(
        "df_anual",
        salvar = FALSE,
        require_complete = TRUE
      )),
      "not complete"
    )
  })

  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) no_campos,
      .package = "vigiar"
    )

    audit <- suppressWarnings(vigiar_auditar_rj_online(
      "df_anual",
      salvar = FALSE
    ))
    expect_equal(audit$conclusion, "partial")
    expect_false(audit$campos_dos_goytacazes_presente)
    expect_true(
      "Campos dos Goytacazes" %in%
        audit$municipios_ausentes[[1]]$municipio
    )
  })
})

test_that("RJ online audit detects missing municipal columns and truncation", {
  no_muni <- tibble::tibble(UF = "RJ", ano = 2022L, est = 1)
  truncated <- .make_rj_data(.rj_codes6(), years = 2022L)
  attr(truncated, "vigiar_possivel_truncamento") <- TRUE

  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) no_muni,
      .package = "vigiar"
    )

    audit <- suppressWarnings(vigiar_auditar_rj_online(
      "tb_uf",
      salvar = FALSE
    ))
    expect_equal(audit$conclusion, "failed")
    expect_match(audit$erro, "Municipality code column")
    expect_error(
      suppressWarnings(vigiar_auditar_rj_online(
        "tb_uf",
        salvar = FALSE,
        require_complete = TRUE
      )),
      "Municipality code"
    )
  })

  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) truncated,
      .package = "vigiar"
    )

    expect_warning(
      audit <- vigiar_auditar_rj_online("df_anual", salvar = FALSE),
      "Truncation status"
    )
    expect_equal(audit$conclusion, "truncated")
    expect_true(audit$possivel_truncamento)
    expect_error(
      suppressWarnings(vigiar_auditar_rj_online(
        "df_anual",
        salvar = FALSE,
        require_complete = TRUE
      )),
      "truncated"
    )
  })
})

test_that("RJ online audit detects schema hash absence", {
  complete <- .make_rj_data(.rj_codes6(), years = 2022L)

  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) complete,
      .vigiar_schema_hash = function(tabela) NA_character_,
      .package = "vigiar"
    )

    audit <- vigiar_auditar_rj_online("df_anual", salvar = FALSE)
    expect_equal(audit$conclusion, "schema_unverified")
    expect_true(is.na(audit$schema_hash))
  })
})

test_that("RJ online audit detects monthly gaps", {
  monthly <- .make_rj_data(.rj_codes6(), years = 2022L, months = 1:2)
  monthly <- monthly[!(monthly$cod_municipio == 330100L & monthly$mes == 2L), ]

  .with_mock_vigiar_session({
    testthat::local_mocked_bindings(
      vigiar_baixar = function(...) monthly,
      .package = "vigiar"
    )

    audit <- suppressWarnings(vigiar_auditar_rj_online(
      "df_mensal",
      salvar = FALSE
    ))
    expect_equal(audit$conclusion, "partial")
    expect_equal(audit$completeness_grade, "municipio x ano x mes")
    expect_equal(audit$n_incomplete_groups, 1L)

    month_gap <- audit$completude_tabela[[1]]
    month_gap <- month_gap[month_gap$ano == 2022L & month_gap$mes == 2L, ]
    expect_equal(nrow(month_gap), 1)
    expect_false(month_gap$completo)
    expect_true(330100L %in% month_gap$codigos_ausentes[[1]])
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
