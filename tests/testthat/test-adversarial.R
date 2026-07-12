# Adversarial regression tests for conservative scientific validation

.adversarial_rj_panel <- function(years = 2022L, months = NULL) {
  rj <- vigiar_rj_municipios()
  periods <- if (is.null(months)) {
    data.frame(ano = as.integer(years))
  } else {
    expand.grid(ano = as.integer(years), mes = as.integer(months))
  }
  merge(
    data.frame(cod_municipio = rj$codigo_ibge_6),
    periods,
    by = NULL
  )
}

test_that("RJ compliance rejects external, invalid, absent, and unknown municipalities", {
  rj <- vigiar_rj_municipios()
  complete <- data.frame(cod_municipio = rj$codigo_ibge_6)

  exact <- suppressMessages(vigiar_compliance_check(
    complete,
    profiles = "rj",
    verbose = FALSE
  ))
  expect_true(exact$rj$ok)
  expect_identical(exact$rj$status, "pass")

  external <- rbind(complete, data.frame(cod_municipio = 355030L))
  mixed <- suppressWarnings(suppressMessages(vigiar_compliance_check(
    external,
    profiles = "rj",
    verbose = FALSE
  )))
  expect_false(mixed$rj$ok)
  expect_identical(mixed$rj$status, "fail")
  expect_equal(mixed$rj$rj_municipios$n_fora_rj, 1L)

  partial <- suppressMessages(vigiar_compliance_check(
    complete[-1, , drop = FALSE],
    profiles = "rj",
    verbose = FALSE
  ))
  expect_false(partial$rj$ok)

  none <- suppressWarnings(suppressMessages(vigiar_compliance_check(
    data.frame(cod_municipio = 355030L),
    profiles = "rj",
    verbose = FALSE
  )))
  expect_false(none$rj$ok)

  invalid <- suppressWarnings(suppressMessages(vigiar_compliance_check(
    data.frame(cod_municipio = c(rj$codigo_ibge_6, "3301008", "abc")),
    profiles = "rj",
    verbose = FALSE
  )))
  expect_false(invalid$rj$ok)
  expect_equal(invalid$rj$rj_municipios$n_invalidos, 2L)

  mixed_widths <- data.frame(
    cod_municipio = ifelse(
      seq_len(nrow(rj)) %% 2L == 0L,
      rj$codigo_ibge_6,
      rj$codigo_ibge_7
    )
  )
  mixed_ok <- suppressMessages(vigiar_compliance_check(
    mixed_widths,
    profiles = "rj",
    verbose = FALSE
  ))
  expect_true(mixed_ok$rj$ok)

  missing_column <- suppressWarnings(suppressMessages(vigiar_compliance_check(
    data.frame(ano = 2022L),
    profiles = "rj",
    verbose = FALSE
  )))
  expect_false(missing_column$rj$ok)
  expect_identical(missing_column$rj$status, "fail")
})

test_that("RJ validation exposes a consistent check contract", {
  rj <- vigiar_rj_municipios()
  result <- suppressMessages(vigiar_validar_rj(
    data.frame(cod_municipio = rj$codigo_ibge_6)
  ))

  expect_true(all(c("ok", "status", "severity", "details") %in% names(result)))
  expect_true(result$ok)
  expect_true(result$valido)
  expect_identical(result$status, "pass")
})

test_that("RJ compliance rejects possible truncation", {
  dados <- .adversarial_rj_panel()
  attr(dados, "vigiar_possivel_truncamento") <- TRUE

  result <- suppressWarnings(suppressMessages(vigiar_compliance_check(
    dados,
    profiles = "rj",
    verbose = FALSE
  )))

  expect_false(result$rj$ok)
  expect_identical(result$rj$truncation$status, "fail")
})

test_that("table completeness detects entirely missing internal periods", {
  monthly <- .adversarial_rj_panel(months = c(1L, 2L, 4L))
  monthly_result <- vigiar_rj_completude_tabela(
    monthly,
    tabela = "df_mensal"
  )

  expect_setequal(monthly_result$mes, 1:4)
  march <- monthly_result[monthly_result$ano == 2022L & monthly_result$mes == 3L, ]
  expect_equal(nrow(march), 1L)
  expect_true(march$periodo_ausente)
  expect_equal(march$n_municipios_presentes, 0L)
  expect_false(march$completo)
  expect_identical(unique(monthly_result$dominio_temporal), "inferred")

  annual <- .adversarial_rj_panel(years = c(2020L, 2022L))
  annual_result <- vigiar_rj_completude_tabela(
    annual,
    tabela = "df_anual"
  )
  expect_setequal(annual_result$ano, 2020:2022)
  expect_true(annual_result$periodo_ausente[annual_result$ano == 2021L])
})

test_that("user-specified temporal domain detects missing boundary periods", {
  monthly <- .adversarial_rj_panel(months = c(2L, 3L))
  result <- vigiar_rj_completude_tabela(
    monthly,
    tabela = "df_mensal",
    periodo_inicio = as.Date("2022-01-01"),
    periodo_fim = as.Date("2022-04-01")
  )

  expect_setequal(result$mes, 1:4)
  expect_true(result$periodo_ausente[result$mes == 1L])
  expect_true(result$periodo_ausente[result$mes == 4L])
  expect_identical(unique(result$dominio_temporal), "user_specified")
})

test_that("panel completeness distinguishes partial rows from absent periods", {
  monthly <- .adversarial_rj_panel(months = c(1L, 2L))
  campos <- 330100L
  monthly <- monthly[!(monthly$mes == 2L & monthly$cod_municipio == campos), ]

  result <- vigiar_rj_completude_tabela(
    monthly,
    tabela = "df_mensal",
    anos_esperados = 2022L,
    meses_esperados = 1:3
  )

  feb <- result[result$mes == 2L, ]
  mar <- result[result$mes == 3L, ]
  expect_false(feb$periodo_ausente)
  expect_equal(feb$n_municipios_presentes, 91L)
  expect_true(campos %in% feb$codigos_ausentes[[1]])
  expect_true(mar$periodo_ausente)
  expect_equal(mar$n_municipios_presentes, 0L)
  expect_false(all(result$completo))
})

test_that("invalid temporal values are explicit and cannot pass strict mode", {
  rj <- vigiar_rj_municipios()
  invalid <- data.frame(
    cod_municipio = rj$codigo_ibge_6[1:4],
    ano = c(2022L, NA_integer_, 1999L, 2022L),
    mes = c(1L, 2L, 3L, 13L)
  )

  result <- suppressWarnings(vigiar_rj_completude_tabela(
    invalid,
    tabela = "df_mensal"
  ))
  temporal <- attr(result, "vigiar_temporal_validation")
  expect_false(temporal$ok)
  expect_equal(temporal$n_ano_ausente, 1L)
  expect_equal(temporal$n_ano_invalido, 1L)
  expect_equal(temporal$n_mes_invalido, 1L)

  expect_error(
    suppressWarnings(vigiar_rj_completude_tabela(
      invalid,
      tabela = "df_mensal",
      require_complete = TRUE
    )),
    "Invalid or missing temporal values"
  )
})

test_that("IBGE code validation separates format, existence, and RJ membership", {
  report <- vigiar_validar_codigo_municipio(c(
    330100,
    3301009,
    3301008,
    330033,
    " 3550308 ",
    "330100.0",
    "abc",
    NA,
    0,
    -330100
  ), uf = "RJ")

  expect_equal(report$codigo_ibge_6[1:2], c(330100L, 330100L))
  expect_true(all(report$formato_valido[1:2]))
  expect_true(all(report$existe[1:2]))
  expect_true(all(report$pertence_uf[1:2]))

  expect_true(report$formato_valido[3])
  expect_false(report$digito_valido[3])
  expect_false(report$existe[3])

  expect_true(report$formato_valido[4])
  expect_false(report$existe[4])
  expect_false(report$pertence_uf[4])

  expect_equal(report$codigo_ibge_6[5], 355030L)
  expect_true(report$existe[5])
  expect_false(report$pertence_uf[5])
  expect_equal(report$codigo_ibge_6[6], 330100L)
  expect_identical(report$status[8], "unknown")
  expect_true(all(report$status[c(3, 4, 5, 7, 9, 10)] == "fail"))
})

test_that("data-frame IBGE validation stores a consumable report", {
  dados <- data.frame(cod_municipio = c(330100L, 330033L, NA_integer_))
  expect_warning(
    out <- vigiar_validar_ibge(dados, uf = "RJ"),
    "IBGE municipality code validation failed"
  )
  report <- attr(out, "vigiar_ibge_validation")
  expect_s3_class(report, "tbl_df")
  expect_equal(report$status, c("pass", "fail", "unknown"))
  expect_error(vigiar_validar_ibge(dados, uf = "RJ", error = TRUE),
               "IBGE municipality code validation failed")
})

test_that("generic downloads no longer default silently to RJ", {
  expect_null(formals(vigiar_baixar)$uf)
})

test_that("raw response truncation evidence survives a local UF filter", {
  old_session <- .vigiar_env$sessao
  old_schema <- .vigiar_env$esquema
  on.exit({
    .vigiar_env$sessao <- old_session
    .vigiar_env$esquema <- old_schema
  }, add = TRUE)
  .vigiar_env$sessao <- list(
    model_id = 1L,
    api_url = "https://example.org/",
    created_at = Sys.time()
  )
  .vigiar_env$esquema <- list(test = list(
    UF = list(tipo = "character"),
    value = list(tipo = "integer")
  ))
  raw <- data.frame(
    UF = c("RJ", rep("SP", 28499L)),
    value = seq_len(28500L)
  )
  testthat::local_mocked_bindings(
    .vigiar_executar_query = function(...) list(),
    .vigiar_parse_dados = function(...) raw,
    .package = "vigiar"
  )

  expect_warning(
    filtered <- vigiar_baixar("test", uf = "RJ"),
    "conservative Power BI response threshold"
  )
  expect_equal(nrow(filtered), 1L)
  expect_equal(attr(filtered, "vigiar_response_rows"), 28500L)
  expect_true(isTRUE(attr(filtered, "vigiar_possivel_truncamento")))
  expect_identical(attr(filtered, "vigiar_truncation_status"), "possible")
})

test_that("UF filtering handles labels and numeric codes", {
  labelled <- data.frame(UF = factor(c("RJ", "SP")), value = 1:2)
  numeric <- data.frame(cod_uf = c(33L, 35L), value = 1:2)
  numeric_character <- data.frame(cod_uf = c("33", "35"), value = 1:2)

  expect_equal(.vigiar_filtrar_uf(labelled, "RJ")$value, 1L)
  expect_equal(.vigiar_filtrar_uf(numeric, "RJ")$value, 1L)
  expect_equal(.vigiar_filtrar_uf(numeric_character, "RJ")$value, 1L)
})

test_that("RJ server filter uses numeric semantics for cod_uf", {
  old_schema <- .vigiar_env$esquema
  on.exit({ .vigiar_env$esquema <- old_schema }, add = TRUE)

  .vigiar_env$esquema <- list(
    numeric_uf = list(cod_uf = list(tipo = "int64")),
    label_uf = list(UF = list(tipo = "string"))
  )

  expect_identical(.vigiar_filtro_servidor_rj("numeric_uf"), list(cod_uf = 33L))
  expect_identical(.vigiar_filtro_servidor_rj("label_uf"), list(UF = "RJ"))
})

test_that("conflicting server-side filters fail explicitly", {
  expect_error(
    .vigiar_combinar_filtros(list(UF = "SP"), list(UF = "RJ")),
    "Conflicting server-side filters"
  )
  expect_identical(
    .vigiar_combinar_filtros(list(cod_uf = "33"), list(cod_uf = 33L)),
    list(cod_uf = 33L)
  )
})

test_that("Power BI literals reject missing and non-finite values", {
  expect_identical(.vigiar_literal_powerbi(33L), "33L")
  expect_identical(.vigiar_literal_powerbi("O'Brien"), "'O''Brien'")
  expect_identical(.vigiar_literal_powerbi(TRUE), "true")
  expect_error(.vigiar_literal_powerbi(NA), "missing")
  expect_error(.vigiar_literal_powerbi(Inf), "finite")
})

test_that("municipality server filter uses the normalized IBGE code", {
  old_schema <- .vigiar_env$esquema
  on.exit({ .vigiar_env$esquema <- old_schema }, add = TRUE)
  .vigiar_env$esquema <- list(
    df_anual = list(muni = list(tipo = "int64")),
    seven = list(codigo_ibge_7 = list(tipo = "int64"))
  )

  expect_identical(
    .vigiar_filtro_servidor_municipio("df_anual", 330100L),
    list(muni = 330100L)
  )
  expect_identical(
    .vigiar_filtro_servidor_municipio("seven", 330100L),
    list(codigo_ibge_7 = 3301009L)
  )
})
