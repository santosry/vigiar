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
