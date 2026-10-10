# vigiar: variable dictionary query interface

test_that("vigiar_dicionario returns the documented dictionary", {
  dict <- vigiar_dicionario()

  expect_s3_class(dict, "tbl_df")
  expect_gt(nrow(dict), 0)
  expect_true(all(c(
    "table_id", "table_name", "original_name", "standard_name",
    "type_processed", "description"
  ) %in% names(dict)))
})

test_that("vigiar_variaveis filters by domain", {
  dict <- vigiar_dicionario()
  expect_equal(nrow(vigiar_variaveis("all")), nrow(dict))

  pm <- vigiar_variaveis("pm25")
  expect_true(nrow(pm) > 0)
  expect_true(all(pm$table_id %in%
                    c("df_anual", "df_mensal", "df_dias", "df_dias_conama")))

  expect_error(vigiar_variaveis("dominio_invalido"), "arg")
})

test_that("every advertised domain is populated in the dictionary", {
  for (dom in c(
    "pm25", "populacao_exposta", "indicadores_saude",
    "fracao_atribuivel", "exposicao_indoor", "municipios"
  )) {
    expect_true(nrow(vigiar_variaveis(dom)) > 0,
      info = paste("dominio vazio:", dom)
    )
  }
})

test_that("vigiar_descrever_variavel prints a known variable and errors on unknown", {
  pm <- vigiar_variaveis("pm25")
  variavel <- pm$standard_name[!is.na(pm$standard_name) & nzchar(pm$standard_name)][1]
  skip_if(is.na(variavel), "dictionary has no standard names for pm25")

  expect_output(vigiar_descrever_variavel("pm25", variavel), "Variavel")
  expect_error(
    vigiar_descrever_variavel("pm25", "variavel_inexistente"),
    "nao encontrada"
  )
})

test_that("vigiar_schema returns the documentation summary", {
  schema <- vigiar_schema("municipios")
  expect_s3_class(schema, "tbl_df")
  expect_named(schema, c(
    "table_id", "standard_name", "type_processed",
    "description", "unit"
  ))
})

test_that("vigiar_convencoes opens the documentation URL", {
  captured <- NULL
  local_mocked_bindings(
    browseURL = function(url, ...) {
      captured <<- url
      invisible(NULL)
    },
    .package = "utils"
  )

  vigiar_convencoes()
  expect_match(captured, "convencoes-vigiar", fixed = TRUE)
})

test_that(".vigiar_dicionario_interno filters by table", {
  rows <- .vigiar_dicionario_interno("df_anual")
  expect_true(is.null(rows) || all(rows$table_id == "df_anual"))
})
