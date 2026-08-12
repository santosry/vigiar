# vigiar: testes offline para o prerequisito de vigiar_status()
#
# Em pipelines nao interativos, vigiar_baixar() deve exigir que o schema ao
# vivo tenha sido validado por vigiar_status() antes do download.

library(testthat)
library(vigiar)

test_that("vigiar_baixar exige vigiar_status em pipelines nao interativos", {
  old_sessao <- .vigiar_env$sessao
  old_esquema <- .vigiar_env$esquema
  old_flag <- .vigiar_env$status_verificado
  on.exit({
    .vigiar_env$sessao <- old_sessao
    .vigiar_env$esquema <- old_esquema
    .vigiar_env$status_verificado <- old_flag
  })

  .vigiar_env$sessao <- list(
    session_id = "abc",
    cookies = "",
    resource_key = "k",
    model_id = 1L,
    api_url = "https://example.com"
  )
  .vigiar_env$esquema <- list(teste = list())
  .vigiar_env$status_verificado <- FALSE

  expect_error(
    vigiar_baixar("teste"),
    "vigiar_status"
  )
})

test_that("vigiar_status sem sessao marca status como nao verificado", {
  old_sessao <- .vigiar_env$sessao
  on.exit(.vigiar_env$sessao <- old_sessao)

  .vigiar_env$sessao <- NULL
  res <- suppressMessages(vigiar_status())

  expect_false(res$online)
  expect_false(res$tables_ok)
  expect_false(.vigiar_env$status_verificado)
})
