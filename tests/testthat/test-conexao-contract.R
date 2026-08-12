# vigiar: contrato do mecanismo de conexao com o dashboard Power BI
#
# Este teste valida, contra o dashboard real, que o padrao de extracao do
# `telemetrySessionId` e dos cookies de sessao ainda funciona. Como o endpoint
# "Publish to Web" e interno e nao contratual, mudancas de schema/HTML podem
# quebrar a regex silenciosamente; o teste de contrato transforma essa falha
# em uma falha de build (CI agendado semanalmente) antes que o usuario final
# seja afetado.
#
# Nao executa em CRAN nem em maquinas offline. Em CI regular (push/PR) e
# desligado pela variavel VIGIAR_RUN_ONLINE_TESTS (veja .github/workflows).

skip_on_cran()
skip_if_offline()

online <- identical(tolower(Sys.getenv("VIGIAR_RUN_ONLINE_TESTS")), "true")
if (!online) {
  skip("Teste de contrato desligado. Defina VIGIAR_RUN_ONLINE_TESTS=true para rodar.")
}

test_that("padrao de extracao do telemetrySessionId continua valido", {
  resp <- httr2::request(VIGIAR_BASE_URL) |>
    httr2::req_user_agent(.vigiar_ua()) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()

  html_content <- httr2::resp_body_string(resp)

  session_id <- regmatches(
    html_content,
    regexpr("(?<=telemetrySessionId = ')[^']+", html_content, perl = TRUE)
  )

  # Se a estrutura do HTML mudou e a regex nao casou, este teste falha aqui,
  # sinalizando a quebra antes de qualquer usuario final.
  expect_true(length(session_id) > 0,
    info = "telemetrySessionId nao encontrado no HTML do dashboard"
  )
  expect_true(nzchar(session_id[1]))
})

test_that("cookies de sessao continuam disponiveis no header set-cookie", {
  resp <- httr2::request(VIGIAR_BASE_URL) |>
    httr2::req_user_agent(.vigiar_ua()) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()

  all_headers <- httr2::resp_headers(resp)
  set_cookie_raw <- all_headers[["set-cookie"]]
  if (is.null(set_cookie_raw)) {
    names_lower <- tolower(names(all_headers))
    idx <- which(names_lower == "set-cookie")
    if (length(idx) > 0) set_cookie_raw <- all_headers[[idx[1]]]
  }

  cookies <- .vigiar_extrair_cookies(set_cookie_raw)

  expect_true(length(cookies) > 0,
    info = "Nenhum cookie de sessao extraido do header set-cookie"
  )
})
