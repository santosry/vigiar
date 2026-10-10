# vigiar: connection lifecycle with a mocked Power BI endpoint
#
# These tests exercise the full session/schema code path without network
# access by replacing httr2::req_perform with a deterministic fake.

test_that("vigiar_conectar builds a session and loads the schema", {
  .vigiar_local_reset()
  local_mocked_bindings(
    req_perform = .vigiar_mock_perform,
    .package = "httr2"
  )

  sess <- suppressMessages(vigiar_conectar(timeout = 5, max_retries = 1))

  expect_s3_class(sess, "vigiar_sessao")
  expect_equal(sess$session_id, "sess-test")
  expect_equal(sess$cookies, "ARRAffinity=abc")
  expect_true(vigiar_sessao_ativa())
  expect_setequal(names(.vigiar_env$esquema), c("df_anual", "df_muni"))
  expect_setequal(
    names(.vigiar_env$esquema$df_anual),
    c("ano", "muni", "Media_pm25")
  )
})

test_that("vigiar_conectar reuses an active session unless refreshed", {
  .vigiar_local_reset()
  calls <- 0L
  local_mocked_bindings(
    req_perform = function(req, ...) {
      calls <<- calls + 1L
      .vigiar_mock_perform(req, ...)
    },
    .package = "httr2"
  )

  suppressMessages(vigiar_conectar(timeout = 5, max_retries = 1))
  first_calls <- calls
  expect_message(vigiar_conectar(timeout = 5, max_retries = 1), "ja esta ativa")
  expect_equal(calls, first_calls)

  suppressMessages(vigiar_conectar(refresh = TRUE, timeout = 5, max_retries = 1))
  expect_gt(calls, first_calls)
})

test_that("vigiar_conectar errors when the session token is missing", {
  .vigiar_local_reset()
  local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::response(200L, body = charToRaw("<html></html>"))
    },
    .package = "httr2"
  )
  expect_error(
    suppressMessages(vigiar_conectar(timeout = 5, max_retries = 1)),
    "telemetrySessionId"
  )
})

test_that("vigiar_conectar warns when no cookies are returned", {
  .vigiar_local_reset()
  local_mocked_bindings(
    req_perform = function(req, ...) {
      if (grepl("conceptualschema", req$url, fixed = TRUE)) {
        return(httr2::response(200L, body = charToRaw(.vigiar_fake_schema_json())))
      }
      html <- "<script>telemetrySessionId = 'sess-x';</script>"
      httr2::response(200L, body = charToRaw(html))
    },
    .package = "httr2"
  )
  expect_warning(
    suppressMessages(vigiar_conectar(timeout = 5, max_retries = 1)),
    "cookies"
  )
})

test_that("vigiar_desconectar clears the session", {
  .vigiar_local_reset()
  .vigiar_env$sessao <- .vigiar_fake_sessao()
  .vigiar_env$esquema <- .vigiar_fake_esquema()

  expect_message(vigiar_desconectar(), "encerrada")
  expect_false(vigiar_sessao_ativa())
  expect_null(.vigiar_env$esquema)
})

test_that("vigiar_status reports offline without a session", {
  .vigiar_local_reset()
  expect_message(res <- vigiar_status(), "Nenhuma sessao ativa")
  expect_false(res$online)
  expect_false(res$tables_ok)
})

test_that("vigiar_status confirms a consistent live schema", {
  .vigiar_local_reset()
  local_mocked_bindings(req_perform = .vigiar_mock_perform, .package = "httr2")
  suppressMessages(vigiar_conectar(timeout = 5, max_retries = 1))

  expect_message(res <- vigiar_status(), "online")
  expect_true(res$online)
  expect_true(res$tables_ok)
  expect_true(.vigiar_env$status_verificado)
})

test_that("vigiar_status warns when the live schema changed", {
  .vigiar_local_reset()
  local_mocked_bindings(req_perform = .vigiar_mock_perform, .package = "httr2")
  suppressMessages(vigiar_conectar(timeout = 5, max_retries = 1))

  # Simulate a cached schema that is missing a table seen live.
  .vigiar_env$esquema <- .vigiar_env$esquema["df_anual"]

  expect_warning(res <- suppressMessages(vigiar_status()), "mudou")
  expect_true(res$online)
  expect_false(res$tables_ok)
  expect_true("df_muni" %in% res$new_tables)
})

test_that("vigiar_client returns a self-contained client", {
  .vigiar_local_reset()
  local_mocked_bindings(req_perform = .vigiar_mock_perform, .package = "httr2")

  cli <- suppressMessages(vigiar_client(timeout = 5, max_retries = 1))
  expect_s3_class(cli, "vigiar_client")
  expect_equal(cli$session_id, "sess-test")
  expect_setequal(names(cli$schema), c("df_anual", "df_muni"))
  expect_output(print(cli), "Cliente VIGIAR")
})

test_that("vigiar_client validates its arguments", {
  expect_error(vigiar_client(timeout = 1), "timeout")
  expect_error(vigiar_client(max_retries = 0), "max_retries")
})
