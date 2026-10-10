# vigiar: Power BI query construction and execution

test_that(".vigiar_construir_query builds a SemanticQueryDataShapeCommand", {
  .vigiar_local_reset()
  q <- .vigiar_construir_query(
    tabela = "df_anual",
    colunas = c("ano", "muni"),
    modelo_id = 42L
  )

  expect_equal(q$modelId, 42L)
  expect_equal(q$version, 1L)

  cmd <- q$queries[[1]]$Query$Commands[[1]]$SemanticQueryDataShapeCommand
  expect_equal(cmd$Query$From[[1]]$Entity, "df_anual")
  expect_length(cmd$Query$Select, 2)
  expect_equal(cmd$Query$Select[[1]]$Name, "ano")
  expect_equal(cmd$Query$Top, 50000L)
})

test_that(".vigiar_construir_query honours limite and ordering", {
  q <- .vigiar_construir_query(
    tabela = "df_anual",
    colunas = c("ano"),
    ordenar_por = "ano",
    limite = 10L,
    modelo_id = 1L,
    direcao = 2L
  )
  cmd <- q$queries[[1]]$Query$Commands[[1]]$SemanticQueryDataShapeCommand
  expect_equal(cmd$Query$Top, 10L)
  expect_equal(cmd$Query$OrderBy[[1]]$Direction, 2L)
  expect_equal(cmd$Query$OrderBy[[1]]$Expression$Column$Property, "ano")
})

test_that(".vigiar_construir_query defaults to all schema columns", {
  .vigiar_local_reset()
  .vigiar_env$esquema <- .vigiar_fake_esquema()
  q <- .vigiar_construir_query(tabela = "df_anual", modelo_id = 1L)
  cmd <- q$queries[[1]]$Query$Commands[[1]]$SemanticQueryDataShapeCommand
  expect_length(cmd$Query$Select, 3)
})

test_that(".vigiar_executar_query posts and parses JSON", {
  local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::response(
        200L,
        headers = list("content-type" = "application/json"),
        body = charToRaw('{"results":[{"tables":[]}]}')
      )
    },
    .package = "httr2"
  )

  res <- .vigiar_executar_query(.vigiar_fake_sessao(), list(a = 1), timeout = 5)
  expect_type(res, "list")
  expect_true("results" %in% names(res))
})

test_that(".vigiar_executar_query surfaces API-level errors", {
  local_mocked_bindings(
    req_perform = function(req, ...) {
      httr2::response(200L, body = charToRaw('{"error":{"message":"boom"}}'))
    },
    .package = "httr2"
  )
  expect_error(
    .vigiar_executar_query(.vigiar_fake_sessao(), list(), timeout = 5),
    "boom"
  )
})

test_that(".vigiar_executar_query decompresses gzip payloads", {
  tmp <- tempfile(fileext = ".gz")
  con <- gzfile(tmp, "wb")
  writeBin(charToRaw('{"results":[{"ok":true}]}'), con)
  close(con)
  gz <- readBin(tmp, raw(), file.info(tmp)$size)
  unlink(tmp)

  local_mocked_bindings(
    req_perform = function(req, ...) httr2::response(200L, body = gz),
    .package = "httr2"
  )
  res <- .vigiar_executar_query(.vigiar_fake_sessao(), list(), timeout = 5)
  expect_true("results" %in% names(res))
})
