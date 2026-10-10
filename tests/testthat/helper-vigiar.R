# vigiar: shared helpers for the offline, deterministic test suite.
#
# These helpers build synthetic Power BI responses so the connection,
# query and download code paths can be tested without network access.

# Reset every piece of mutable package state so tests never leak into
# each other (important when testthat runs files in parallel).
.vigiar_reset_state <- function() {
  .vigiar_env$sessao <- NULL
  .vigiar_env$client <- NULL
  .vigiar_env$esquema <- NULL
  .vigiar_env$status_verificado <- FALSE
  .vigiar_env$log <- list()
  .vigiar_env$download_history <- list()
  .vigiar_env$cache_dir <- NULL
  invisible(NULL)
}

# Reset state now and automatically restore it when the enclosing test ends.
.vigiar_local_reset <- function(env = parent.frame()) {
  .vigiar_reset_state()
  withr::defer(.vigiar_reset_state(), envir = env)
  invisible(NULL)
}

# A fully-formed (but fake) session object.
.vigiar_fake_sessao <- function() {
  structure(
    list(
      session_id   = "sess-test",
      cookies      = "ARRAffinity=abc",
      resource_key = "key-test",
      model_id     = 1L,
      api_url      = "https://example.invalid/",
      created_at   = Sys.time()
    ),
    class = "vigiar_sessao"
  )
}

# A minimal conceptual schema with the columns used across tests.
.vigiar_fake_esquema <- function() {
  list(
    df_anual = list(
      ano        = list(nome = "ano", tipo = "integer"),
      muni       = list(nome = "muni", tipo = "integer"),
      Media_pm25 = list(nome = "Media_pm25", tipo = "numeric")
    ),
    df_muni = list(
      cod_municipio = list(nome = "cod_municipio", tipo = "integer"),
      municipio     = list(nome = "municipio", tipo = "character")
    ),
    pop = list(
      muni = list(nome = "muni", tipo = "integer"),
      ano  = list(nome = "ano", tipo = "integer")
    )
  )
}

# JSON body returned by the Power BI conceptualschema endpoint.
.vigiar_fake_schema_json <- function() {
  to_entity <- function(nm, props) {
    list(
      Name = nm,
      Properties = lapply(names(props), function(p) {
        list(Name = p, DataType = props[[p]])
      })
    )
  }
  entities <- list(
    to_entity("df_anual", list(ano = 8L, muni = 4L, Media_pm25 = 3L)),
    to_entity("df_muni", list(cod_municipio = 4L, municipio = 1L))
  )
  jsonlite::toJSON(
    list(schemas = list(list(schema = list(Entities = entities)))),
    auto_unbox = TRUE
  )
}

# A session + schema ready for download/benchmark code paths.
.vigiar_ready_session <- function(env = parent.frame()) {
  .vigiar_local_reset(env)
  .vigiar_env$sessao <- .vigiar_fake_sessao()
  .vigiar_env$esquema <- .vigiar_fake_esquema()
  .vigiar_env$status_verificado <- TRUE
  invisible(NULL)
}

# Drop-in replacement for httr2::req_perform used with local_mocked_bindings.
.vigiar_mock_perform <- function(req, ...) {
  url <- req$url
  if (grepl("conceptualschema", url, fixed = TRUE)) {
    return(httr2::response(
      200L,
      headers = list("content-type" = "application/json"),
      body = charToRaw(.vigiar_fake_schema_json())
    ))
  }
  html <- "<html><script>telemetrySessionId = 'sess-test';</script></html>"
  httr2::response(
    200L,
    headers = list("set-cookie" = "ARRAffinity=abc; Path=/; HttpOnly"),
    body = charToRaw(html)
  )
}
