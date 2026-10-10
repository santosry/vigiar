# Package: vigiar
# Power BI client abstraction
#
# Encapsulates session management and API communication
# behind a clean S3 interface.  Replaces the global .vigiar_env
# pattern with explicit session objects.

#' Create a VIGIAR Power BI client
#'
#' Returns an S3 object that holds session state and provides
#' methods for data download.  All state is self-contained;
#' no global variables are used.
#'
#' @param timeout Seconds before HTTP timeout.
#' @param max_retries Maximum retry attempts on transient failures.
#' @return An object of class \code{vigiar_client}.
#' @examples
#' \dontrun{
#' client <- vigiar_client()
#' client
#' }
#' @export
vigiar_client <- function(timeout = 30, max_retries = 3) {
  # Validate arguments (checkmate — microdatasus pattern)
  checkmate::assert_number(timeout, lower = 5, upper = 300)
  checkmate::assert_integerish(max_retries, lower = 1, upper = 10)

  # Step 1: fetch dashboard page for cookies + session token (shared helper)
  handshake <- .vigiar_obter_sessao_dashboard(
    timeout     = timeout,
    max_retries = max_retries,
    context     = "client"
  )
  sid <- handshake$session_id
  cookies <- paste(handshake$cookie_parts, collapse = "; ")

  # Step 2: fetch conceptual schema
  schema <- .vigiar_fetch_schema(sid, cookies, timeout)

  client <- list(
    session_id = sid,
    cookies    = cookies,
    schema     = schema,
    api_url    = VIGIAR_API_CLUSTER,
    rk         = VIGIAR_RESOURCE_KEY,
    model_id   = VIGIAR_MODEL_ID,
    created    = Sys.time()
  )
  class(client) <- "vigiar_client"

  n_tables <- length(schema)
  message(sprintf("VIGIAR conectado: %d tabelas disponiveis.", n_tables))
  client
}

#' @export
#' @param x A \code{vigiar_client} object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly, \code{x}.
#' @examples
#' \dontrun{
#' print(vigiar_client())
#' }
print.vigiar_client <- function(x, ...) {
  cat(sprintf("VIGIAR client -- %d tabelas\n", length(x$schema)))
  cat(sprintf("  Criado em: %s\n", format(x$created)))
  cat(sprintf("  Tabelas:   %s\n",
              paste(names(x$schema)[1:5], collapse = ", ")))
  invisible(x)
}

# ---- Internal: fetch schema --------------------------------------------------

# Thin wrapper kept for internal callers; delegates to the canonical schema
# fetcher in conexao.R so the parsing logic exists in a single place.
.vigiar_fetch_schema <- function(sid, cookies, timeout) {
  .vigiar_obter_esquema(
    list(
      session_id   = sid,
      cookies      = cookies,
      resource_key = VIGIAR_RESOURCE_KEY,
      api_url      = VIGIAR_API_CLUSTER
    ),
    timeout = timeout
  )
}

# Keep backward compatibility: global session
.vigiar_client_default <- function() {
  if (is.null(.vigiar_env$client)) {
    .vigiar_env$client <- vigiar_client()
  }
  .vigiar_env$client
}
