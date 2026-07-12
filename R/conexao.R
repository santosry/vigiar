# Package: vigiar
# Session management and dashboard connection
#
# Handles Power BI anonymous "Publish to Web" session lifecycle:
#   1. Fetch dashboard page -> extract cookies + telemetrySessionId
#   2. Fetch conceptual schema -> discover tables and columns
#   3. Maintain session in package environment

#' Connect to the VIGIAR Power BI dashboard
#'
#' Establishes an anonymous session with the public VIGIAR Power BI
#' dashboard, obtaining the cookies and session token required for
#' subsequent data queries. Also fetches the conceptual schema
#' (table and column metadata).
#'
#' @param refresh If `TRUE`, forces a new session even if one exists.
#' @param timeout Maximum time in seconds to establish the connection.
#' @param max_retries Maximum number of retry attempts on transient failures.
#' @return Invisibly, a list with session data.
#' @export
vigiar_conectar <- function(refresh = FALSE, timeout = 30, max_retries = 3) {
  if (!refresh && !is.null(.vigiar_env$sessao)) {
    .vigiar_log("INFO", "Connection reused an active VIGIAR session.",
                event = "connect_reuse")
    message("A VIGIAR session is already active. Use refresh = TRUE to renew it.")
    return(invisible(.vigiar_env$sessao))
  }

  .vigiar_log(
    "INFO", "Starting VIGIAR dashboard connection.",
    metadata = list(timeout_seconds = timeout, max_retries = max_retries),
    event = "connect_start"
  )

  # Step 1 -- Fetch dashboard page
  resp <- tryCatch(
    .vigiar_retry(
      {
        httr2::request(VIGIAR_BASE_URL) |>
          httr2::req_user_agent(.vigiar_ua()) |>
          httr2::req_timeout(timeout) |>
          httr2::req_perform()
      },
      max_tries = max_retries,
      context = "connect"
    ),
    error = function(e) {
      .vigiar_log(
        "ERROR", paste("VIGIAR connection failed:", conditionMessage(e)),
        event = "connect_failure"
      )
      stop(e)
    }
  )

  html_content <- httr2::resp_body_string(resp)

  # Extract telemetrySessionId from JavaScript
  session_id <- regmatches(
    html_content,
    regexpr("(?<=telemetrySessionId = ')[^']+", html_content, perl = TRUE)
  )
  if (length(session_id) == 0) {
    .vigiar_log(
      "ERROR", "The Power BI telemetry session identifier was not found.",
      event = "connect_failure"
    )
    stop(
      "Could not extract telemetrySessionId from the Power BI dashboard. ",
      "The dashboard may be temporarily unavailable."
    )
  }

  # Extract cookies from response headers
  all_headers <- httr2::resp_headers(resp)
  set_cookie_raw <- all_headers[["set-cookie"]]

  if (is.null(set_cookie_raw)) {
    names_lower <- tolower(names(all_headers))
    idx <- which(names_lower == "set-cookie")
    if (length(idx) > 0) set_cookie_raw <- all_headers[[idx[1]]]
  }

  cookie_parts <- .vigiar_extrair_cookies(set_cookie_raw)

  if (length(cookie_parts) == 0) {
    warning(
      "Could not extract cookies from the response. ",
      "Data queries may fail."
    )
    cookie_string <- ""
  } else {
    cookie_string <- paste(cookie_parts, collapse = "; ")
  }

  # Build session object
  sessao <- list(
    session_id   = session_id,
    cookies      = cookie_string,
    resource_key = VIGIAR_RESOURCE_KEY,
    model_id     = VIGIAR_MODEL_ID,
    api_url      = VIGIAR_API_CLUSTER,
    created_at   = Sys.time()
  )
  class(sessao) <- "vigiar_sessao"

  .vigiar_env$sessao <- sessao

  # Step 2 -- Fetch conceptual schema
  message("VIGIAR session established. Loading the data schema...")
  .vigiar_env$esquema <- tryCatch(
    .vigiar_obter_esquema(sessao, timeout = timeout),
    error = function(e) {
      .vigiar_env$sessao <- NULL
      .vigiar_log(
        "ERROR", paste("Schema loading failed:", conditionMessage(e)),
        event = "connect_failure"
      )
      stop(e)
    }
  )

  n_tables <- length(.vigiar_env$esquema)
  .vigiar_log(
    "INFO", sprintf("VIGIAR connection ready with %d tables.", n_tables),
    metadata = list(n_tables = n_tables), event = "connect_success"
  )
  message(sprintf("Session ready: %d tables available.", n_tables))

  invisible(sessao)
}

#' Disconnect and clear VIGIAR session
#'
#' @return Invisibly, `NULL`.
#' @export
vigiar_desconectar <- function() {
  was_active <- !is.null(.vigiar_env$sessao)
  .vigiar_env$sessao  <- NULL
  .vigiar_env$esquema <- NULL
  .vigiar_log(
    "INFO", "VIGIAR session disconnected.",
    metadata = list(was_active = was_active), event = "disconnect"
  )
  message("VIGIAR session disconnected.")
  invisible(NULL)
}

#' Check if a VIGIAR session is active
#'
#' @return `TRUE` if a session exists, `FALSE` otherwise.
#' @export
vigiar_sessao_ativa <- function() {
  !is.null(.vigiar_env$sessao)
}

# -- Internal helpers ----------------------------------------------------------

.vigiar_ua <- function() {
  paste0(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) ",
    "Chrome/131.0.0.0 Safari/537.36"
  )
}

#' Fetch conceptual schema from Power BI
#' @param sessao Session list
#' @param timeout Timeout in seconds
#' @return Named list of tables, each with named column metadata
#' @keywords internal
.vigiar_obter_esquema <- function(sessao, timeout = 30) {
  req_id <- uuid_v4()
  url <- sprintf(
    "%spublic/reports/%s/conceptualschema",
    sessao$api_url, sessao$resource_key
  )

  resp <- .vigiar_retry(
    {
      httr2::request(url) |>
        httr2::req_headers(
          "X-PowerBI-ResourceKey" = sessao$resource_key,
          ActivityId              = sessao$session_id,
          RequestId               = req_id,
          Accept                  = "application/json",
          Referer                 = "https://app.powerbi.com/",
          Cookie                  = sessao$cookies
        ) |>
        httr2::req_user_agent(.vigiar_ua()) |>
        httr2::req_timeout(timeout) |>
        httr2::req_perform()
    },
    max_tries = 2,
    context   = "schema"
  )

  raw_body <- httr2::resp_body_raw(resp)
  raw_body <- .vigiar_gunzip(raw_body)

  schema_data <- jsonlite::fromJSON(
    rawToChar(raw_body),
    simplifyVector = FALSE
  )

  entities <- schema_data$schemas[[1L]]$schema$Entities

  tabelas <- list()
  for (ent in entities) {
    nome <- ent$Name
    props <- ent$Properties
    colunas <- lapply(props, function(p) {
      list(nome = p$Name, tipo = .vigiar_tipo_dado(p$DataType))
    })
    names(colunas) <- vapply(props, `[[`, "", "Name", USE.NAMES = FALSE)
    tabelas[[nome]] <- colunas
  }

  tabelas
}

#' Check VIGIAR dashboard status
#'
#' Verifies that the Power BI dashboard is reachable and the
#' conceptual schema is unchanged from the cached version.
#'
#' @return Invisibly, a list with status information.
#' @export
vigiar_status <- function() {
  if (is.null(.vigiar_env$sessao)) {
    message("No active session.")
    return(invisible(list(
      online = FALSE, tables_ok = FALSE, status = "unknown"
    )))
  }

  online <- FALSE
  tryCatch({
    esquema <- .vigiar_obter_esquema(.vigiar_env$sessao, timeout = 10)
    online <- TRUE
    cached_tables <- names(.vigiar_env$esquema)
    live_tables   <- names(esquema)
    new_tables    <- setdiff(live_tables, cached_tables)
    missing_tables <- setdiff(cached_tables, live_tables)

    tables_ok <- length(new_tables) == 0 && length(missing_tables) == 0
  }, error = function(e) {
    online <<- FALSE
    new_tables <<- character(0)
    missing_tables <<- character(0)
    tables_ok <<- FALSE
  })

  status <- list(
    online        = online,
    tables_ok     = tables_ok,
    status        = if (!online) "unknown" else if (tables_ok) "pass" else "fail",
    new_tables    = if (exists("new_tables")) new_tables else character(0),
    missing_tables = if (exists("missing_tables")) missing_tables else character(0)
  )

  if (online && tables_ok) {
    message("The VIGIAR dashboard is online and table names are unchanged.")
  } else if (online) {
    .vigiar_log(
      "ERROR", "The live Power BI table set differs from the cached schema.",
      metadata = list(new_tables = new_tables, missing_tables = missing_tables),
      event = "schema_change"
    )
    warning(
      "The VIGIAR dashboard is online, but the table schema changed. ",
      "Run vigiar_conectar(refresh = TRUE) to refresh it."
    )
  } else {
    warning("The VIGIAR dashboard is unavailable or inaccessible.")
  }

  invisible(status)
}
