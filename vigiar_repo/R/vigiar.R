# Package: vigiar
# Core constants and session management

# URLs do dashboard VIGIAR
VIGIAR_BASE_URL <- "https://app.powerbi.com/view?r=eyJrIjoiNmRhODQwNzItNThlOS00ZmQ4LWJjZmItZDYxOTNhOTRmYmFhIiwidCI6IjlhNTU0YWQzLWI1MmItNDg2Mi1hMzZmLTg0ZDg5MWU1YzcwNSJ9"
VIGIAR_RESOURCE_KEY <- "6da84072-58e9-4fd8-bcfb-d6193a94fbaa"
VIGIAR_TENANT_ID <- "9a554ad3-b52b-4862-a36f-84d891e5c705"
VIGIAR_MODEL_ID <- 3930757L
VIGIAR_CLUSTER <- "https://wabi-brazil-south-b-primary-redirect.analysis.windows.net/"
VIGIAR_API_CLUSTER <- "https://wabi-brazil-south-b-primary-api.analysis.windows.net/"

# Ambiente interno do pacote
.vigiar_env <- new.env(parent = emptyenv())

#' Conecta ao dashboard VIGIAR do Power BI
#'
#' Estabelece uma sessão com o dashboard público do Power BI,
#' obtendo cookies e tokens necessários para as consultas de dados.
#'
#' @param atualizar Se TRUE, força uma nova conexão mesmo que já exista
#'   uma sessão ativa.
#' @param timeout Tempo máximo (em segundos) para estabelecer a conexão.
#' @return Invisivelmente, uma lista com os dados da sessão.
#' @export
vigiar_conectar <- function(atualizar = FALSE, timeout = 30) {
  if (!atualizar && !is.null(.vigiar_env$sessao)) {
    message("Sessão VIGIAR já está ativa. Use atualizar = TRUE para renovar.")
    return(invisible(.vigiar_env$sessao))
  }

  # Passo 1: Obter a página inicial do Power BI para pegar cookies e session ID
  resp <- httr2::request(VIGIAR_BASE_URL) |>
    httr2::req_user_agent(
      paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
             "AppleWebKit/537.36 (KHTML, like Gecko) ",
             "Chrome/131.0.0.0 Safari/537.36")
    ) |>
    httr2::req_timeout(timeout) |>
    httr2::req_perform()

  html_content <- httr2::resp_body_string(resp)

  # Extrair o telemetrySessionId do HTML
  session_id <- regmatches(
    html_content,
    regexpr("(?<=telemetrySessionId = ')[^']+", html_content, perl = TRUE)
  )
  if (length(session_id) == 0) {
    stop("Não foi possível extrair o telemetrySessionId do dashboard Power BI.")
  }

  # Extrair cookies da resposta. httr2 retorna set-cookie como header único
  # que pode conter múltiplos cookies separados por newline
  all_headers <- httr2::resp_headers(resp)
  set_cookie_raw <- all_headers[["set-cookie"]]

  # Se não achar diretamente, procurar case-insensitive
  if (is.null(set_cookie_raw)) {
    names_lower <- tolower(names(all_headers))
    idx <- which(names_lower == "set-cookie")
    if (length(idx) > 0) {
      set_cookie_raw <- all_headers[[idx[1]]]
    }
  }

  # Extrair cookies da string (podem estar em múltiplos headers ou um só)
  cookie_parts <- .vigiar_extrair_cookies(set_cookie_raw)

  # Se não conseguimos cookies, tentamos mesmo assim (alguns casos funcionam)
  if (length(cookie_parts) == 0) {
    warning("Não foi possível extrair cookies da resposta. ",
            "As consultas de dados podem falhar.")
    cookie_string <- ""
  } else {
    cookie_string <- paste(cookie_parts, collapse = "; ")
  }

  sessao <- list(
    session_id = session_id,
    cookies = cookie_string,
    resource_key = VIGIAR_RESOURCE_KEY,
    model_id = VIGIAR_MODEL_ID,
    api_url = VIGIAR_API_CLUSTER,
    criada_em = Sys.time()
  )

  .vigiar_env$sessao <- sessao

  # Também carregar o esquema conceitual
  message("Sessão VIGIAR estabelecida. Carregando esquema de dados...")
  .vigiar_env$esquema <- .vigiar_obter_esquema(sessao)

  tabelas <- names(.vigiar_env$esquema)
  message(sprintf("Sessão pronta! %d tabelas disponíveis.", length(tabelas)))

  invisible(sessao)
}

#' Extrai cookies do header set-cookie
#' @param set_cookie Valor(es) do header set-cookie
#' @return Vetor de strings de cookie (nome=valor)
#' @keywords internal
.vigiar_extrair_cookies <- function(set_cookie) {
  if (is.null(set_cookie) || length(set_cookie) == 0) return(character(0))

  # Se for uma lista/vector, concatenar
  if (length(set_cookie) > 1) {
    set_cookie <- paste(set_cookie, collapse = "\n")
  }

  # Separar por vírgula ou newline que separam cookies
  parts <- strsplit(set_cookie, "[\n,]\\s*")[[1]]

  # Extrair nome=valor de cada parte
  cookies <- character(0)
  for (part in parts) {
    part <- trimws(part)
    # Procurar por nome=valor antes do primeiro ;
    match <- regmatches(part, regexpr("^[^=;]+=[^;]+", part))
    if (length(match) > 0 && nchar(match) > 0) {
      cookies <- c(cookies, match)
    }
  }

  unique(cookies)
}

#' Obtém o esquema conceitual do dashboard
#' @param sessao Lista com os dados da sessão
#' @return Lista com as tabelas e colunas do modelo
#' @keywords internal
.vigiar_obter_esquema <- function(sessao) {
  req_id <- uuid_v4()

  resp <- httr2::request(sprintf("%spublic/reports/%s/conceptualschema",
                                  sessao$api_url, sessao$resource_key)) |>
    httr2::req_headers(
      "X-PowerBI-ResourceKey" = sessao$resource_key,
      "ActivityId" = sessao$session_id,
      "RequestId" = req_id,
      "Accept" = "application/json",
      "Referer" = "https://app.powerbi.com/"
    ) |>
    httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36") |>
    httr2::req_headers(Cookie = sessao$cookies) |>
    httr2::req_perform()

  # Obtém corpo bruto (pode estar comprimido com gzip)
  raw_body <- httr2::resp_body_raw(resp)

  # Descomprime se necessário
  if (length(raw_body) >= 2 && raw_body[1] == 0x1f && raw_body[2] == 0x8b) {
    raw_body <- .vigiar_gunzip(raw_body)
  }

  schema_data <- jsonlite::fromJSON(rawToChar(raw_body), simplifyVector = FALSE)

  # Extrai entidades (tabelas)
  entities <- schema_data$schemas[[1]]$schema$Entities

  tabelas <- list()
  for (ent in entities) {
    nome <- ent$Name
    props <- ent$Properties
    colunas <- lapply(props, function(p) {
      list(
        nome = p$Name,
        tipo = .vigiar_tipo_dado(p$DataType)
      )
    })
    names(colunas) <- sapply(props, `[[`, "Name")
    tabelas[[nome]] <- colunas
  }

  tabelas
}

#' Mapeia códigos de tipo do Power BI para tipos do R
#' @param code Código numérico do tipo de dado
#' @return String com o tipo R correspondente
#' @keywords internal
.vigiar_tipo_dado <- function(code) {
  switch(as.character(code),
    "1" = "character",  # Text
    "2" = "numeric",    # Decimal (currency)
    "3" = "numeric",    # Double
    "4" = "integer",    # Integer
    "5" = "logical",    # Boolean
    "6" = "Date",       # Date
    "7" = "POSIXct",    # DateTime
    "8" = "integer",    # Int64 (aproximado)
    "character"         # default
  )
}

#' Descomprime dados em formato gzip
#' @param raw_body Vetor raw com dados comprimidos
#' @return Vetor raw descomprimido
#' @keywords internal
.vigiar_gunzip <- function(raw_body) {
  tmp <- tempfile(fileext = ".gz")
  writeBin(raw_body, tmp)
  con <- gzfile(tmp, "rb")
  chunks <- list()
  repeat {
    chunk <- readBin(con, raw(), 65536L)
    if (length(chunk) == 0) break
    chunks[[length(chunks) + 1]] <- chunk
  }
  close(con)
  unlink(tmp)
  do.call(c, chunks)
}

#' Gera um UUID v4 simples (sem dependência extra)
#' @keywords internal
uuid_v4 <- function() {
  hex <- c(0:9, "a", "b", "c", "d", "e", "f")
  parts <- c(
    paste0(sample(hex, 8, replace = TRUE), collapse = ""),
    paste0(sample(hex, 4, replace = TRUE), collapse = ""),
    paste0("4", sample(hex[1:4], 3, replace = TRUE), collapse = ""),
    paste0(sample(c("8", "9", "a", "b"), 1),
           paste0(sample(hex, 3, replace = TRUE), collapse = ""), collapse = ""),
    paste0(sample(hex, 12, replace = TRUE), collapse = "")
  )
  paste(parts, collapse = "-")
}
