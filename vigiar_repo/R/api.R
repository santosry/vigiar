# Package: vigiar
# Funções de interação com a API do Power BI

#' Constrói a query DAX no formato esperado pela API do Power BI
#'
#' @param tabela Nome da tabela/entidade no modelo
#' @param colunas Vetor de nomes de colunas a selecionar (NULL = todas)
#' @param ordenar_por Coluna para ordenação (opcional)
#' @param limite Número máximo de linhas (NULL = sem limite)
#' @param modelo_id ID do modelo no Power BI
#' @return Lista no formato JSON esperado pela API queryData
#' @keywords internal
.vigiar_construir_query <- function(tabela, colunas = NULL, ordenar_por = NULL,
                                     limite = NULL, modelo_id) {
  # Se colunas não especificadas, pegar do esquema
  if (is.null(colunas)) {
    colunas <- names(.vigiar_env$esquema[[tabela]])
  }

  # Construir Select array
  selects <- list()
  for (i in seq_along(colunas)) {
    col <- colunas[i]
    selects[[i]] <- list(
      Column = list(
        Expression = list(SourceRef = list(Source = tabela)),
        Property = col
      ),
      Name = col
    )
  }

  # Construir OrderBy (se especificado)
  order_by <- list()
  if (!is.null(ordenar_por)) {
    order_by <- list(list(
      Direction = 1,  # 1 = ascendente
      Expression = list(
        Column = list(
          Expression = list(SourceRef = list(Source = tabela)),
          Property = ordenar_por
        )
      )
    ))
  }

  # Construir a query completa
  query_cmd <- list(
    SemanticQueryDataShapeCommand = list(
      Query = list(
        Version = 2,
        From = list(list(Name = tabela, Entity = tabela)),
        Select = selects
      ),
      Binding = list(
        Primary = list(
          Groupings = list(list(Projections = seq(0, length(colunas) - 1)))
        ),
        Version = 1
      )
    )
  )

  # Adicionar OrderBy se existir
  if (length(order_by) > 0) {
    query_cmd$SemanticQueryDataShapeCommand$Query$OrderBy <- order_by
  }

  # Adicionar Top (limite) se especificado
  # O formato esperado pela API é um inteiro simples
  if (!is.null(limite)) {
    query_cmd$SemanticQueryDataShapeCommand$Query$Top <- as.integer(limite)
  }

  # Query wrapper
  list(
    version = 1,
    cancelQueries = list(),
    queries = list(list(
      Query = list(Commands = list(query_cmd)),
      CacheKey = "",
      QueryId = "",
      ApplicationContext = list(
        Sources = list(),
        DatasetId = as.character(modelo_id)
      )
    )),
    modelId = modelo_id
  )
}

#' Executa uma consulta contra a API queryData do Power BI
#'
#' @param sessao Sessão ativa do VIGIAR
#' @param query_body Corpo da query em formato de lista R
#' @return Resposta bruta da API (lista R)
#' @keywords internal
.vigiar_executar_query <- function(sessao, query_body) {
  req_id <- uuid_v4()
  body_json <- jsonlite::toJSON(query_body, auto_unbox = TRUE,
                                 null = "null", digits = NA)

  resp <- httr2::request(sprintf("%spublic/reports/querydata?synchronous=true",
                                  sessao$api_url)) |>
    httr2::req_headers(
      "X-PowerBI-ResourceKey" = sessao$resource_key,
      "ActivityId" = sessao$session_id,
      "RequestId" = req_id,
      "Accept" = "application/json",
      "Content-Type" = "application/json",
      "Referer" = "https://app.powerbi.com/"
    ) |>
    httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36") |>
    httr2::req_headers(Cookie = sessao$cookies) |>
    httr2::req_body_raw(body_json) |>
    httr2::req_method("POST") |>
    httr2::req_perform()

  raw_body <- httr2::resp_body_raw(resp)

  # Descomprime gzip se necessário
  if (length(raw_body) >= 2 && raw_body[1] == 0x1f && raw_body[2] == 0x8b) {
    raw_body <- .vigiar_gunzip(raw_body)
  }

  jsonlite::fromJSON(rawToChar(raw_body), simplifyVector = FALSE)
}
