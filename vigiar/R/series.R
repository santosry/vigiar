# Package: vigiar
# Time series helpers (descriptive only -- no DLNM, GAM, or causal models)
#
# These functions aggregate VIGIAR data along the time dimension
# and compute simple descriptive statistics.  They do NOT fit models.

#' Aggregate VIGIAR data along the time dimension
#'
#' Groups data by time variables (ano, mes) and optional spatial
#' variables (UF, municipio, regiao) and computes descriptive
#' summaries of key numeric columns.
#'
#' @param dados A processed VIGIAR tibble (or any data frame with
#'   \code{ano} column).
#' @param agregar_por Character vector of grouping variables.
#'   Default: \code{c("ano")}. Other options: \code{"mes"},
#'   \code{"sigla_uf"}, \code{"cod_municipio"}, \code{"regiao"}.
#' @param variavel Name of the numeric column to summarise. If
#'   \code{NULL}, auto-detects based on the data class.
#' @param funcoes Named list of summary functions. Default:
#'   \code{list(media = mean, n = length)}.
#' @return A tibble with grouping columns and summary columns.
#' @export
vigiar_agregar_tempo <- function(dados,
                                  agregar_por = c("ano"),
                                  variavel = NULL,
                                  funcoes = list(media = function(x) mean(x, na.rm = TRUE),
                                                 n     = length)) {
  if (!"ano" %in% names(dados)) {
    stop("A coluna 'ano' e obrigatoria para agregacao temporal.")
  }

  # Validate grouping columns
  validas <- intersect(agregar_por, names(dados))
  if (length(validas) == 0) {
    stop("Nenhuma coluna de agregacao encontrada nos dados.")
  }

  # Auto-detect variable
  if (is.null(variavel)) {
    variavel <- .vigiar_detectar_variavel_numerica(dados)
  }
  if (is.null(variavel) || !variavel %in% names(dados)) {
    stop("Nenhuma variavel numerica encontrada para sumarizar.")
  }

  # Ensure grouping columns are the right type
  dados <- dplyr::mutate(dados, dplyr::across(
    dplyr::all_of(intersect(validas, c("ano", "mes"))),
    as.integer
  ))

  # Group and summarise
  result <- dados |>
    dplyr::group_by(dplyr::across(dplyr::all_of(validas))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(variavel),
        funcoes,
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )

  tibble::as_tibble(result)
}

#' Compute descriptive temporal trends
#'
#' Calculates year-over-year changes and simple moving averages.
#' This is purely descriptive -- no model is fitted.
#'
#' @param dados A VIGIAR tibble with \code{ano} and a numeric variable.
#' @param variavel Numeric column to analyse.
#' @param janela_media_movel Window size for moving average (default 3).
#' @return A tibble with columns: ano, media, variacao_anual,
#'   media_movel.
#' @export
vigiar_tendencia_descritiva <- function(dados, variavel = NULL,
                                         janela_media_movel = 3) {
  if (is.null(variavel)) {
    variavel <- .vigiar_detectar_variavel_numerica(dados)
  }
  if (is.null(variavel)) {
    stop("Nenhuma variavel numerica encontrada.")
  }

  anual <- vigiar_agregar_tempo(
    dados,
    agregar_por = "ano",
    variavel    = variavel,
    funcoes     = list(media = function(x) mean(x, na.rm = TRUE))
  )

  col_media <- paste0(variavel, "_media")
  vals <- anual[[col_media]]
  anos <- anual$ano

  # Year-over-year change
  variacao <- c(NA_real_, diff(vals) / head(vals, -1) * 100)

  # Simple moving average
  media_movel <- stats::filter(vals, rep(1 / janela_media_movel, janela_media_movel), sides = 2)
  media_movel <- as.numeric(media_movel)

  tibble::tibble(
    ano            = anos,
    media          = vals,
    variacao_anual = variacao,
    media_movel    = media_movel
  )
}

#' Prepare a VIGIAR tibble for time series exploration
#'
#' Returns a tibble with year and aggregated values, suitable
#' for plotting or further descriptive analysis. No model is fitted.
#'
#' @param dados A processed VIGIAR tibble.
#' @param nivel Aggregation level: \code{"nacional"} (default),
#'   \code{"uf"}, or \code{"municipio"}.
#' @return A tibble with columns: ano, media, n, and spatial
#'   identifier columns if \code{nivel != "nacional"}.
#' @export
vigiar_serie_temporal <- function(dados,
                                   nivel = c("nacional", "uf", "municipio")) {
  nivel <- match.arg(nivel)

  agrupar <- switch(nivel,
    nacional   = "ano",
    uf         = c("ano", "sigla_uf"),
    municipio  = c("ano", "cod_municipio")
  )

  variavel <- .vigiar_detectar_variavel_numerica(dados)
  if (is.null(variavel)) {
    stop("Nenhuma variavel numerica detectada para serie temporal.")
  }

  vigiar_agregar_tempo(
    dados,
    agregar_por = agrupar,
    variavel    = variavel,
    funcoes     = list(
      media = function(x) mean(x, na.rm = TRUE),
      dp    = function(x) stats::sd(x, na.rm = TRUE),
      n     = length,
      min   = function(x) min(x, na.rm = TRUE),
      max   = function(x) max(x, na.rm = TRUE)
    )
  )
}

# -- Helper --------------------------------------------------------------------

.vigiar_detectar_variavel_numerica <- function(dados) {
  candidates <- c(
    "pm25_media_anual", "pm25_media", "pm25_media_periodo",
    "estimativa", "fracao_atribuivel",
    "populacao", "populacao_exposta",
    "perc_combustiveis_solidos", "percentual_combustiveis",
    "n_dias_criticos", "n_dias_criticos_conama"
  )
  found <- intersect(candidates, names(dados))
  if (length(found) == 0) {
    # Pick first numeric column
    for (col in names(dados)) {
      if (is.numeric(dados[[col]])) return(col)
    }
    return(NULL)
  }
  found[1]
}
