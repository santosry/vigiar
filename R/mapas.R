# Package: vigiar
# Map / spatial exploratory functions
#
# All map functions return ggplot objects.  geobr is an optional
# dependency (Suggests).  If not installed, an informative error
# is raised.

#' Join VIGIAR data with geobr spatial geometries
#'
#' Merges a processed VIGIAR tibble with municipality or state
#' geometries from the geobr package.
#'
#' @param dados A processed VIGIAR tibble with \code{cod_municipio}
#'   or \code{sigla_uf} column.
#' @param nivel Either \code{"municipio"} or \code{"uf"}.
#' @param ano Year of the geobr geometries (default 2020).
#' @return An \code{sf} data frame with VIGIAR data joined to geometries.
#' @export
vigiar_join_geobr <- function(dados, nivel = c("municipio", "uf"),
                                ano = 2020) {
  nivel <- match.arg(nivel)

  if (!requireNamespace("geobr", quietly = TRUE)) {
    stop(
      "O pacote 'geobr' é necessário para mapas. Instale com:\n",
      "  install.packages(\"geobr\")"
    )
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "O pacote 'sf' é necessário para mapas. Instale com:\n",
      "  install.packages(\"sf\")"
    )
  }

  if (nivel == "municipio") {
    geo <- geobr::read_municipality(year = ano, simplified = TRUE, showProgress = FALSE)
    key_col <- "code_muni"
    data_col <- "cod_municipio"
  } else {
    geo <- geobr::read_state(year = ano, simplified = TRUE, showProgress = FALSE)
    key_col <- "abbrev_state"
    data_col <- "sigla_uf"
  }

  # Ensure matching types
  if (data_col %in% names(dados)) {
    if (is.numeric(dados[[data_col]]) && is.character(geo[[key_col]])) {
      dados[[data_col]] <- as.character(dados[[data_col]])
    }
  }

  merged <- dplyr::left_join(geo, dados, by = dplyr::join_by(!!key_col == !!data_col))
  sf::st_as_sf(merged)
}

#' Map PM2.5 concentrations
#'
#' @param dados A processed \code{vigiar_pm25} tibble or joined sf object.
#' @param nivel \code{"municipio"} or \code{"uf"}.
#' @param ano Year to map. \code{NULL} = latest available.
#' @param variavel Column name to map (default auto-detected).
#' @param titulo Optional plot title.
#' @return A \code{ggplot} object.
#' @export
vigiar_mapa_pm25 <- function(dados, nivel = c("municipio", "uf"),
                               ano = NULL, variavel = NULL,
                               titulo = NULL) {
  nivel <- match.arg(nivel)

  if (!inherits(dados, "sf")) {
    dados <- vigiar_join_geobr(dados, nivel = nivel)
  }

  variavel <- variavel %||%
    intersect(c("pm25_media_anual", "pm25_media"),
              names(dados))[1] %||%
    stop("Variável de PM2.5 não encontrada nos dados.")

  if (!is.null(ano) && "ano" %in% names(dados)) {
    dados <- dplyr::filter(dados, ano == !!ano)
  }

  titulo <- titulo %||% sprintf(
    "PM2.5 — %s (%s)",
    if (nivel == "municipio") "Municípios" else "Unidades Federativas",
    if (!is.null(ano)) ano else "todos os anos"
  )

  ggplot2::ggplot(dados) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[variavel]]),
                     colour = NA, size = 0.05) +
    ggplot2::scale_fill_viridis_c(
      name = expression(PM[2.5] ~ (µg/m³)),
      option = "plasma", na.value = "grey90"
    ) +
    ggplot2::labs(title = titulo) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text       = ggplot2::element_blank(),
      axis.ticks      = ggplot2::element_blank(),
      panel.grid      = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

#' @rdname vigiar_mapa_pm25
#' @export
vigiar_mapa_populacao_exposta <- function(dados, nivel = c("municipio", "uf"),
                                            ano = NULL, variavel = "populacao",
                                            titulo = NULL) {
  nivel <- match.arg(nivel)
  if (!inherits(dados, "sf")) dados <- vigiar_join_geobr(dados, nivel = nivel)
  if (!is.null(ano) && "ano" %in% names(dados)) dados <- dplyr::filter(dados, ano == !!ano)

  titulo <- titulo %||% "População Exposta por Concentração de PM2.5"

  ggplot2::ggplot(dados) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[variavel]]),
                     colour = NA, size = 0.05) +
    ggplot2::scale_fill_viridis_c(option = "rocket", na.value = "grey90") +
    ggplot2::labs(title = titulo, fill = "População") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())
}

#' @rdname vigiar_mapa_pm25
#' @export
vigiar_mapa_indicadores_saude <- function(dados, nivel = c("uf", "municipio"),
                                            ano = NULL,
                                            variavel = NULL,
                                            titulo = NULL) {
  nivel <- match.arg(nivel)
  if (!inherits(dados, "sf")) dados <- vigiar_join_geobr(dados, nivel = nivel)
  if (!is.null(ano) && "ano" %in% names(dados)) dados <- dplyr::filter(dados, ano == !!ano)

  variavel <- variavel %||%
    intersect("estimativa", names(dados))[1] %||%
    stop("Variável de estimativa não encontrada.")

  titulo <- titulo %||% "Indicadores de Saúde — Estimativas"

  ggplot2::ggplot(dados) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[variavel]]),
                     colour = NA, size = 0.05) +
    ggplot2::scale_fill_viridis_c(option = "mako", na.value = "grey90") +
    ggplot2::labs(title = titulo) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())
}

#' @rdname vigiar_mapa_pm25
#' @export
vigiar_mapa_fracao_atribuivel <- function(dados, nivel = c("uf", "municipio"),
                                            ano = NULL,
                                            variavel = "fracao_atribuivel",
                                            titulo = NULL) {
  nivel <- match.arg(nivel)
  if (!inherits(dados, "sf")) dados <- vigiar_join_geobr(dados, nivel = nivel)
  if (!is.null(ano) && "ano" %in% names(dados)) dados <- dplyr::filter(dados, ano == !!ano)

  titulo <- titulo %||% "Fração Atribuível (%)"

  ggplot2::ggplot(dados) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[variavel]]),
                     colour = NA, size = 0.05) +
    ggplot2::scale_fill_viridis_c(option = "inferno", na.value = "grey90") +
    ggplot2::labs(title = titulo, fill = "%") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())
}

#' @rdname vigiar_mapa_pm25
#' @export
vigiar_mapa_indoor <- function(dados, nivel = c("uf", "municipio"),
                                 ano = NULL,
                                 variavel = NULL,
                                 titulo = NULL) {
  nivel <- match.arg(nivel)
  if (!inherits(dados, "sf")) dados <- vigiar_join_geobr(dados, nivel = nivel)
  if (!is.null(ano) && "ano" %in% names(dados)) dados <- dplyr::filter(dados, ano == !!ano)

  variavel <- variavel %||%
    intersect(c("perc_combustiveis_solidos", "percentual_combustiveis"),
              names(dados))[1] %||%
    stop("Variável de exposição indoor não encontrada.")

  titulo <- titulo %||% "Exposição a Combustíveis Sólidos (%)"

  ggplot2::ggplot(dados) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[variavel]]),
                     colour = NA, size = 0.05) +
    ggplot2::scale_fill_viridis_c(option = "cividis", na.value = "grey90") +
    ggplot2::labs(title = titulo, fill = "%") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())
}
