# Package: vigiar
# Summary / exploratory statistics (descriptive only, no inference)
#
# Each vigiar_resumo_*() returns a tibble with descriptive statistics
# appropriate for the data domain.

#' Summarise a VIGIAR tibble -- generic dispatcher
#'
#' @param x A processed VIGIAR tibble.
#' @param ... Additional arguments passed to specific summarisers.
#' @return A tibble with summary statistics.
#' @export
vigiar_resumo <- function(x, ...) {
  stopifnot(inherits(x, "vigiar_tbl"))
  UseMethod("vigiar_resumo")
}

#' @export
vigiar_resumo.vigiar_pm25 <- function(x, ...) {
  vigiar_resumo_pm25(x, ...)
}

#' @export
vigiar_resumo.vigiar_health <- function(x, ...) {
  vigiar_resumo_saude(x, ...)
}

#' @export
vigiar_resumo.vigiar_population <- function(x, ...) {
  vigiar_resumo_populacao(x, ...)
}

#' @export
vigiar_resumo.vigiar_attributable_fraction <- function(x, ...) {
  vigiar_resumo_fracao_atribuivel(x, ...)
}

#' @export
vigiar_resumo.vigiar_indoor <- function(x, ...) {
  vigiar_resumo_indoor(x, ...)
}

#' @export
vigiar_resumo.default <- function(x, ...) {
  .vigiar_resumo_generico(x, ...)
}

# -- PM2.5 summary -------------------------------------------------------------

#' Summarise PM2.5 data
#'
#' @param x A \code{vigiar_pm25} tibble.
#' @param ... Ignored.
#' @return A tibble with descriptive statistics.
#' @export
vigiar_resumo_pm25 <- function(x, ...) {
  pm25_col <- intersect(
    c("pm25_media_anual", "pm25_media", "pm25_media_periodo"),
    names(x)
  )
  if (length(pm25_col) == 0) pm25_col <- character(0)

  res <- .vigiar_resumo_generico(x)

  if (length(pm25_col) == 1) {
    vals <- x[[pm25_col]]
    res$media     <- mean(vals, na.rm = TRUE)
    res$mediana   <- median(vals, na.rm = TRUE)
    res$min       <- min(vals, na.rm = TRUE)
    res$max       <- max(vals, na.rm = TRUE)
    res$desvio_padrao <- sd(vals, na.rm = TRUE)
    res$p25       <- quantile(vals, 0.25, na.rm = TRUE)
    res$p75       <- quantile(vals, 0.75, na.rm = TRUE)
    res$n_fora_faixa <- sum(!is.na(vals) & (vals < 0 | vals > 1000))
  }

  res$tabela_original <- attr(x, "vigiar_tabela") %||% NA_character_
  res
}

# -- Health indicators summary -------------------------------------------------

#' @rdname vigiar_resumo_pm25
#' @export
vigiar_resumo_saude <- function(x, ...) {
  res <- .vigiar_resumo_generico(x)

  if ("estimativa" %in% names(x)) {
    vals <- x$estimativa
    res$media     <- mean(vals, na.rm = TRUE)
    res$mediana   <- median(vals, na.rm = TRUE)
    res$min       <- min(vals, na.rm = TRUE)
    res$max       <- max(vals, na.rm = TRUE)
  }

  if ("indicador" %in% names(x)) {
    res$n_indicadores <- dplyr::n_distinct(x$indicador)
  }
  if ("desfecho" %in% names(x)) {
    res$n_desfechos <- dplyr::n_distinct(x$desfecho)
  }

  res$tabela_original <- attr(x, "vigiar_tabela") %||% NA_character_
  res
}

#' @rdname vigiar_resumo_pm25
#' @export
vigiar_resumo_populacao <- function(x, ...) {
  res <- .vigiar_resumo_generico(x)
  if ("populacao" %in% names(x)) {
    res$pop_total <- sum(x$populacao, na.rm = TRUE)
  }
  res$tabela_original <- attr(x, "vigiar_tabela") %||% NA_character_
  res
}

#' @rdname vigiar_resumo_pm25
#' @export
vigiar_resumo_fracao_atribuivel <- function(x, ...) {
  res <- .vigiar_resumo_generico(x)
  if ("fracao_atribuivel" %in% names(x)) {
    vals <- x$fracao_atribuivel
    res$media  <- mean(vals, na.rm = TRUE)
    res$min    <- min(vals, na.rm = TRUE)
    res$max    <- max(vals, na.rm = TRUE)
  }
  res$tabela_original <- attr(x, "vigiar_tabela") %||% NA_character_
  res
}

#' @rdname vigiar_resumo_pm25
#' @export
vigiar_resumo_indoor <- function(x, ...) {
  res <- .vigiar_resumo_generico(x)
  if ("perc_combustiveis_solidos" %in% names(x)) {
    vals <- x$perc_combustiveis_solidos
    res$media  <- mean(vals, na.rm = TRUE)
    res$min    <- min(vals, na.rm = TRUE)
    res$max    <- max(vals, na.rm = TRUE)
  }
  res$tabela_original <- attr(x, "vigiar_tabela") %||% NA_character_
  res
}

# -- Generic summary -----------------------------------------------------------

.vigiar_resumo_generico <- function(x) {
  n_rows <- nrow(x)
  n_cols <- ncol(x)
  n_na   <- sum(is.na(x))
  n_dup  <- sum(duplicated(x))

  # Temporal range
  ano_min <- if ("ano" %in% names(x)) min(x$ano, na.rm = TRUE) else NA_integer_
  ano_max <- if ("ano" %in% names(x)) max(x$ano, na.rm = TRUE) else NA_integer_

  # Spatial
  n_uf <- if ("sigla_uf" %in% names(x)) dplyr::n_distinct(x$sigla_uf, na.rm = TRUE) else NA_integer_
  n_mun <- if ("cod_municipio" %in% names(x)) dplyr::n_distinct(x$cod_municipio, na.rm = TRUE) else NA_integer_

  tibble::tibble(
    n_observacoes = n_rows,
    n_colunas     = n_cols,
    n_ausentes    = n_na,
    n_duplicatas  = n_dup,
    ano_min       = ano_min,
    ano_max       = ano_max,
    n_uf          = n_uf,
    n_municipios  = n_mun
  )
}
