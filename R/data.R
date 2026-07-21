# Package: vigiar
# Documentation for built-in package data

#' Sample PM2.5 data for Rio de Janeiro
#'
#' A small synthetic dataset representing PM2.5 annual averages for
#' 5 municipalities in Rio de Janeiro state (2020-2022). Used for
#' examples, tests, and vignettes without requiring an internet
#' connection or Power BI session.
#'
#' @format A [tibble::tibble()] with S3 classes `vigiar_pm25` and `vigiar_tbl`,
#'   15 rows and 7 columns:
#' \describe{
#'   \item{cod_municipio}{IBGE municipality code (6-digit integer)}
#'   \item{ano}{Year of measurement (2020-2022)}
#'   \item{pm25_media_anual}{Annual average PM2.5 concentration (ug/m3)}
#'   \item{sigla_uf}{State abbreviation ("RJ")}
#'   \item{pm25_categoria}{OMS PM2.5 category (factor)}
#'   \item{latitude}{Approximate latitude (decimal degrees)}
#'   \item{longitude}{Approximate longitude (decimal degrees)}
#' }
#'
#' @source Synthetic data generated for package examples.
#'
#' @examples
#' data(pm25_rj_sample)
#' summary(pm25_rj_sample)
#' print(pm25_rj_sample)
"pm25_rj_sample"
