# Package: vigiar
# Data export functions -- CSV, RDS, Parquet
#
# All export functions preserve metadata attributes where possible.

#' Export VIGIAR data to CSV
#'
#' @param dados A processed VIGIAR tibble.
#' @param caminho File path (directory will be created if needed).
#' @param overwrite If \code{FALSE}, errors if file exists.
#' @param ... Additional arguments passed to \code{utils::write.csv()}.
#' @return Invisibly, the file path.
#' @export
vigiar_exportar_csv <- function(dados, caminho, overwrite = FALSE, ...) {
  if (file.exists(caminho) && !overwrite) {
    stop("Arquivo ja existe: ", caminho, ". Use overwrite = TRUE.")
  }
  dir.create(dirname(caminho), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(dados, caminho, row.names = FALSE, fileEncoding = "UTF-8", ...)
  message("Dados exportados: ", caminho)
  invisible(caminho)
}

#' Export VIGIAR data to RDS
#'
#' Preserves all attributes (metadata, classes).
#'
#' @param dados A processed VIGIAR tibble.
#' @param caminho File path.
#' @param overwrite If \code{FALSE}, errors if file exists.
#' @return Invisibly, the file path.
#' @export
vigiar_exportar_rds <- function(dados, caminho, overwrite = FALSE) {
  if (file.exists(caminho) && !overwrite) {
    stop("Arquivo ja existe: ", caminho, ". Use overwrite = TRUE.")
  }
  dir.create(dirname(caminho), showWarnings = FALSE, recursive = TRUE)
  saveRDS(dados, caminho)
  message("Dados exportados: ", caminho)
  invisible(caminho)
}

#' Export VIGIAR data to Parquet
#'
#' Requires the \code{arrow} package (Suggests).
#'
#' @param dados A processed VIGIAR tibble.
#' @param caminho File path.
#' @param overwrite If \code{FALSE}, errors if file exists.
#' @return Invisibly, the file path.
#' @export
vigiar_exportar_parquet <- function(dados, caminho, overwrite = FALSE) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      "O pacote 'arrow' e necessario para exportar Parquet. Instale com:\n",
      "  install.packages(\"arrow\")"
    )
  }
  if (file.exists(caminho) && !overwrite) {
    stop("Arquivo ja existe: ", caminho, ". Use overwrite = TRUE.")
  }
  dir.create(dirname(caminho), showWarnings = FALSE, recursive = TRUE)
  arrow::write_parquet(dados, caminho)
  message("Dados exportados: ", caminho)
  invisible(caminho)
}
