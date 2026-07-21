# Package: vigiar
# Structured logging and provenance tracking
#
# Every download, every operation is logged with timestamps,
# enabling full audit trails and reproducibility.
# Inspired by microdatasus download logging patterns.

#' Structured log entry
#'
#' @param level Log level: \code{"INFO"}, \code{"WARN"}, \code{"ERROR"}, \code{"DEBUG"}.
#' @param message Log message.
#' @param table Table name (optional).
#' @param metadata Named list of additional metadata.
#' @return Invisibly, the log entry (as a list).
#' @keywords internal
.vigiar_log <- function(level, message, table = NULL, metadata = NULL) {
  entry <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"),
    level     = level,
    message   = message,
    table     = table %||% NA_character_,
    metadata  = metadata %||% list()
  )

  # Store in package env
  if (is.null(.vigiar_env$log)) {
    .vigiar_env$log <- list()
  }
  .vigiar_env$log[[length(.vigiar_env$log) + 1L]] <- entry

  # Also print to console if DEBUG or ERROR
  if (level %in% c("ERROR", "DEBUG")) {
    prefix <- switch(level,
      ERROR = cli::col_red("[ERROR]"),
      DEBUG = cli::col_cyan("[DEBUG]"),
      level
    )
    cli::cli_text("{prefix} {message}")
  }

  invisible(entry)
}

#' Retrieve the complete download log
#'
#' Returns a tibble with all logged operations since package load
#' or last \code{vigiar_limpar_log()}.
#'
#' @return A tibble with columns: timestamp, level, message,
#'   table, metadata_json.
#' @export
vigiar_log <- function() {
  if (is.null(.vigiar_env$log) || length(.vigiar_env$log) == 0) {
    cli::cli_alert_info("Log vazio. Execute uma operacao primeiro.")
    return(tibble::tibble(
      timestamp = character(0),
      level     = character(0),
      message   = character(0),
      table     = character(0),
      metadata  = character(0)
    ))
  }

  entries <- .vigiar_env$log
  n <- length(entries)

  df <- tibble::tibble(
    timestamp = vapply(entries, `[[`, "", "timestamp", USE.NAMES = FALSE),
    level     = vapply(entries, `[[`, "", "level", USE.NAMES = FALSE),
    message   = vapply(entries, `[[`, "", "message", USE.NAMES = FALSE),
    table     = vapply(entries, function(e) {
      t <- e$table %||% NA_character_
      if (length(t) == 0) NA_character_ else t
    }, "", USE.NAMES = FALSE),
    metadata_json = vapply(entries, function(e) {
      if (length(e$metadata) == 0) return("{}")
      jsonlite::toJSON(e$metadata, auto_unbox = TRUE, null = "null")
    }, "", USE.NAMES = FALSE)
  )

  df
}

#' Clear the operation log
#'
#' @export
vigiar_limpar_log <- function() {
  n <- length(.vigiar_env$log %||% list())
  .vigiar_env$log <- list()
  cli::cli_alert_info("Log limpo ({n} entradas removidas).")
  invisible(NULL)
}

#' Export the operation log to a file
#'
#' @param caminho File path (JSON or CSV based on extension).
#' @param overwrite If \code{FALSE}, errors if file exists.
#' @return Invisibly, the file path.
#' @export
vigiar_exportar_log <- function(caminho, overwrite = FALSE) {
  if (file.exists(caminho) && !overwrite) {
    stop("Arquivo ja existe: ", caminho, ". Use overwrite = TRUE.")
  }

  log_df <- vigiar_log()
  dir.create(dirname(caminho), showWarnings = FALSE, recursive = TRUE)

  if (grepl("\\.json$", caminho, ignore.case = TRUE)) {
    jsonlite::write_json(log_df, caminho, pretty = TRUE, auto_unbox = TRUE)
  } else {
    utils::write.csv(log_df, caminho, row.names = FALSE, fileEncoding = "UTF-8")
  }

  cli::cli_alert_success("Log exportado: {caminho} ({nrow(log_df)} entradas)")
  invisible(caminho)
}

#' Summarise the operation log
#'
#' Shows counts by log level, table, and time range.
#'
#' @return Invisibly, a list with log statistics.
#' @export
vigiar_resumo_log <- function() {
  log_df <- vigiar_log()

  if (nrow(log_df) == 0) {
    cli::cli_alert_info("Log vazio.")
    return(invisible(list()))
  }

  cli::cli_h1("Resumo do Log")
  cli::cli_text("Periodo: {min(log_df$timestamp)} a {max(log_df$timestamp)}")
  cli::cli_text("Total de entradas: {nrow(log_df)}")

  # By level
  cli::cli_h2("Por nivel")
  level_counts <- table(log_df$level)
  for (lev in names(level_counts)) {
    color <- switch(lev,
      ERROR = cli::col_red,
      WARN  = cli::col_yellow,
      INFO  = cli::col_green,
      DEBUG = cli::col_cyan,
      identity
    )
    cli::cli_text("{color(lev)}: {level_counts[lev]}")
  }

  # By table
  cli::cli_h2("Por tabela")
  table_counts <- sort(table(log_df$table[!is.na(log_df$table)]), decreasing = TRUE)
  if (length(table_counts) > 0) {
    for (i in seq_len(min(length(table_counts), 10))) {
      cli::cli_text("  {names(table_counts)[i]}: {table_counts[i]}")
    }
  }

  invisible(list(
    n_entries = nrow(log_df),
    level_counts = as.list(level_counts),
    table_counts = as.list(table_counts)
  ))
}

# -- Download history ----------------------------------------------------------

#' Download history entry
#' @keywords internal
.vigiar_registrar_download <- function(tabela, n_rows, n_cols, elapsed, url) {
  entry <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"),
    tabela    = tabela,
    n_rows    = n_rows,
    n_cols    = n_cols,
    elapsed   = elapsed,
    url       = url,
    checksum  = NULL  # filled later if data is available
  )

  if (is.null(.vigiar_env$download_history)) {
    .vigiar_env$download_history <- list()
  }
  .vigiar_env$download_history[[length(.vigiar_env$download_history) + 1L]] <- entry

  .vigiar_log("INFO", sprintf("Download: %s (%d linhas x %d colunas, %.1fs)",
                               tabela, n_rows, n_cols, elapsed),
               table = tabela,
               metadata = list(n_rows = n_rows, n_cols = n_cols, elapsed = elapsed))

  invisible(entry)
}

#' Show download history
#'
#' @return A tibble with download history since package load.
#' @export
vigiar_historico_downloads <- function() {
  if (is.null(.vigiar_env$download_history) ||
      length(.vigiar_env$download_history) == 0) {
    cli::cli_alert_info("Nenhum download registrado nesta sessao.")
    return(tibble::tibble(
      timestamp = character(0),
      tabela    = character(0),
      n_rows    = integer(0),
      n_cols    = integer(0),
      elapsed   = numeric(0)
    ))
  }

  entries <- .vigiar_env$download_history
  df <- tibble::tibble(
    timestamp = vapply(entries, `[[`, "", "timestamp", USE.NAMES = FALSE),
    tabela    = vapply(entries, `[[`, "", "tabela", USE.NAMES = FALSE),
    n_rows    = vapply(entries, `[[`, 0L, "n_rows", USE.NAMES = FALSE),
    n_cols    = vapply(entries, `[[`, 0L, "n_cols", USE.NAMES = FALSE),
    elapsed   = vapply(entries, `[[`, 0.0, "elapsed", USE.NAMES = FALSE)
  )

  df
}

#' Summary of download history
#'
#' @export
vigiar_resumo_downloads <- function() {
  hist <- vigiar_historico_downloads()
  if (nrow(hist) == 0) return(invisible())

  cli::cli_h1("Resumo de Downloads")
  cli::cli_text("Total de downloads: {nrow(hist)}")
  cli::cli_text("Tempo total: {round(sum(hist$elapsed, na.rm=TRUE), 1)}s")
  cli::cli_text("Total de linhas: {sum(hist$n_rows, na.rm=TRUE)}")

  cli::cli_h2("Por tabela")
  by_table <- dplyr::count(hist, tabela, sort = TRUE)
  for (i in seq_len(min(nrow(by_table), 10))) {
    cli::cli_text("  {by_table$tabela[i]}: {by_table$n[i]} downloads")
  }

  invisible(hist)
}
