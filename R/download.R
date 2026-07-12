# Package: vigiar
# User-facing download and inspection functions

#' List available tables
#'
#' @return Character vector of table names.
#' @export
vigiar_tabelas <- function() {
  if (is.null(.vigiar_env$esquema)) {
    stop("No active session. Run vigiar_conectar() first.")
  }
  names(.vigiar_env$esquema)
}

#' Display table schema
#'
#' Shows column names and R types for one or all tables.
#'
#' @param tabela Table name (optional). If `NULL`, lists all tables.
#' @return Invisibly, the schema list.
#' @export
vigiar_esquema <- function(tabela = NULL) {
  if (is.null(.vigiar_env$esquema)) {
    stop("No active session. Run vigiar_conectar() first.")
  }

  if (!is.null(tabela)) {
    .vigiar_check_tabela(tabela)
    cat(sprintf("\n=== Table: %s ===\n", tabela))
    col_info <- .vigiar_env$esquema[[tabela]]
    df <- data.frame(
      coluna = names(col_info),
      tipo   = vapply(col_info, `[[`, "", "tipo", USE.NAMES = FALSE),
      stringsAsFactors = FALSE
    )
    print(df, row.names = FALSE)
    return(invisible(col_info))
  }

  for (tab in names(.vigiar_env$esquema)) {
    n <- length(.vigiar_env$esquema[[tab]])
    cat(sprintf("%-42s %3d columns\n", tab, n))
  }
  invisible(.vigiar_env$esquema)
}

#' Download data from a single table
#'
#' @param tabela Table name (use `vigiar_tabelas()` to list).
#' @param colunas Optional character vector of column names. `NULL` = all.
#' @param ordenar_por Column to sort by (optional).
#' @param limite Maximum number of rows (optional).
#' @param timeout Timeout in seconds for the HTTP request.
#' @param uf Optional UF filter applied client-side. The generic downloader does
#'   not restrict geography by default; use \code{vigiar_baixar_rj()} for an
#'   explicitly audited Rio de Janeiro download.
#' @param direcao Sort direction: `"asc"` (ascending) or `"desc"` (descending).
#' @param filtros Optional named list of server-side equality filters. This is
#'   primarily used internally for audited RJ downloads.
#' @return A [tibble::tibble()] with the downloaded data.
#' @export
vigiar_baixar <- function(tabela, colunas = NULL, ordenar_por = NULL,
                           limite = NULL, timeout = 120, uf = NULL,
                           direcao = c("asc", "desc"), filtros = NULL) {
  if (is.null(.vigiar_env$sessao)) {
    stop("No active session. Run vigiar_conectar() first.")
  }
  .vigiar_check_tabela(tabela)

  t_start <- Sys.time()
  .vigiar_log(
    "INFO", sprintf("Starting download: %s", tabela), table = tabela,
    metadata = list(
      columns = colunas,
      order_by = ordenar_por,
      requested_limit = limite,
      uf = uf,
      filters = filtros
    ),
    event = "download_start"
  )
  cli::cli_alert_info("Downloading table '{tabela}'...")

  query <- .vigiar_construir_query(
    tabela      = tabela,
    colunas     = colunas,
    ordenar_por = ordenar_por,
    limite      = limite,
    direcao     = if (direcao[1] == "desc") 2L else 1L,
    filtros     = filtros,
    modelo_id   = .vigiar_env$sessao$model_id
  )

  dados <- tryCatch({
    resposta <- .vigiar_executar_query(
      .vigiar_env$sessao, query, timeout = timeout
    )
    .vigiar_parse_dados(resposta, tabela)
  }, error = function(e) {
    .vigiar_log(
      "ERROR", paste("Download failed:", conditionMessage(e)),
      table = tabela,
      metadata = list(requested_limit = limite, filters = filtros),
      event = "download_failure"
    )
    stop(e)
  })
  response_rows <- nrow(dados)
  dados <- .vigiar_detectar_truncamento(dados, tabela = tabela, limite = limite)
  response_attribute_names <- c(
    "vigiar_truncation_status",
    "vigiar_truncation_evidence",
    "vigiar_truncation_assessment",
    "vigiar_possivel_truncamento",
    "vigiar_response_metadata",
    "vigiar_parser_status",
    "vigiar_parser_issues"
  )
  response_attributes <- lapply(
    response_attribute_names,
    function(name) attr(dados, name)
  )
  names(response_attributes) <- response_attribute_names

  uf_normalized <- if (is.null(uf)) NULL else .vigiar_normalizar_uf(uf)
  if (!is.null(uf_normalized) &&
      (length(uf_normalized) != 1L || is.na(uf_normalized))) {
    stop("'uf' must be one valid Brazilian UF code or abbreviation.", call. = FALSE)
  }
  requested_scope <- if (is.null(uf)) "all_returned_geographies" else
    paste0("uf:", uf_normalized)

  # Client-side UF filter
  if (!is.null(uf)) {
    n_antes <- nrow(dados)
    dados <- .vigiar_filtrar_uf(dados, uf)
    cli::cli_alert_info(
      "UF filter '{uf_normalized}': {nrow(dados)} rows from {n_antes}."
    )
    for (name in names(response_attributes)) {
      attr(dados, name) <- response_attributes[[name]]
    }
  }

  elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))

  .vigiar_registrar_download(
    tabela  = tabela,
    n_rows  = nrow(dados),
    n_cols  = ncol(dados),
    elapsed = elapsed,
    url     = .vigiar_env$sessao$api_url
  )

  cli::cli_alert_success(
    "Table '{tabela}' downloaded: {nrow(dados)} rows x {ncol(dados)} columns ({round(elapsed, 1)}s)"
  )

  attr(dados, "vigiar_tabela") <- tabela
  attr(dados, "vigiar_requested_scope") <- requested_scope
  attr(dados, "vigiar_response_rows") <- response_rows
  attr(dados, "vigiar_returned_rows") <- nrow(dados)
  attr(dados, "vigiar_server_filter") <- filtros
  attr(dados, "vigiar_requested_limit") <- limite
  attr(dados, "vigiar_query_strategy") <- "single_semantic_query"
  truncation_status <- attr(dados, "vigiar_truncation_status") %||% "unknown"
  attr(dados, "vigiar_verification_status") <- if (truncation_status == "no_evidence") {
    "unverified"
  } else {
    paste0("truncation_", truncation_status)
  }
  attr(dados, "vigiar_download_timestamp") <- Sys.time()
  out <- tibble::as_tibble(dados)
  for (name in response_attribute_names) {
    attr(out, name) <- attr(dados, name)
  }
  out
}

#' Download multiple tables
#'
#' @param tabelas Character vector of table names. `NULL` = all.
#' @param progress Show progress messages.
#' @param delay Seconds to wait between downloads (rate limiting). Default 0.5.
#' @return Named list of tibbles.
#' @export
vigiar_baixar_tudo <- function(tabelas = NULL, progress = TRUE, delay = 0.5) {
  if (is.null(.vigiar_env$sessao)) {
    stop("No active session. Run vigiar_conectar() first.")
  }

  if (is.null(tabelas)) {
    tabelas <- names(.vigiar_env$esquema)
  } else {
    invalidas <- setdiff(tabelas, names(.vigiar_env$esquema))
    if (length(invalidas) > 0) {
      warning(
        "Tables not found: ",
        paste(invalidas, collapse = ", ")
      )
      tabelas <- intersect(tabelas, names(.vigiar_env$esquema))
    }
  }

  resultado <- vector("list", length(tabelas))
  names(resultado) <- tabelas

  for (i in seq_along(tabelas)) {
    tab <- tabelas[[i]]
    if (progress) {
      message(sprintf("[%d/%d] Downloading '%s'...", i, length(tabelas), tab))
    }
    resultado[[tab]] <- tryCatch(
      vigiar_baixar(tab),
      error = function(e) {
        warning(sprintf("Failed to download '%s': %s", tab, e$message))
        NULL
      }
    )
    if (delay > 0 && i < length(tabelas)) Sys.sleep(delay)
  }

  n_ok <- sum(!vapply(resultado, is.null, logical(1)))
  message(sprintf(
      "Download completed: %d/%d tables downloaded successfully.",
    n_ok, length(tabelas)
  ))

  resultado
}

#' Download main tables (convenience shortcut)
#'
#' Downloads 14 key tables covering all data categories.
#'
#' @return Named list of tibbles.
#' @export
vigiar_baixar_principais <- function() {
  principais <- c(
    "df_anual", "df_mensal", "df_muni", "pop",
    "tb_brasil", "tb_uf", "tb_muni",
    "df_indoor", "df_indoor_desfecho",
    "df_dias", "df_dias_conama",
    "tb_fracao", "tb_quartis", "medidas"
  )
  disponiveis <- intersect(principais, names(.vigiar_env$esquema))
  vigiar_baixar_tudo(disponiveis, progress = TRUE)
}

#' Table catalogue with descriptions
#'
#' Returns a tibble with all tables, column counts, descriptions,
#' and thematic categories.
#'
#' @return A tibble with columns: `tabela`, `colunas`, `descricao`, `categoria`.
#' @export
vigiar_info <- function() {
  if (is.null(.vigiar_env$esquema)) {
    stop("No active session. Run vigiar_conectar() first.")
  }

  catalogo <- .vigiar_catalogo()
  tabelas  <- names(.vigiar_env$esquema)
  n_cols   <- vapply(.vigiar_env$esquema, length, integer(1))

  result <- data.frame(
    tabela  = tabelas,
    colunas = n_cols,
    stringsAsFactors = FALSE
  )

  idx <- match(tabelas, catalogo$tabela)
  result$descricao <- catalogo$descricao[idx]
  result$categoria <- catalogo$categoria[idx]

  result$descricao[is.na(result$descricao)] <- "Auxiliary dashboard table"
  result$categoria[is.na(result$categoria)] <- "Auxiliary"

  tibble::as_tibble(result)[
    order(result$categoria, result$tabela),
  ]
}

#' Validate downloaded data
#'
#' Performs basic sanity checks on a downloaded table:
#' reports missing values, duplicate rows, and type consistency.
#'
#' @param dados A data frame (or tibble) returned by `vigiar_baixar()`.
#' @param tabela Table name (for messages).
#' @return Invisibly, a list of diagnostics.
#' @export
vigiar_checar_dados <- function(dados, tabela = NULL) {
  checks <- list()

  checks$n_rows <- nrow(dados)
  checks$n_cols <- ncol(dados)
  checks$col_names <- names(dados)

  # Missing values
  na_count <- vapply(dados, function(x) sum(is.na(x)), integer(1))
  checks$na_per_column <- na_count

  # Duplicate rows
  checks$duplicated_rows <- sum(duplicated(dados))

  # Empty
  checks$is_empty <- nrow(dados) == 0

  if (!is.null(tabela)) {
    cat(sprintf("\nDiagnostic: %s\n", tabela))
    cat(strrep("-", 40), "\n")
  }
  cat(sprintf("Rows:    %d\n", checks$n_rows))
  cat(sprintf("Columns: %d\n", checks$n_cols))
  cat(sprintf("Duplicate rows: %d\n", checks$duplicated_rows))

  if (any(na_count > 0)) {
    cat("\nMissing values by column:\n")
    na_info <- na_count[na_count > 0]
    for (nm in names(na_info)) {
      cat(sprintf("  %-30s %d (%.1f%%)\n",
                  nm, na_info[[nm]],
                  100 * na_info[[nm]] / checks$n_rows))
    }
  } else {
    cat("Missing values: 0\n")
  }

  invisible(checks)
}

#' Diagnostic summary of all downloaded tables
#'
#' Downloads a small sample from every table and reports basic
#' diagnostics to detect schema changes or data issues.
#'
#' @param amostra Number of rows to sample per table.
#' @return Invisibly, a list of diagnostics per table.
#' @export
vigiar_diagnostico <- function(amostra = 100) {
  if (is.null(.vigiar_env$sessao)) {
    stop("No active session. Run vigiar_conectar() first.")
  }

  tabelas <- names(.vigiar_env$esquema)
  resultados <- vector("list", length(tabelas))
  names(resultados) <- tabelas

  for (tab in tabelas) {
    message(sprintf("Sampling '%s' (%d rows)...", tab, amostra))
    resultados[[tab]] <- tryCatch({
      dados <- vigiar_baixar(tab, limite = amostra)
      vigiar_checar_dados(dados, tabela = tab)
    }, error = function(e) {
      warning(sprintf("Failure in '%s': %s", tab, e$message))
      list(error = e$message)
    })
  }

  invisible(resultados)
}

# -- Internal helpers ----------------------------------------------------------

.vigiar_check_tabela <- function(tabela) {
  if (!tabela %in% names(.vigiar_env$esquema)) {
    stop(
      sprintf("Table '%s' was not found.", tabela),
      " Use vigiar_tabelas() to list available tables."
    )
  }
}

.vigiar_catalogo <- function() {
  data.frame(
    tabela = c(
      "df_anual", "df_mensal", "df_dias", "df_dias_conama",
      "pop", "df_muni", "df_mes", "df_ano",
      "tb_brasil", "tb_uf", "tb_muni", "tb_fracao", "tb_quartis",
      "df_indoor", "df_indoor_desfecho",
      "medidas",
      "legenda", "legenda_conama", "legenda_quartis", "legenda_indoor",
      "Ano", "Selecao", "referencia", "referencia_conama",
      "seletor_indicador",
      "aux_uf", "dados_ate", "last_update", "att_em"
    ),
    descricao = c(
      "Annual municipality PM2.5 means",
      "Monthly municipality PM2.5 means with latitude and longitude",
      "Days above the WHO threshold (PM2.5 > 15 ug/m3)",
      "Days above the CONAMA threshold (PM2.5 > 50 ug/m3)",
      "Resident population by municipality, year, and exposure category",
      "Municipality registry: region, UF, coordinates, names",
      "Auxiliary table: month number to name",
      "Years available in the source",
      "Aggregated health indicators -- Brazil",
      "Aggregated health indicators -- UF",
      "Municipality health indicators (IBGE code, latitude, longitude)",
      "Attributable fraction by indicator and outcome",
      "Indicator quartiles (q1, q2, q3)",
      "Household exposure to solid fuels",
      "Health outcomes associated with indoor pollution",
      "Calculated measures: rankings, means, alerts, proportions (61 columns)",
      "PM2.5 color legend (WHO)",
      "PM2.5 color legend (CONAMA)",
      "Quartile color legend",
      "Indoor-exposure color legend",
      "Year selector (dashboard filter)",
      "Category selector (dashboard filter)",
      "WHO reference values",
      "CONAMA reference values",
      "Health indicator selector",
      "UF code to name mapping",
      "Date of the latest available data",
      "Latest database update",
      "Update timestamp"
    ),
    categoria = c(
      "Air quality", "Air quality", "Air quality", "Air quality",
      "Population", "Registry", "Auxiliary", "Auxiliary",
      "Health indicators", "Health indicators", "Health indicators",
      "Health indicators", "Health indicators",
      "Indoor exposure", "Indoor exposure",
      "Measures",
      "Auxiliary", "Auxiliary", "Auxiliary", "Auxiliary",
      "Filters", "Filters", "Filters", "Filters", "Filters",
      "Auxiliary", "Metadata", "Metadata", "Metadata"
    ),
    stringsAsFactors = FALSE
  )
}
