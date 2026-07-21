# Package: vigiar
# Performance benchmarks
#
# Measures download and processing performance across strategies,
# enabling users to choose optimal approaches for their use case.
# Analogous to microdatasus::microdatasus_benchmark() pattern.

#' Benchmark VIGIAR download strategies
#'
#' Tests different download strategies (direct, year-partitioned,
#' column-restricted) on a given table and reports timing,
#' row counts, and data quality metrics.
#'
#' @param tabela Table name to benchmark.
#' @param strategies Character vector of strategies to test.
#'   Options: \code{"direct"}, \code{"year_asc_desc"},
#'   \code{"minimal_columns"}, \code{"all"}.
#' @param repeticoes Number of repetitions per strategy.
#' @param timeout Timeout per download in seconds.
#' @return A tibble with columns: estrategia, tempo_medio,
#'   tempo_min, tempo_max, n_linhas, n_colunas, n_ausentes,
#'   taxa_sucesso.
#' @export
vigiar_benchmark <- function(tabela,
                              strategies = c("direct", "year_asc_desc",
                                             "minimal_columns"),
                              repeticoes = 3L,
                              timeout = 120) {
  if (is.null(.vigiar_env$esquema)) {
    stop("Nenhuma sessao ativa. Execute vigiar_conectar() primeiro.")
  }
  .vigiar_check_tabela(tabela)

  cli::cli_h1("VIGIAR Benchmark")
  cli::cli_text("Tabela: {.strong {tabela}}")
  cli::cli_text("Repeticoes por estrategia: {repeticoes}")
  cli::cli_text("Sessao criada em: {format(.vigiar_env$sessao$created_at)}")
  cli::cli_rule()

  all_strategies <- c("direct", "year_asc_desc", "minimal_columns")
  if ("all" %in% strategies) strategies <- all_strategies
  strategies <- match.arg(strategies, all_strategies, several.ok = TRUE)

  results <- vector("list", length(strategies) * repeticoes)
  row_idx <- 1L

  # Minimal columns for this table
  col_min <- .vigiar_benchmark_minimal_columns(tabela)

  for (strategy in strategies) {
    cli::cli_h2("Estrategia: {strategy}")
    times <- numeric(repeticoes)
    rows <- integer(repeticoes)
    cols <- integer(repeticoes)
    nas <- integer(repeticoes)
    success <- logical(repeticoes)

    for (rep in seq_len(repeticoes)) {
      cli::cli_progress_step("Repeticao {rep}/{repeticoes}")

      t_start <- Sys.time()
      result <- tryCatch({
        switch(strategy,
          direct = {
            vigiar_baixar(tabela, limite = 10000, timeout = timeout)
          },
          year_asc_desc = {
            d1 <- vigiar_baixar(tabela, colunas = col_min,
                                ordenar_por = "ano", direcao = "asc",
                                limite = 10000, timeout = timeout)
            d2 <- vigiar_baixar(tabela, colunas = col_min,
                                ordenar_por = "ano", direcao = "desc",
                                limite = 10000, timeout = timeout)
            dados <- rbind(d1, d2)
            dados[!duplicated(dados), ]
          },
          minimal_columns = {
            vigiar_baixar(tabela, colunas = col_min,
                          limite = 10000, timeout = timeout)
          }
        )
      }, error = function(e) NULL)

      t_end <- Sys.time()
      elapsed <- as.numeric(difftime(t_end, t_start, units = "secs"))
      times[rep] <- elapsed

      if (is.null(result)) {
        success[rep] <- FALSE
        rows[rep] <- 0L
        cols[rep] <- 0L
        nas[rep] <- 0L
        cli::cli_alert_danger("Falhou ({round(elapsed, 1)}s)")
      } else {
        success[rep] <- TRUE
        rows[rep] <- nrow(result)
        cols[rep] <- ncol(result)
        nas[rep] <- sum(is.na(result))
        cli::cli_alert_success(
          "{rows[rep]} linhas x {cols[rep]} cols ({round(elapsed, 1)}s)"
        )
      }
    }

    results[[row_idx]] <- tibble::tibble(
      estrategia   = strategy,
      tempo_medio  = mean(times[success], na.rm = TRUE),
      tempo_min    = if (any(success)) min(times[success]) else NA_real_,
      tempo_max    = if (any(success)) max(times[success]) else NA_real_,
      tempo_dp     = if (any(success)) stats::sd(times[success]) else NA_real_,
      n_linhas     = stats::median(rows[success], na.rm = TRUE),
      n_colunas    = stats::median(cols[success], na.rm = TRUE),
      n_ausentes   = stats::median(nas[success], na.rm = TRUE),
      taxa_sucesso = sum(success) / repeticoes,
      repeticoes   = repeticoes
    )
    row_idx <- row_idx + 1L
  }

  out <- do.call(rbind, results[seq_len(row_idx - 1L)])
  rownames(out) <- NULL

  cli::cli_rule()
  cli::cli_h1("Resultados")

  # Find best strategy
  best <- out[which.min(out$tempo_medio), ]
  cli::cli_alert_success(
    "Melhor estrategia: {.strong {best$estrategia}} ({round(best$tempo_medio, 1)}s medio)"
  )

  tibble::as_tibble(out)
}

#' Compare download performance across tables
#'
#' Runs benchmarks on multiple tables and compares download performance.
#' Useful for monitoring API health and choosing strategies.
#'
#' @param tabelas Character vector of table names. Default: main tables.
#' @param repeticoes Number of repetitions per table.
#' @param timeout Timeout per download.
#' @return A tibble with per-table performance metrics.
#' @export
vigiar_benchmark_tabelas <- function(tabelas = NULL, repeticoes = 2L,
                                      timeout = 120) {
  if (is.null(.vigiar_env$esquema)) {
    stop("Nenhuma sessao ativa. Execute vigiar_conectar() primeiro.")
  }

  if (is.null(tabelas)) {
    tabelas <- c("df_anual", "df_mensal", "df_muni",
                 "pop", "tb_brasil", "df_indoor")
  }
  tabelas <- intersect(tabelas, names(.vigiar_env$esquema))

  cli::cli_h1("Benchmark Multi-Tabela")
  results <- vector("list", length(tabelas))

  for (i in seq_along(tabelas)) {
    tab <- tabelas[i]
    n_cols_schema <- length(.vigiar_env$esquema[[tab]])

    cli::cli_h2("[{i}/{length(tabelas)}] {tab} ({n_cols_schema} colunas)")

    times <- numeric(repeticoes)
    rows <- integer(repeticoes)
    ok <- logical(repeticoes)

    for (rep in seq_len(repeticoes)) {
      t0 <- Sys.time()
      dados <- tryCatch(
        vigiar_baixar(tab, limite = 1000, timeout = timeout),
        error = function(e) NULL
      )
      elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      times[rep] <- elapsed
      ok[rep] <- !is.null(dados)
      if (ok[rep]) rows[rep] <- nrow(dados)
    }

    results[[i]] <- tibble::tibble(
      tabela        = tab,
      colunas       = n_cols_schema,
      tempo_medio   = mean(times[ok], na.rm = TRUE),
      tempo_min     = if (any(ok)) min(times[ok]) else NA_real_,
      tempo_max     = if (any(ok)) max(times[ok]) else NA_real_,
      n_linhas      = if (any(ok)) stats::median(rows[ok], na.rm = TRUE) else NA_real_,
      taxa_sucesso  = sum(ok) / repeticoes,
      status        = if (all(ok)) "OK" else if (any(ok)) "PARCIAL" else "FALHA"
    )
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL

  cli::cli_rule()
  cli::cli_h1("Resumo")
  n_ok <- sum(out$status == "OK")
  cli::cli_alert_info(
    "{n_ok}/{length(tabelas)} tabelas baixadas com sucesso"
  )
  if (any(out$status != "OK")) {
    problemas <- out[out$status != "OK", ]
    cli::cli_alert_warning(
      "Tabelas com problemas: {.strong {paste(problemas$tabela, collapse=', ')}}"
    )
  }

  tibble::as_tibble(out)
}

#' Run a full health check on the VIGIAR API
#'
#' Connects, validates schema, benchmarks downloads, and checks
#' data quality. Returns a comprehensive health report.
#'
#' @param timeout Timeout per operation.
#' @return Invisibly, a list with health metrics.
#' @export
vigiar_health_check <- function(timeout = 120) {
  cli::cli_h1("VIGIAR Health Check")
  start_time <- Sys.time()

  # 1. Connection
  cli::cli_h2("1. Conexao")
  conn_ok <- FALSE
  tryCatch({
    vigiar_conectar(timeout = timeout)
    conn_ok <- TRUE
    cli::cli_alert_success("Dashbord conectado")
  }, error = function(e) {
    cli::cli_alert_danger("Falha na conexao: {e$message}")
  })

  if (!conn_ok) {
    return(invisible(list(online = FALSE, error = "Conexao falhou")))
  }

  # 2. Schema
  cli::cli_h2("2. Esquema")
  n_tables <- length(vigiar_tabelas())
  cli::cli_alert_info("{n_tables} tabelas disponiveis")

  # 3. Benchmark key tables
  cli::cli_h2("3. Benchmark")
  bench <- tryCatch(
    vigiar_benchmark_tabelas(
      tabelas = c("df_anual", "df_muni", "pop", "tb_brasil"),
      repeticoes = 1L,
      timeout = timeout
    ),
    error = function(e) {
      cli::cli_alert_danger("Benchmark falhou: {e$message}")
      NULL
    }
  )

  # 4. Schema compliance
  cli::cli_h2("4. Compliance de Esquema")
  compliance <- tryCatch({
    vigiar_validar_dicionario()
  }, error = function(e) {
    cli::cli_alert_danger("Validacao de dicionario falhou: {e$message}")
    NULL
  })

  # 5. Summary
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cli::cli_h1("Health Check Completo")
  cli::cli_alert_info("Tempo total: {round(elapsed, 1)}s")
  cli::cli_alert_info("Tabelas: {n_tables}")
  if (!is.null(bench)) {
    cli::cli_alert_info("Benchmark: {sum(bench$status == 'OK')}/{nrow(bench)} tabelas OK")
  }

  invisible(list(
    online      = TRUE,
    n_tables    = n_tables,
    benchmark   = bench,
    compliance  = compliance,
    elapsed     = elapsed,
    timestamp   = start_time
  ))
}

# -- Internal helpers ----------------------------------------------------------

.vigiar_benchmark_minimal_columns <- function(tabela) {
  switch(tabela,
    df_anual       = c("muni", "UF", "ano", "Media_pm25"),
    df_mensal      = c("muni", "UF", "ano", "mes", "pm25"),
    df_dias        = c("ID_MUNI", "mes", "ano", "n_dias"),
    df_dias_conama = c("ID_MUNI", "mes", "ano", "n_dias_conama"),
    pop            = c("muni", "ano", "pop", "UF"),
    tb_brasil      = c("Indicador", "est", "desfecho", "ano"),
    tb_uf          = c("Indicador", "est", "desfecho", "ano", "loc"),
    tb_muni        = c("Indicador", "est", "desfecho", "ano", "cod"),
    tb_fracao      = c("Indicador", "est", "desfecho"),
    df_indoor      = c("Code", "Ano", "comb_sol", "pop_exposta"),
    NULL
  )
}
