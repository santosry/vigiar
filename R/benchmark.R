# Package: vigiar
# Reproducible performance benchmarks and API health assessment

#' Benchmark VIGIAR download strategies
#'
#' Runs an excluded warm-up followed by repeated timed downloads. The returned
#' metrics include the median, quartiles, range, successes, failures, row counts,
#' canonical checksums, and execution environment. The `two_ended_sample`
#' strategy combines ascending and descending samples; it does not prove that
#' the interval between those samples was downloaded.
#'
#' @param tabela Table name to benchmark.
#' @param strategies Character vector containing `direct`,
#'   `two_ended_sample`, or `minimal_columns`. The legacy name
#'   `year_asc_desc` is accepted with a deprecation warning.
#' @param repeticoes Number of measured repetitions per strategy.
#' @param timeout Timeout per download in seconds.
#' @param warmup Number of unmeasured warm-up repetitions per strategy.
#' @param limite Requested row limit for each query.
#' @return A `vigiar_benchmark` tibble with reproducible timing and result
#'   identity metrics.
#' @export
vigiar_benchmark <- function(
  tabela,
  strategies = c("direct", "two_ended_sample", "minimal_columns"),
  repeticoes = 3L,
  timeout = 120,
  warmup = 1L,
  limite = 10000L
) {
  if (is.null(.vigiar_env$esquema)) {
    stop("No active session. Run vigiar_conectar() first.", call. = FALSE)
  }
  .vigiar_check_tabela(tabela)
  repeticoes <- .vigiar_positive_count(repeticoes, "repeticoes", allow_zero = FALSE)
  warmup <- .vigiar_positive_count(warmup, "warmup", allow_zero = TRUE)

  if ("year_asc_desc" %in% strategies) {
    warning(
      "Strategy 'year_asc_desc' is deprecated; use 'two_ended_sample'. ",
      "This strategy samples both ends and does not prove completeness.",
      call. = FALSE
    )
    strategies[strategies == "year_asc_desc"] <- "two_ended_sample"
  }
  if ("all" %in% strategies) {
    strategies <- c("direct", "two_ended_sample", "minimal_columns")
  }
  strategies <- unique(match.arg(
    strategies,
    c("direct", "two_ended_sample", "minimal_columns"),
    several.ok = TRUE
  ))

  cli::cli_h1("VIGIAR benchmark")
  cli::cli_text("Table: {.strong {tabela}}")
  cli::cli_text("Warm-up: {warmup}; measured repetitions: {repeticoes}")
  cli::cli_rule()

  minimal_columns <- intersect(
    .vigiar_benchmark_minimal_columns(tabela) %||% character(),
    names(.vigiar_env$esquema[[tabela]])
  )
  environment <- .vigiar_benchmark_environment()
  results <- lapply(strategies, function(strategy) {
    for (i in seq_len(warmup)) {
      try(
        .vigiar_run_benchmark_strategy(
          tabela, strategy, minimal_columns, limite, timeout
        ),
        silent = TRUE
      )
    }

    runs <- vector("list", repeticoes)
    for (i in seq_len(repeticoes)) {
      started <- Sys.time()
      run <- tryCatch(
        list(
          ok = TRUE,
          data = .vigiar_run_benchmark_strategy(
            tabela, strategy, minimal_columns, limite, timeout
          ),
          error = NA_character_
        ),
        error = function(e) list(
          ok = FALSE, data = NULL, error = conditionMessage(e)
        )
      )
      run$elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
      runs[[i]] <- run
    }
    .vigiar_summarize_benchmark_runs(
      tabela, strategy, runs, warmup, limite, environment
    )
  })

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out <- tibble::as_tibble(out)
  class(out) <- c("vigiar_benchmark", class(out))

  successful <- out[out$n_success > 0L, , drop = FALSE]
  if (nrow(successful) > 0L) {
    best <- successful[which.min(successful$elapsed_median), , drop = FALSE]
    cli::cli_alert_success(
      paste0(
        "Fastest measured strategy: {best$strategy[[1]]} ",
        "({round(best$elapsed_median[[1]], 3)} seconds median)."
      )
    )
  } else {
    cli::cli_alert_danger("Every measured benchmark run failed.")
  }
  out
}

#' Compare reproducible download benchmarks across tables
#'
#' @param tabelas Character vector of table names. Defaults to key tables that
#'   are present in the live schema.
#' @param repeticoes Number of measured repetitions per table.
#' @param timeout Timeout per download in seconds.
#' @param warmup Number of unmeasured warm-up repetitions per table.
#' @param limite Requested row limit per query.
#' @return A `vigiar_benchmark` tibble with one direct-strategy row per table.
#' @export
vigiar_benchmark_tabelas <- function(
  tabelas = NULL,
  repeticoes = 2L,
  timeout = 120,
  warmup = 1L,
  limite = 1000L
) {
  if (is.null(.vigiar_env$esquema)) {
    stop("No active session. Run vigiar_conectar() first.", call. = FALSE)
  }
  if (is.null(tabelas)) {
    tabelas <- c("df_anual", "df_mensal", "df_muni", "pop", "tb_brasil")
  }
  unknown <- setdiff(tabelas, names(.vigiar_env$esquema))
  if (length(unknown) > 0L) {
    warning(
      "Skipping tables absent from the live schema: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  tabelas <- intersect(tabelas, names(.vigiar_env$esquema))
  if (length(tabelas) == 0L) {
    stop("None of the requested tables exists in the live schema.", call. = FALSE)
  }

  results <- lapply(tabelas, function(tabela) {
    result <- vigiar_benchmark(
      tabela = tabela,
      strategies = "direct",
      repeticoes = repeticoes,
      timeout = timeout,
      warmup = warmup,
      limite = limite
    )
    result$schema_columns <- length(.vigiar_env$esquema[[tabela]])
    result$status <- if (result$success_rate == 1) {
      "pass"
    } else if (result$success_rate > 0) {
      "partial"
    } else {
      "fail"
    }
    result
  })
  out <- tibble::as_tibble(do.call(rbind, results))
  class(out) <- c("vigiar_benchmark", class(out))
  out
}

#' Run a conservative health check on the VIGIAR API
#'
#' The report separates connection, critical-schema compatibility, and download
#' canary status. Unknown evidence never becomes a pass. This online function is
#' disabled when `VIGIAR_RUN_ONLINE_TESTS=false`.
#'
#' @param timeout Timeout per online operation.
#' @param require_healthy If `TRUE`, error unless the final status is `pass`.
#' @return A structured `vigiar_health_report` object.
#' @export
vigiar_health_check <- function(timeout = 120, require_healthy = FALSE) {
  run_online <- Sys.getenv("VIGIAR_RUN_ONLINE_TESTS", unset = NA_character_)
  if (identical(tolower(run_online), "false")) {
    stop("Online health check disabled by VIGIAR_RUN_ONLINE_TESTS=false.",
         call. = FALSE)
  }

  started <- Sys.time()
  .vigiar_log("INFO", "Starting VIGIAR API health check.",
              event = "health_check_start")
  connection_error <- NULL
  if (is.null(.vigiar_env$sessao)) {
    tryCatch(
      vigiar_conectar(timeout = timeout),
      error = function(e) connection_error <<- conditionMessage(e)
    )
  }

  connection_status <- if (is.null(.vigiar_env$sessao)) "fail" else "pass"
  if (connection_status == "fail") {
    report <- .vigiar_health_report(
      started = started,
      connection = .vigiar_check_result("fail", details = connection_error),
      schema = .vigiar_check_result("unknown", details = "No live schema."),
      canary = .vigiar_check_result("unknown", details = "No download attempted."),
      benchmark = NULL
    )
    return(.vigiar_finish_health_report(report, require_healthy))
  }

  schema_error <- NULL
  schema_diff <- tryCatch(
    vigiar_esquema_verificar_critico(error = FALSE),
    error = function(e) {
      schema_error <<- conditionMessage(e)
      NULL
    }
  )
  schema_check <- if (!is.null(schema_error)) {
    .vigiar_check_result("unknown", details = schema_error)
  } else if (length(schema_diff) == 0L) {
    .vigiar_check_result("pass", details = "Critical schema matches the lock.")
  } else {
    .vigiar_check_result("fail", details = jsonlite::toJSON(
      schema_diff, auto_unbox = TRUE, null = "null"
    ))
  }

  tables <- intersect(
    c("df_anual", "df_muni", "pop", "tb_brasil"),
    names(.vigiar_env$esquema)
  )
  benchmark <- if (length(tables) == 0L) NULL else tryCatch(
    vigiar_benchmark_tabelas(
      tabelas = tables, repeticoes = 1L, timeout = timeout,
      warmup = 0L, limite = 1000L
    ),
    error = function(e) e
  )
  canary_check <- if (inherits(benchmark, "error")) {
    .vigiar_check_result("fail", details = conditionMessage(benchmark))
  } else if (is.null(benchmark)) {
    .vigiar_check_result("unknown", details = "No canary table is available.")
  } else if (all(benchmark$status == "pass")) {
    .vigiar_check_result("pass", details = "All download canaries succeeded.")
  } else if (all(benchmark$status == "fail")) {
    .vigiar_check_result("fail", details = "Every download canary failed.")
  } else {
    .vigiar_check_result("unknown", details = "Download canaries were only partial.")
  }

  report <- .vigiar_health_report(
    started = started,
    connection = .vigiar_check_result("pass", details = "Connection established."),
    schema = schema_check,
    canary = canary_check,
    benchmark = if (inherits(benchmark, "error")) NULL else benchmark
  )
  .vigiar_finish_health_report(report, require_healthy)
}

#' @export
print.vigiar_health_report <- function(x, ...) {
  cat("<vigiar_health_report>\n")
  cat("  status:     ", x$status, "\n", sep = "")
  cat("  connection: ", x$connection$status, "\n", sep = "")
  cat("  schema:     ", x$schema$status, "\n", sep = "")
  cat("  canary:     ", x$canary$status, "\n", sep = "")
  cat("  elapsed:    ", round(x$elapsed_seconds, 3), " seconds\n", sep = "")
  invisible(x)
}

.vigiar_health_report <- function(started, connection, schema, canary, benchmark) {
  checks <- list(connection = connection, schema = schema, canary = canary)
  aggregate <- .vigiar_aggregate_checks(checks)
  report <- list(
    status = aggregate$status,
    ok = identical(aggregate$status, "pass"),
    connection = connection,
    schema = schema,
    canary = canary,
    benchmark = benchmark,
    started_at = started,
    finished_at = Sys.time(),
    elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
    environment = .vigiar_benchmark_environment()
  )
  class(report) <- "vigiar_health_report"
  report
}

.vigiar_finish_health_report <- function(report, require_healthy) {
  .vigiar_log(
    if (report$status == "pass") "INFO" else if (report$status == "unknown") "WARN" else "ERROR",
    sprintf("VIGIAR API health check finished with status '%s'.", report$status),
    metadata = list(
      status = report$status,
      connection = report$connection$status,
      schema = report$schema$status,
      canary = report$canary$status
    ),
    event = "health_check"
  )
  if (isTRUE(require_healthy) && !identical(report$status, "pass")) {
    stop(
      sprintf("VIGIAR health status is '%s'; inspect the structured report.",
              report$status),
      call. = FALSE
    )
  }
  invisible(report)
}

.vigiar_run_benchmark_strategy <- function(tabela, strategy, minimal_columns,
                                            limite, timeout) {
  switch(strategy,
    direct = vigiar_baixar(tabela, limite = limite, timeout = timeout),
    two_ended_sample = {
      first <- vigiar_baixar(
        tabela, colunas = minimal_columns, ordenar_por = "ano",
        direcao = "asc", limite = limite, timeout = timeout
      )
      last <- vigiar_baixar(
        tabela, colunas = minimal_columns, ordenar_por = "ano",
        direcao = "desc", limite = limite, timeout = timeout
      )
      combined <- rbind(first, last)
      combined[!duplicated(combined), , drop = FALSE]
    },
    minimal_columns = vigiar_baixar(
      tabela, colunas = minimal_columns, limite = limite, timeout = timeout
    )
  )
}

.vigiar_summarize_benchmark_runs <- function(tabela, strategy, runs, warmup,
                                              limite, environment) {
  ok <- vapply(runs, `[[`, logical(1), "ok")
  elapsed <- vapply(runs, `[[`, numeric(1), "elapsed")
  success_elapsed <- elapsed[ok]
  data <- lapply(runs[ok], `[[`, "data")
  rows <- vapply(data, nrow, integer(1))
  columns <- vapply(data, ncol, integer(1))
  checksums <- vapply(data, vigiar_checksum, character(1))
  errors <- vapply(runs[!ok], `[[`, character(1), "error")

  stat <- function(fun, default = NA_real_) {
    if (length(success_elapsed) == 0L) default else fun(success_elapsed)
  }
  quant <- function(prob) stat(function(x) {
    as.numeric(stats::quantile(x, prob, names = FALSE, type = 7))
  })
  tibble::tibble(
    table = tabela,
    strategy = strategy,
    warmup_runs = warmup,
    measured_runs = length(runs),
    n_success = sum(ok),
    n_failure = sum(!ok),
    success_rate = mean(ok),
    elapsed_median = stat(stats::median),
    elapsed_p25 = quant(0.25),
    elapsed_p75 = quant(0.75),
    elapsed_min = stat(min),
    elapsed_max = stat(max),
    rows_median = if (length(rows) == 0L) NA_real_ else stats::median(rows),
    columns_median = if (length(columns) == 0L) NA_real_ else stats::median(columns),
    checksums = if (length(checksums) == 0L) NA_character_ else
      paste(sort(unique(checksums)), collapse = ";"),
    errors = if (length(errors) == 0L) NA_character_ else
      paste(unique(errors), collapse = "; "),
    requested_limit = limite %||% NA_integer_,
    environment = environment
  )
}

.vigiar_positive_count <- function(x, name, allow_zero) {
  if (length(x) != 1L || is.na(x) || x != as.integer(x) ||
      x < if (allow_zero) 0L else 1L) {
    stop(sprintf("'%s' must be %s integer.", name,
                 if (allow_zero) "a non-negative" else "a positive"),
         call. = FALSE)
  }
  as.integer(x)
}

.vigiar_benchmark_environment <- function() {
  jsonlite::toJSON(
    list(
      r_version = R.version.string,
      vigiar_version = as.character(utils::packageVersion("vigiar")),
      platform = R.version$platform,
      os = unname(Sys.info()[["sysname"]]),
      machine = unname(Sys.info()[["machine"]])
    ),
    auto_unbox = TRUE,
    null = "null"
  )
}

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
