# Package: vigiar
# Compliance auditing and data quality assurance
#
# Implements comprehensive data auditing following the microdatasus
# philosophy: every download is validated, every deviation is reported,
# and audit trails are preserved for reproducibility.

.vigiar_check_result <- function(status = c("pass", "fail", "unknown", "not_applicable"),
                                 severity = NULL, details = character(), ...) {
  status <- match.arg(status)
  if (is.null(severity)) {
    severity <- switch(status,
      pass = "ok",
      fail = "error",
      unknown = "warning",
      not_applicable = "info"
    )
  }
  list(
    ok = identical(status, "pass"),
    status = status,
    severity = severity,
    details = as.character(details),
    ...
  )
}

.vigiar_normalize_check <- function(x, label = "check") {
  if (!is.list(x)) {
    return(.vigiar_check_result(
      "unknown",
      details = sprintf("%s did not return a structured result.", label)
    ))
  }

  status <- x$status %||% NULL
  if (is.null(status)) {
    legacy_ok <- x$ok %||% x$passed %||% x$valido %||% NULL
    if (is.null(legacy_ok) || length(legacy_ok) != 1L || is.na(legacy_ok)) {
      status <- "unknown"
    } else {
      status <- if (isTRUE(legacy_ok)) "pass" else "fail"
    }
  }

  if (!status %in% c("pass", "fail", "unknown", "not_applicable")) {
    status <- "unknown"
  }
  normalized <- .vigiar_check_result(
    status,
    severity = x$severity %||% NULL,
    details = x$details %||% character()
  )
  utils::modifyList(x, normalized)
}

.vigiar_aggregate_checks <- function(checks) {
  normalized <- Map(.vigiar_normalize_check, checks, names(checks))
  statuses <- vapply(normalized, `[[`, character(1), "status")
  status <- if (any(statuses == "fail")) {
    "fail"
  } else if (any(statuses == "unknown")) {
    "unknown"
  } else if (length(statuses) > 0L && all(statuses == "not_applicable")) {
    "not_applicable"
  } else {
    "pass"
  }
  result <- .vigiar_check_result(
    status,
    details = unlist(lapply(normalized, `[[`, "details"), use.names = FALSE)
  )
  c(normalized, result)
}

#' Full data compliance audit
#'
#' Performs a comprehensive audit of a VIGIAR data table against
#' the expected schema, IBGE standards, and data quality rules.
#' Returns a structured report suitable for regulatory compliance.
#'
#' @param dados A VIGIAR data frame or tibble.
#' @param tabela Table name for context.
#' @param verbose If \code{TRUE}, prints detailed audit results.
#' @return A list with audit sections: \code{schema}, \code{ibge},
#'   \code{temporal}, \code{units}, \code{coverage}, \code{checksums}.
#' @export
vigiar_auditar <- function(dados, tabela = NULL, verbose = TRUE) {
  tabela <- tabela %||% attr(dados, "vigiar_tabela") %||% "desconhecida"

  if (verbose) {
    cli::cli_h1("Auditoria VIGIAR")
    cli::cli_text("Tabela: {.strong {tabela}}")
    cli::cli_text("Auditado em: {format(Sys.time())}")
    cli::cli_rule()
  }

  audit <- list(
    tabela     = tabela,
    timestamp  = Sys.time(),
    r_version  = R.version.string,
    vigiar_version = as.character(utils::packageVersion("vigiar")),
    session_info  = .vigiar_session_info()
  )

  # 1. Schema compliance
  checks <- list(
    schema = .vigiar_auditar_schema(dados, tabela, verbose),
    ibge = .vigiar_auditar_ibge(dados, verbose),
    temporal = .vigiar_auditar_temporal(dados, verbose, tabela),
    units = .vigiar_auditar_units(dados, tabela, verbose),
    coverage = .vigiar_auditar_coverage(dados, verbose, tabela),
    checksums = .vigiar_auditar_checksums(dados, verbose)
  )
  assessment <- .vigiar_aggregate_checks(checks)
  for (name in names(checks)) {
    audit[[name]] <- assessment[[name]]
  }
  audit$status <- assessment$status
  audit$severity <- assessment$severity
  audit$details <- assessment$details
  audit$passed <- identical(audit$status, "pass")
  audit$ok <- audit$passed

  .vigiar_log(
    if (audit$status == "pass") "INFO" else if (audit$status == "unknown") "WARN" else "ERROR",
    sprintf("Audit completed with status '%s'.", audit$status),
    table = tabela,
    metadata = list(
      status = audit$status,
      severity = audit$severity,
      details = audit$details
    ),
    event = "audit"
  )

  if (verbose) {
    cli::cli_rule()
    if (audit$status == "pass") {
      cli::cli_alert_success("AUDIT PASSED -- all required checks passed")
    } else if (audit$status == "unknown") {
      cli::cli_alert_warning(
        "AUDIT UNVERIFIED -- one or more required properties are unknown"
      )
    } else {
      cli::cli_alert_danger("AUDIT FAILED -- inspect sections marked FAIL")
    }
  }

  class(audit) <- "vigiar_audit"
  invisible(audit)
}

#' Print method for vigiar_audit
#' @param x A vigiar_audit object.
#' @param ... Additional arguments (ignored).
#' @export
print.vigiar_audit <- function(x, ...) {
  cli::cli_h1("Relatorio de Auditoria VIGIAR")
  cli::cli_text("Tabela: {x$tabela}")
  cli::cli_text("Data: {format(x$timestamp)}")
  cli::cli_text("R: {x$r_version}")
  cli::cli_text("vigiar: {x$vigiar_version}")
  cli::cli_rule()

  sections <- c("schema", "ibge", "temporal", "units", "coverage")
  labels <- c(
    schema   = "Conformidade de Esquema",
    ibge     = "Validacao de Codigos IBGE",
    temporal = "Consistencia Temporal",
    units    = "Validacao de Unidades",
    coverage = "Cobertura Espacial"
  )

  for (s in sections) {
    if (!is.null(x[[s]])) {
      status <- switch(x[[s]]$status %||% if (isTRUE(x[[s]]$ok)) "pass" else "fail",
        pass = cli::col_green("PASS"),
        fail = cli::col_red("FAIL"),
        unknown = cli::col_yellow("UNKNOWN"),
        not_applicable = cli::col_silver("N/A")
      )
      cli::cli_text("{status} {labels[s]}")
      if (!is.null(x[[s]]$details)) {
        for (d in x[[s]]$details) {
          cli::cli_text("  - {d}")
        }
      }
    }
  }

  cli::cli_rule()
  if (identical(x$status %||% if (x$passed) "pass" else "fail", "pass")) {
    cli::cli_alert_success("Resultado final: APROVADO")
  } else if (identical(x$status, "unknown")) {
    cli::cli_alert_warning("Final result: UNVERIFIED")
  } else {
    cli::cli_alert_danger("Resultado final: REPROVADO")
  }
  invisible(x)
}

#' Audit all downloaded tables
#'
#' Runs \code{vigiar_auditar()} on every table in a named list
#' (e.g., the result of \code{vigiar_baixar_tudo()}).
#'
#' @param dados_list Named list of data frames.
#' @param verbose If \code{TRUE}, prints progress.
#' @return A named list of audit reports.
#' @export
vigiar_auditar_tudo <- function(dados_list, verbose = TRUE) {
  if (!is.list(dados_list) || is.null(names(dados_list))) {
    stop("'dados_list' deve ser uma lista nomeada de data frames.")
  }

  results <- vector("list", length(dados_list))
  names(results) <- names(dados_list)

  for (i in seq_along(dados_list)) {
    tab <- names(dados_list)[i]
    if (verbose) cli::cli_text("Auditando: {tab} ({i}/{length(dados_list)})")
    results[[tab]] <- tryCatch(
      vigiar_auditar(dados_list[[tab]], tabela = tab, verbose = FALSE),
      error = function(e) {
        list(tabela = tab, error = e$message, passed = FALSE)
      }
    )
  }

  n_passed <- sum(vapply(results, function(x) isTRUE(x$passed), logical(1)))
  if (verbose) {
    cli::cli_rule()
    cli::cli_alert_info(
      "Auditoria concluida: {n_passed}/{length(results)} tabelas aprovadas"
    )
  }

  class(results) <- "vigiar_audit_list"
  invisible(results)
}

#' Print method for vigiar_audit_list
#' @param x A vigiar_audit_list object.
#' @param ... Additional arguments (ignored).
#' @export
print.vigiar_audit_list <- function(x, ...) {
  n <- length(x)
  n_ok <- sum(vapply(x, function(a) isTRUE(a$passed), logical(1)))
  cli::cli_h1("Auditoria Multi-Tabela")
  cli::cli_text("{n_ok}/{n} tabelas aprovadas")
  cli::cli_rule()
  for (tab in names(x)) {
    a <- x[[tab]]
    status <- if (isTRUE(a$passed)) cli::col_green("OK") else cli::col_red("FAIL")
    cli::cli_text("{status} {tab}")
  }
  invisible(x)
}

#' Run a batch audit across multiple compliance profiles
#'
#' @param dados A VIGIAR data frame.
#' @param tabela Table name.
#' @param profiles Character vector of audit profiles:
#'   \code{"basico"} (default), \code{"rigoroso"}, \code{"rj"},
#'   \code{"corrupcao"}. Use \code{"all"} for everything.
#' @param verbose If \code{TRUE}, prints progress and detailed results.
#' @return A list of per-profile audit results.
#' @export
vigiar_compliance_check <- function(dados, tabela = NULL,
                                     profiles = c("basico", "rigoroso", "rj"),
                                     verbose = TRUE) {
  tabela <- tabela %||% attr(dados, "vigiar_tabela") %||% "desconhecida"

  all_profiles <- c("basico", "rigoroso", "rj", "corrupcao")
  if ("all" %in% profiles) profiles <- all_profiles
  profiles <- match.arg(profiles, all_profiles, several.ok = TRUE)

  results <- vector("list", length(profiles))
  names(results) <- profiles

  for (p in profiles) {
    if (verbose) cli::cli_h2("Perfil: {p}")
    results[[p]] <- switch(p,
      basico = {
        # Basic: schema + IBGE + temporal
        checks <- list(
          schema   = .vigiar_auditar_schema(dados, tabela, verbose),
          ibge     = .vigiar_auditar_ibge(dados, verbose),
          temporal = .vigiar_auditar_temporal(dados, verbose, tabela)
        )
        .vigiar_aggregate_checks(checks)
      },
      rigoroso = {
        # Strict: everything + outlier detection
        base <- vigiar_auditar(dados, tabela, verbose = FALSE)
        base$outliers <- .vigiar_detectar_outliers(dados, verbose = verbose)
        base$ok <- isTRUE(base$passed)
        base$status <- if (base$ok) "pass" else "fail"
        base$severity <- if (base$ok) "ok" else "error"
        base
      },
      rj = {
        # RJ-specific compliance
        validar_rj <- function() {
          tryCatch(
            vigiar_validar_rj(dados),
            error = function(e) .vigiar_check_result(
              "fail",
              details = conditionMessage(e),
              error = conditionMessage(e)
            )
          )
        }
        checks <- list(
          rj_municipios = if (verbose) validar_rj() else
            suppressWarnings(suppressMessages(validar_rj())),
          rj_cobertura = .vigiar_auditar_cobertura_rj(dados, verbose = verbose),
          truncation = if (isTRUE(attr(dados, "vigiar_possivel_truncamento"))) {
            .vigiar_check_result(
              "fail",
              details = paste(
                "Possible API truncation prevents a complete RJ compliance",
                "claim; use a validated partitioned download."
              )
            )
          } else {
            .vigiar_check_result("pass", details = "No truncation evidence recorded.")
          }
        )
        .vigiar_aggregate_checks(checks)
      },
      corrupcao = {
        # Data integrity / corruption checks
        .vigiar_auditar_integridade(dados, tabela, verbose = verbose)
      }
    )

    results[[p]] <- .vigiar_normalize_check(results[[p]], p)
  }

  all_ok <- all(vapply(results, function(x) isTRUE(x$ok), logical(1)))
  overall_status <- if (any(vapply(
    results, function(x) identical(x$status, "fail"), logical(1)
  ))) {
    "fail"
  } else if (all_ok) {
    "pass"
  } else {
    "unknown"
  }
  .vigiar_log(
    if (overall_status == "pass") "INFO" else if (overall_status == "unknown") "WARN" else "ERROR",
    sprintf("Compliance completed with status '%s'.", overall_status),
    table = tabela,
    metadata = list(
      status = overall_status,
      profiles = profiles,
      profile_status = vapply(results, `[[`, "", "status")
    ),
    event = if (overall_status == "fail") "compliance_failure" else "compliance"
  )

  if (verbose) {
    cli::cli_rule()
    if (all_ok) {
      cli::cli_alert_success("COMPLIANCE PASSED: all requested profiles passed")
    } else {
      fails <- names(results)[!vapply(results, function(x) isTRUE(x$ok), logical(1))]
      cli::cli_alert_danger("COMPLIANCE FAILED for profiles: {paste(fails, collapse=', ')}")
    }
  }

  class(results) <- "vigiar_compliance"
  invisible(results)
}

#' Print method for vigiar_compliance
#' @param x A vigiar_compliance object.
#' @param ... Additional arguments (ignored).
#' @export
print.vigiar_compliance <- function(x, ...) {
  cli::cli_h1("Relatorio de Compliance VIGIAR")
  for (p in names(x)) {
    status <- if (isTRUE(x[[p]]$ok)) cli::col_green("PASS") else cli::col_red("FAIL")
    cli::cli_text("{status} Perfil: {p}")
  }
  invisible(x)
}

# -- Internal audit helpers ----------------------------------------------------

.vigiar_auditar_schema <- function(dados, tabela, verbose) {
  n_rows <- nrow(dados)
  n_cols <- ncol(dados)
  col_names <- names(dados)
  n_dup <- sum(duplicated(dados))
  na_total <- sum(is.na(dados))
  na_pct <- if (n_rows > 0) round(100 * na_total / (n_rows * n_cols), 2) else 0

  issues <- character()
  warnings <- character()
  schema_available <- !is.null(.vigiar_env$esquema) &&
    tabela %in% names(.vigiar_env$esquema)
  missing <- extra <- character()

  if (n_rows == 0L) issues <- c(issues, "Table has zero rows.")
  if (n_cols == 0L) issues <- c(issues, "Table has zero columns.")
  if (n_dup > 0L) warnings <- c(warnings, sprintf(
    "%d duplicate row(s) require table-grain review.", n_dup
  ))
  if (na_pct > 20) warnings <- c(warnings, sprintf(
    "High missing-value rate: %.1f%%.", na_pct
  ))

  if (schema_available) {
    expected_cols <- names(.vigiar_env$esquema[[tabela]])
    missing <- setdiff(expected_cols, col_names)
    extra <- setdiff(col_names, expected_cols)
    if (length(missing) > 0) {
      issues <- c(issues, sprintf("Expected columns are absent: %s.",
                                  paste(missing, collapse = ", ")))
    }
    if (length(extra) > 0) {
      warnings <- c(warnings, sprintf("Additional columns require review: %s.",
                                      paste(extra, collapse = ", ")))
    }
  } else {
    warnings <- c(warnings, "Expected schema is unavailable; compatibility is unknown.")
  }

  status <- if (length(issues) > 0L) {
    "fail"
  } else if (!schema_available || length(warnings) > 0L) {
    "unknown"
  } else {
    "pass"
  }
  result <- c(.vigiar_check_result(
    status,
    details = c(issues, warnings)
  ), list(
    n_rows     = n_rows,
    n_cols     = n_cols,
    n_dup      = n_dup,
    na_total   = na_total,
    na_pct     = na_pct,
    schema_available = schema_available,
    missing_columns = missing,
    extra_columns = extra,
    equality_status = if (!schema_available) "unknown" else if (
      length(missing) == 0L && length(extra) == 0L
    ) "pass" else "fail",
    compatibility_status = if (!schema_available) "unknown" else if (
      length(missing) == 0L
    ) "pass" else "fail"
  ))

  if (verbose && length(result$details) > 0) {
    for (issue in result$details) cli::cli_alert_warning(issue)
  }
  if (verbose && result$status == "pass") cli::cli_alert_success("Schema: OK")

  result
}

.vigiar_auditar_ibge <- function(dados, verbose) {
  col_muni <- intersect(
    c("cod_municipio", "muni", "id_muni", "ID_MUNI", "codigo_ibge",
      "cod_ibge", "codigo_municipio", "MUN_COD"),
    names(dados)
  )[1]

  if (is.na(col_muni)) {
    if (verbose) cli::cli_alert_warning("IBGE municipality column is absent.")
    return(.vigiar_check_result(
      "unknown",
      details = "IBGE municipality existence and membership could not be verified."
    ))
  }

  raw_codes <- dados[[col_muni]]
  validation <- vigiar_validar_codigo_municipio(raw_codes)
  raw_non_missing <- raw_codes[!is.na(raw_codes)]

  if (length(raw_non_missing) == 0) {
    return(.vigiar_check_result(
      "unknown",
      details = "All municipality code values are missing."
    ))
  }

  invalidos <- raw_codes[validation$status == "fail"]
  validos <- validation$codigo_ibge_6[validation$status == "pass"]

  ok <- length(invalidos) == 0
  details <- sprintf(
    "%d codigos IBGE, %d validos, %d fora do intervalo esperado",
    length(raw_non_missing), length(validos), length(invalidos)
  )

  if (verbose) {
    if (ok) {
      cli::cli_alert_success("IBGE: {details}")
    } else {
      cli::cli_alert_warning("IBGE: {details}")
    }
  }

  c(.vigiar_check_result(
    if (ok) "pass" else "fail",
    details = details
  ), list(
    n_total   = length(raw_non_missing),
    n_validos = length(validos),
    n_invalidos = length(invalidos),
    codigos_invalidos = invalidos
  ))
}

.vigiar_auditar_temporal <- function(dados, verbose, tabela = NULL) {
  issues <- character(0)
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  requires_year <- tabela %in% c(
    "df_anual", "df_mensal", "df_dias", "df_dias_conama", "pop"
  )
  requires_month <- tabela %in% c("df_mensal", "df_dias", "df_dias_conama")

  if ("ano" %in% names(dados)) {
    anos <- as.integer(dados$ano)
    anos <- anos[!is.na(anos)]
    bad_anos <- sum(anos < 2000 | anos > current_year)
    if (bad_anos > 0) {
      issues <- c(issues, sprintf("%d anos fora de 2000-%d", bad_anos, current_year))
    }
    range_anos <- if (length(anos) > 0) paste(range(anos), collapse = "-") else "N/A"
  } else {
    range_anos <- "N/A"
    if (requires_year) {
      issues <- c(issues, "Required year column is absent.")
    }
  }

  if ("mes" %in% names(dados)) {
    meses <- as.integer(dados$mes)
    meses <- meses[!is.na(meses)]
    bad_mes <- sum(meses < 1 | meses > 12)
    if (bad_mes > 0) {
      issues <- c(issues, sprintf("%d meses fora de 1-12", bad_mes))
    }
  } else if (requires_month) {
    issues <- c(issues, "Required month column is absent.")
  }

  status <- if (length(issues) > 0L) {
    "fail"
  } else if (!"ano" %in% names(dados) && !"mes" %in% names(dados)) {
    "not_applicable"
  } else {
    "pass"
  }

  if (verbose) {
    if (status == "pass") {
      cli::cli_alert_success("Temporal: faixa {range_anos} -- OK")
    } else {
      for (i in issues) cli::cli_alert_warning(i)
    }
  }

  c(.vigiar_check_result(status, details = issues), list(
    faixa_anos = range_anos,
    expected_year = requires_year,
    expected_month = requires_month
  ))
}

.vigiar_auditar_units <- function(dados, tabela, verbose) {
  issues <- character(0)

  # PM2.5 range check
  pm25_cols <- intersect(
    c("pm25_media", "pm25_media_anual", "pm25_media_periodo", "Media_pm25"),
    names(dados)
  )
  for (col in pm25_cols) {
    vals <- as.numeric(dados[[col]])
    bad <- sum(!is.na(vals) & (vals < 0 | vals > 1000))
    if (bad > 0) {
      issues <- c(issues, sprintf("PM2.5 (%s): %d valores implausiveis", col, bad))
    }
  }

  # Population magnitude check
  pop_cols <- intersect(c("populacao", "populacao_exposta", "pop"), names(dados))
  for (col in pop_cols) {
    vals <- as.numeric(dados[[col]])
    bad <- sum(!is.na(vals) & vals < 0, na.rm = TRUE)
    if (bad > 0) {
      issues <- c(issues, sprintf("Populacao (%s): %d valores negativos", col, bad))
    }
  }

  relevant <- length(pm25_cols) + length(pop_cols) > 0L
  status <- if (length(issues) > 0L) {
    "fail"
  } else if (!relevant) {
    "not_applicable"
  } else {
    "pass"
  }

  if (verbose) {
    if (status == "pass") {
      cli::cli_alert_success("Unidades: OK")
    } else {
      for (i in issues) cli::cli_alert_warning(i)
    }
  }

  .vigiar_check_result(status, details = issues)
}

.vigiar_auditar_coverage <- function(dados, verbose, tabela = NULL) {
  col_uf <- .vigiar_coluna_uf(dados)
  col_muni <- .vigiar_coluna_municipio(dados)

  n_uf <- NA_integer_
  n_muni <- NA_integer_

  if (!is.na(col_uf)) n_uf <- dplyr::n_distinct(dados[[col_uf]], na.rm = TRUE)
  if (!is.na(col_muni)) n_muni <- dplyr::n_distinct(dados[[col_muni]], na.rm = TRUE)

  if (verbose) {
    msg <- sprintf(
      "Cobertura: %s UFs, %s municipios",
      if (is.na(n_uf)) "?" else as.character(n_uf),
      if (is.na(n_muni)) "?" else as.character(n_muni)
    )
    cli::cli_alert_info(msg)
  }

  requires_municipality <- tabela %in% c(
    "df_anual", "df_mensal", "df_dias", "df_dias_conama", "pop", "df_muni"
  )
  status <- if (requires_municipality && is.na(col_muni)) {
    "fail"
  } else {
    "unknown"
  }
  details <- if (status == "fail") {
    "Municipality coverage is required but no municipality code column exists."
  } else {
    "No expected geographic domain was supplied; coverage remains unverified."
  }
  c(.vigiar_check_result(status, details = details), list(
    n_uf    = n_uf,
    n_muni  = n_muni
  ))
}

.vigiar_auditar_cobertura_rj <- function(dados, verbose) {
  col_muni <- intersect(
    c("cod_municipio", "muni", "ID_MUNI", "codigo_ibge"), names(dados)
  )[1]

  if (is.na(col_muni)) {
    if (verbose) cli::cli_alert_warning("Cobertura RJ: sem coluna de municipio")
    return(.vigiar_check_result(
      "fail",
      details = "Municipality code column is required for RJ coverage."
    ))
  }

  codigos <- unique(.vigiar_normalizar_codigo_municipio(dados[[col_muni]]))
  codigos <- codigos[!is.na(codigos)]

  rj_codes <- RJ_MUNICIPIOS$codigo_ibge_6
  presentes <- intersect(codigos, rj_codes)
  faltantes <- setdiff(rj_codes, codigos)

  pct <- round(100 * length(presentes) / 92, 1)

  if (verbose) {
    cli::cli_alert_info(
      "Cobertura RJ: {length(presentes)}/92 municipios ({pct}%)"
    )
    if (length(faltantes) > 0) {
      cli::cli_alert_warning(
        "{length(faltantes)} municipios RJ faltando"
      )
    }
  }

  c(.vigiar_check_result(
    if (length(faltantes) == 0L) "pass" else "fail",
    details = if (length(faltantes) == 0L) {
      "All 92 Rio de Janeiro municipalities are present."
    } else {
      sprintf("%d Rio de Janeiro municipalities are absent.", length(faltantes))
    }
  ), list(
    n_presentes        = length(presentes),
    n_faltantes        = length(faltantes),
    pct_cobertura      = pct,
    municipios_faltantes = faltantes
  ))
}

.vigiar_auditar_integridade <- function(dados, tabela, verbose) {
  issues <- character(0)
  anomalies <- list()

  # Check for all-NA columns
  all_na <- vapply(dados, function(x) all(is.na(x)), logical(1))
  if (any(all_na)) {
    na_cols <- names(dados)[all_na]
    issues <- c(issues, sprintf("Colunas 100%% NA: %s", paste(na_cols, collapse = ", ")))
  }

  # Check for constant-value columns
  constant <- vapply(dados, function(x) {
    if (all(is.na(x))) return(FALSE)
    length(unique(x[!is.na(x)])) == 1L
  }, logical(1))
  if (any(constant)) {
    const_cols <- names(dados)[constant]
    anomalies$constant <- const_cols
    if (verbose) {
      cli::cli_alert_info(
        "Constant columns recorded as anomalies: {paste(const_cols, collapse=', ')}"
      )
    }
  }

  # Check for mixed types per column (raw list columns)
  for (col in names(dados)) {
    if (is.list(dados[[col]]) && !inherits(dados[[col]], "POSIXct")) {
      types <- unique(vapply(dados[[col]], typeof, ""))
      if (length(types) > 1) {
        issues <- c(issues, sprintf("Coluna '%s' tem tipos mistos: %s",
                                    col, paste(types, collapse = ", ")))
      }
    }
  }

  # Row count consistency with schema
  if (!is.null(.vigiar_env$esquema) && tabela %in% names(.vigiar_env$esquema)) {
    expected_ncols <- length(.vigiar_env$esquema[[tabela]])
    if (ncol(dados) != expected_ncols) {
      issues <- c(issues,
        sprintf("Numero de colunas: %d (esperado: %d)", ncol(dados), expected_ncols))
    }
  }

  ok <- length(issues) == 0

  if (verbose) {
    if (ok) {
      cli::cli_alert_success("Integridade: OK")
    } else {
      for (i in issues) cli::cli_alert_danger(i)
    }
  }

  c(.vigiar_check_result(if (ok) "pass" else "fail", details = issues), list(
    anomalies = anomalies
  ))
}

#' Detect outliers in numeric columns
#' @keywords internal
.vigiar_detectar_outliers <- function(dados, verbose = TRUE) {
  num_cols <- names(dados)[vapply(dados, is.numeric, logical(1))]
  outliers <- list()

  for (col in num_cols) {
    x <- dados[[col]]
    x <- x[!is.na(x)]
    if (length(x) < 10) next

    q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
    q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr

    n_low <- sum(x < lower, na.rm = TRUE)
    n_high <- sum(x > upper, na.rm = TRUE)

    if (n_low + n_high > 0) {
      outliers[[col]] <- list(
        n_low  = n_low,
        n_high = n_high,
        lower  = lower,
        upper  = upper,
        pct    = 100 * (n_low + n_high) / length(x)
      )
      if (verbose) {
        cli::cli_alert_warning(
          "Outliers em '{col}': {n_low + n_high} valores ({round(outliers[[col]]$pct, 1)}%)"
        )
      }
    }
  }

  if (verbose && length(outliers) == 0) {
    cli::cli_alert_success("Outliers: nenhum detectado (metodo IQR)")
  }

  outliers
}

# -- Checksum-based reproducibility --------------------------------------------

.vigiar_auditar_checksums <- function(dados, verbose) {
  # Compute deterministic checksums for key columns
  checksums <- list(
    nrow       = nrow(dados),
    ncol       = ncol(dados),
    col_names  = paste(sort(names(dados)), collapse = ", "),
    sha256     = .vigiar_data_checksum(dados)
  )

  if (verbose) {
    cli::cli_alert_info(
      "Checksum SHA256: {substr(checksums$sha256, 1, 16)}..."
    )
  }

  list(
    ok        = TRUE,
    checksums = checksums
  )
}

.VIGIAR_CANONICALIZATION_VERSION <- "2"

.vigiar_sha256_object <- function(x) {
  raw_hash <- openssl::sha256(serialize(x, NULL, version = 3L))
  paste(format(raw_hash), collapse = "")
}

.vigiar_canonical_number <- function(x) {
  out <- rep(NA_character_, length(x))
  out[is.na(x) & !is.nan(x)] <- "<NA>"
  out[is.nan(x)] <- "<NaN>"
  out[is.infinite(x) & x > 0] <- "<Inf>"
  out[is.infinite(x) & x < 0] <- "<-Inf>"
  finite <- is.finite(x)
  out[finite] <- sprintf("%.17g", as.numeric(x[finite]))
  out
}

.vigiar_canonical_column <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(list(family = "datetime_utc", values = .vigiar_canonical_number(as.numeric(x))))
  }
  if (inherits(x, "Date")) {
    return(list(family = "date", values = .vigiar_canonical_number(as.numeric(x))))
  }
  if (is.factor(x) || is.character(x)) {
    values <- enc2utf8(as.character(x))
    values[is.na(x)] <- "<NA>"
    return(list(family = "character", values = values))
  }
  if (is.integer(x) || is.numeric(x)) {
    return(list(family = "number", values = .vigiar_canonical_number(x)))
  }
  if (is.logical(x)) {
    values <- ifelse(is.na(x), "<NA>", ifelse(x, "TRUE", "FALSE"))
    return(list(family = "logical", values = values))
  }
  if (is.raw(x)) {
    return(list(family = "raw", values = paste(format(x), collapse = "")))
  }
  if (is.list(x)) {
    values <- vapply(x, function(value) .vigiar_sha256_object(value), character(1))
    return(list(family = "list", values = values))
  }
  values <- enc2utf8(as.character(x))
  values[is.na(x)] <- "<NA>"
  list(family = paste0("other:", typeof(x)), values = values)
}

.vigiar_canonical_table <- function(dados, mode = c("canonical", "ordered")) {
  mode <- match.arg(mode)
  dados <- as.data.frame(dados, stringsAsFactors = FALSE)
  column_order <- seq_along(dados)
  if (identical(mode, "canonical") && ncol(dados) > 0L) {
    column_order <- order(names(dados), column_order)
  }
  dados <- dados[column_order]
  columns <- lapply(dados, .vigiar_canonical_column)
  families <- vapply(columns, `[[`, character(1), "family")
  values <- lapply(columns, `[[`, "values")

  if (identical(mode, "canonical") && nrow(dados) > 1L) {
    row_hash <- vapply(seq_len(nrow(dados)), function(i) {
      .vigiar_sha256_object(lapply(values, `[`, i))
    }, character(1))
    row_order <- order(row_hash)
    values <- lapply(values, `[`, row_order)
  }

  list(
    canonicalization_version = .VIGIAR_CANONICALIZATION_VERSION,
    mode = mode,
    n_rows = nrow(dados),
    n_cols = ncol(dados),
    column_names = enc2utf8(names(dados)),
    column_families = families,
    columns = values
  )
}

#' Compute a versioned deterministic checksum for a data frame
#'
#' The default `canonical` mode represents semantic table identity: row and
#' column order are ignored, factors equal their character representation,
#' equivalent integer/double values match, timestamps are normalized to UTC,
#' and full double precision is retained. `ordered` mode preserves row and
#' column order. Non-semantic data-frame attributes are excluded and are hashed
#' separately in VIGIAR snapshots.
#'
#' @param dados A data frame.
#' @param mode Checksum identity mode, either `canonical` or `ordered`.
#' @return A SHA256 hex string.
#' @export
vigiar_checksum <- function(dados, mode = c("canonical", "ordered")) {
  mode <- match.arg(mode)
  .vigiar_data_checksum(dados, mode = mode)
}

.vigiar_data_checksum <- function(dados, mode = c("canonical", "ordered")) {
  mode <- match.arg(mode)
  .vigiar_sha256_object(.vigiar_canonical_table(dados, mode = mode))
}

.vigiar_schema_checksum <- function(dados) {
  schema <- lapply(dados, function(x) {
    list(class = class(x), typeof = typeof(x))
  })
  .vigiar_sha256_object(list(
    names = names(dados),
    schema = schema
  ))
}

.vigiar_metadata_checksum <- function(dados, metadata = list()) {
  attributes <- attributes(dados)
  attributes[c("names", "row.names", "class")] <- NULL
  .vigiar_sha256_object(list(
    canonicalization_version = .VIGIAR_CANONICALIZATION_VERSION,
    attributes = attributes,
    metadata = metadata
  ))
}

# -- Session info --------------------------------------------------------------

.vigiar_session_info <- function() {
  list(
    r_version   = R.version.string,
    platform    = R.version$platform,
    locale      = Sys.getlocale("LC_COLLATE"),
    timezone    = Sys.timezone(),
    packages    = c(
      httr2 = as.character(utils::packageVersion("httr2")),
      jsonlite = as.character(utils::packageVersion("jsonlite")),
      tibble = as.character(utils::packageVersion("tibble")),
      dplyr = as.character(utils::packageVersion("dplyr"))
    )
  )
}

#' Export audit report as JSON
#'
#' Serializes a \code{vigiar_audit} or \code{vigiar_compliance}
#' report to JSON for long-term archiving.
#'
#' @param audit An audit object.
#' @param caminho File path to write JSON.
#' @return Invisibly, the file path.
#' @export
vigiar_exportar_auditoria <- function(audit, caminho) {
  dir.create(dirname(caminho), showWarnings = FALSE, recursive = TRUE)
  json <- jsonlite::toJSON(audit, auto_unbox = TRUE, pretty = TRUE,
                            null = "null", force = TRUE)
  writeLines(json, caminho)
  cli::cli_alert_success("Auditoria exportada: {caminho}")
  invisible(caminho)
}
