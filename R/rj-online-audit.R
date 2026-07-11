# Package: vigiar
# Online RJ audit reports for scientific validation

#' Audit online RJ downloads for completeness and traceability
#'
#' Downloads one or more VIGIAR tables scoped to Rio de Janeiro, computes RJ
#' municipality coverage at the table-specific panel grain, and optionally saves
#' reproducible audit artifacts that can be attached to a scientific report.
#'
#' The audit distinguishes a successful download from a complete RJ panel. For
#' example, \code{df_mensal} is assessed as municipality x year x month, while
#' \code{df_anual} and \code{pop} are assessed as municipality x year.
#'
#' @param tabelas Character vector of VIGIAR table names.
#' @param salvar If \code{TRUE}, write CSV, JSON, and RDS audit artifacts.
#' @param dir Output directory for audit artifacts.
#' @param require_complete If \code{TRUE}, error when any table is incomplete,
#'   truncated, lacks municipality codes, has no schema hash, or fails.
#' @param timeout Timeout in seconds for each download.
#' @param timestamp Timestamp used in the audit records and output directory.
#' @param dominios_esperados Optional named list with one entry per table. Each
#'   entry may provide `anos_esperados`, `meses_esperados`, `periodo_inicio`,
#'   `periodo_fim`, or `inferir_periodos` to define the expected temporal domain.
#' @return A tibble with scalar audit fields and list-columns containing
#'   coverage details, table-grain completeness, and missing municipalities.
#' @export
vigiar_auditar_rj_online <- function(
  tabelas = c("df_anual", "df_mensal", "df_dias", "pop"),
  salvar = TRUE,
  dir = "data-raw/rj-download-completeness-output",
  require_complete = FALSE,
  timeout = 120,
  timestamp = Sys.time(),
  dominios_esperados = NULL
) {
  if (length(tabelas) == 0) {
    stop("'tabelas' must contain at least one table name.", call. = FALSE)
  }

  timestamp <- as.POSIXct(timestamp)
  checked_at <- .vigiar_rj_audit_timestamp(timestamp)
  package_version <- as.character(utils::packageVersion("vigiar"))
  git_sha <- .vigiar_git_sha()
  if (!is.null(dominios_esperados) &&
      (!is.list(dominios_esperados) || is.null(names(dominios_esperados)))) {
    stop("'dominios_esperados' must be a named list by table.", call. = FALSE)
  }

  rows <- lapply(unique(as.character(tabelas)), function(tab) {
    .vigiar_auditar_rj_tabela_online(
      tabela = tab,
      checked_at = checked_at,
      package_version = package_version,
      git_sha = git_sha,
      timeout = timeout,
      dominio_esperado = dominios_esperados[[tab]] %||% list()
    )
  })
  audit <- dplyr::bind_rows(rows)

  if (isTRUE(salvar)) {
    out_dir <- .vigiar_rj_write_audit_artifacts(audit, dir, timestamp)
    attr(audit, "vigiar_audit_dir") <- out_dir
    attr(audit, "vigiar_audit_timestamp") <- checked_at
  }

  if (isTRUE(require_complete)) {
    .vigiar_rj_require_complete_audit(audit)
  }

  audit
}

.vigiar_auditar_rj_tabela_online <- function(tabela, checked_at,
                                             package_version, git_sha,
                                             timeout, dominio_esperado = list()) {
  tryCatch(
    {
      processar <- tabela %in% c(
        "df_anual", "df_mensal", "df_dias", "df_dias_conama", "pop"
      )
      dados <- vigiar_baixar_rj(
        tabela,
        validar_cobertura = FALSE,
        require_complete = FALSE,
        processar = processar,
        timeout = timeout
      )
      .vigiar_rj_audit_success_row(
        tabela = tabela,
        checked_at = checked_at,
        package_version = package_version,
        git_sha = git_sha,
        dados = dados,
        dominio_esperado = dominio_esperado
      )
    },
    error = function(e) {
      .vigiar_rj_audit_failed_row(
        tabela = tabela,
        checked_at = checked_at,
        package_version = package_version,
        git_sha = git_sha,
        erro = conditionMessage(e)
      )
    }
  )
}

.vigiar_rj_audit_success_row <- function(tabela, checked_at, package_version,
                                         git_sha, dados,
                                         dominio_esperado = list()) {
  truncation_status <- attr(dados, "vigiar_truncation_status") %||% "unknown"
  truncation_evidence <- attr(dados, "vigiar_truncation_evidence") %||% character()
  dados <- tibble::as_tibble(dados)
  col_muni <- .vigiar_coluna_municipio(dados)
  has_municipality <- !is.na(col_muni)
  schema_hash <- attr(dados, "vigiar_schema_hash") %||% .vigiar_schema_hash(tabela)
  schema_hash <- as.character(schema_hash)
  schema_assessment <- .vigiar_rj_schema_assessment(tabela)
  possivel_truncamento <- isTRUE(attr(dados, "vigiar_possivel_truncamento"))
  checksum <- as.character(vigiar_checksum(dados))
  temporal <- .vigiar_rj_temporal_summary(dados)

  cobertura_geral <- suppressWarnings(vigiar_rj_cobertura(
    dados,
    por = "geral",
    exigir_coluna_municipio = has_municipality
  ))
  cobertura_ano <- .vigiar_rj_optional_coverage(dados, "ano", has_municipality)
  cobertura_ano_mes <- .vigiar_rj_optional_coverage(
    dados,
    "ano_mes",
    has_municipality && all(c("ano", "mes") %in% names(dados))
  )
  ausentes <- suppressWarnings(vigiar_rj_municipios_ausentes(dados))

  allowed_domain_args <- c(
    "anos_esperados", "meses_esperados", "periodo_inicio", "periodo_fim",
    "inferir_periodos"
  )
  unknown_domain_args <- setdiff(names(dominio_esperado), allowed_domain_args)
  if (length(unknown_domain_args) > 0L) {
    stop(
      "Unknown expected-domain argument(s) for table '", tabela, "': ",
      paste(unknown_domain_args, collapse = ", "),
      call. = FALSE
    )
  }
  completude <- tryCatch(
    do.call(vigiar_rj_completude_tabela, c(
      list(dados = dados, tabela = tabela),
      dominio_esperado
    )),
    error = function(e) e
  )
  erro <- NA_character_
  if (inherits(completude, "error")) {
    erro <- conditionMessage(completude)
    completude <- tibble::tibble()
  }

  if (!has_municipality) {
    erro <- paste(
      "Municipality code column not found;",
      "RJ completeness cannot be evaluated."
    )
  }

  macro_incomplete <- .vigiar_rj_macro_incomplete(cobertura_geral)
  campos_presente <- .vigiar_rj_campos_presente(dados, has_municipality)
  conclusion <- .vigiar_rj_audit_conclusion(
    has_municipality = has_municipality,
    schema_hash = schema_hash,
    completude = completude,
    possivel_truncamento = possivel_truncamento,
    erro = erro,
    truncation_status = truncation_status,
    schema_status = schema_assessment$status
  )
  spatial_status <- if (
    nrow(cobertura_geral) > 0L && all(cobertura_geral$completo)
  ) "complete" else "incomplete"
  panel_status <- unique(completude$panel_status %||% "unknown")[[1]]
  temporal_status <- unique(
    completude$temporal_domain_status %||% "unknown"
  )[[1]]
  verification_status <- unique(
    completude$verification_status %||% "unverified"
  )[[1]]
  overall_status <- unique(completude$overall_status %||% "unknown")[[1]]
  schema_status <- if (is.na(schema_hash) || !nzchar(schema_hash)) {
    "unknown"
  } else {
    schema_assessment$status
  }
  response_status <- if (truncation_status == "no_evidence") {
    "unverified"
  } else {
    truncation_status
  }

  tibble::tibble(
    checked_at = checked_at,
    tabela = tabela,
    package_version = package_version,
    git_sha = git_sha,
    schema_hash = schema_hash,
    checksum = checksum,
    n_rows = nrow(dados),
    n_cols = ncol(dados),
    completeness_grade = paste(unique(completude$grade %||% NA_character_),
                               collapse = "; "),
    n_completeness_groups = nrow(completude),
    n_incomplete_groups = if (nrow(completude) > 0) {
      sum(!completude$completo)
    } else {
      NA_integer_
    },
    n_municipios_presentes = cobertura_geral$n_municipios_presentes[[1]],
    n_municipios_esperados = cobertura_geral$n_municipios_esperados[[1]],
    cobertura_pct = cobertura_geral$cobertura_pct[[1]],
    n_municipios_ausentes = cobertura_geral$n_ausentes[[1]],
    anos_disponiveis = temporal$anos_disponiveis,
    anos_ausentes = temporal$anos_ausentes,
    meses_disponiveis = temporal$meses_disponiveis,
    meses_ausentes_por_ano = temporal$meses_ausentes_por_ano,
    campos_dos_goytacazes_presente = campos_presente,
    possivel_truncamento = possivel_truncamento,
    truncation_status = truncation_status,
    truncation_evidence = paste(truncation_evidence, collapse = "; "),
    spatial_coverage_status = spatial_status,
    temporal_domain_status = temporal_status,
    panel_completeness_status = panel_status,
    schema_status = schema_status,
    schema_details = paste(schema_assessment$details, collapse = "; "),
    response_completeness_status = response_status,
    verification_status = verification_status,
    overall_status = overall_status,
    conclusion = conclusion,
    erro = erro,
    cobertura_geral = list(cobertura_geral),
    cobertura_ano = list(cobertura_ano),
    cobertura_ano_mes = list(cobertura_ano_mes),
    completude_tabela = list(completude),
    municipios_ausentes = list(ausentes),
    macrorregioes_incompletas = list(macro_incomplete)
  )
}

.vigiar_rj_audit_failed_row <- function(tabela, checked_at, package_version,
                                        git_sha, erro) {
  tibble::tibble(
    checked_at = checked_at,
    tabela = tabela,
    package_version = package_version,
    git_sha = git_sha,
    schema_hash = NA_character_,
    checksum = NA_character_,
    n_rows = NA_integer_,
    n_cols = NA_integer_,
    completeness_grade = NA_character_,
    n_completeness_groups = NA_integer_,
    n_incomplete_groups = NA_integer_,
    n_municipios_presentes = NA_integer_,
    n_municipios_esperados = 92L,
    cobertura_pct = NA_real_,
    n_municipios_ausentes = NA_integer_,
    anos_disponiveis = NA_character_,
    anos_ausentes = NA_character_,
    meses_disponiveis = NA_character_,
    meses_ausentes_por_ano = NA_character_,
    campos_dos_goytacazes_presente = FALSE,
    possivel_truncamento = NA,
    truncation_status = "unknown",
    truncation_evidence = NA_character_,
    spatial_coverage_status = "unknown",
    temporal_domain_status = "unknown",
    panel_completeness_status = "unknown",
    schema_status = "unknown",
    schema_details = NA_character_,
    response_completeness_status = "unknown",
    verification_status = "failed",
    overall_status = "fail",
    conclusion = "failed",
    erro = erro,
    cobertura_geral = list(tibble::tibble()),
    cobertura_ano = list(tibble::tibble()),
    cobertura_ano_mes = list(tibble::tibble()),
    completude_tabela = list(tibble::tibble()),
    municipios_ausentes = list(tibble::tibble()),
    macrorregioes_incompletas = list(character(0))
  )
}

.vigiar_rj_audit_conclusion <- function(has_municipality, schema_hash,
                                        completude, possivel_truncamento,
                                        erro, truncation_status = "unknown",
                                        schema_status = "unknown") {
  if (!is.na(erro)) {
    if (!has_municipality) {
      return("failed")
    }
    return("schema_changed")
  }
  if (is.na(schema_hash) || !nzchar(schema_hash)) {
    return("schema_unverified")
  }
  if (identical(schema_status, "fail")) {
    return("schema_changed")
  }
  if (!identical(schema_status, "pass")) {
    return("schema_unverified")
  }
  truncated <- truncation_status %in% c("possible", "probable", "confirmed") ||
    isTRUE(possivel_truncamento) ||
    any(completude$possivel_truncamento %||% FALSE)
  if (truncated) {
    return("truncated")
  }
  if (nrow(completude) == 0 || any(!completude$completo)) {
    return("partial")
  }
  overall <- unique(completude$overall_status %||% "unknown")[[1]]
  if (identical(overall, "pass")) {
    return("complete")
  }
  verification <- unique(
    completude$verification_status %||% "complete_within_observed_domain"
  )[[1]]
  verification
}

.vigiar_rj_schema_assessment <- function(tabela) {
  path <- system.file(
    "extdata", "vigiar_schema_critical_lock.json", package = "vigiar"
  )
  if (!nzchar(path) || !file.exists(path) || is.null(.vigiar_env$esquema)) {
    return(.vigiar_check_result(
      "unknown",
      details = "Critical schema lock or live schema is unavailable."
    ))
  }
  lock <- tryCatch(
    vigiar_esquema_carregar_lock(path),
    error = function(e) NULL
  )
  if (is.null(lock) || !tabela %in% lock$tabelas) {
    return(.vigiar_check_result(
      "unknown",
      details = "Table is not covered by the critical schema lock."
    ))
  }
  live <- .vigiar_env$esquema[[tabela]]
  if (is.null(live)) {
    return(.vigiar_check_result(
      "fail",
      details = "Table is absent from the live Power BI schema."
    ))
  }
  locked <- lock$esquema[[tabela]] %||% list()
  missing <- setdiff(names(locked), names(live))
  changed <- vapply(intersect(names(locked), names(live)), function(column) {
    !identical(
      .vigiar_schema_column_type(locked[[column]]),
      .vigiar_schema_column_type(live[[column]])
    )
  }, logical(1))
  changed <- names(changed)[changed]
  details <- character()
  if (length(missing) > 0L) {
    details <- c(details, paste0(
      "Missing critical columns: ", paste(missing, collapse = ", "), "."
    ))
  }
  if (length(changed) > 0L) {
    details <- c(details, paste0(
      "Changed critical column types: ", paste(changed, collapse = ", "), "."
    ))
  }
  .vigiar_check_result(
    if (length(details) == 0L) "pass" else "fail",
    details = if (length(details) == 0L) {
      "Critical schema columns match the versioned lock."
    } else {
      details
    }
  )
}

.vigiar_rj_require_complete_audit <- function(audit) {
  bad <- audit[!audit$conclusion %in% "complete", , drop = FALSE]
  if (nrow(bad) == 0) {
    return(invisible(TRUE))
  }

  details <- sprintf(
    "%s=%s%s",
    bad$tabela,
    bad$conclusion,
    ifelse(is.na(bad$erro), "", paste0(" (", bad$erro, ")"))
  )
  stop(
    "RJ online audit is not complete. require_complete=TRUE requires ",
    "all audited tables to have complete 92-municipality coverage at the ",
    "expected table grain, no API truncation, municipality codes, and a ",
    "stable schema hash, and an explicit expected temporal domain. ",
    "Provide 'dominios_esperados' for temporal tables. Failing tables: ",
    paste(details, collapse = "; "),
    call. = FALSE
  )
}

.vigiar_rj_optional_coverage <- function(dados, por, available) {
  if (!isTRUE(available)) {
    return(tibble::tibble())
  }
  suppressWarnings(vigiar_rj_cobertura(
    dados,
    por = por,
    exigir_coluna_municipio = TRUE
  ))
}

.vigiar_rj_temporal_summary <- function(dados) {
  years <- integer(0)
  missing_years <- integer(0)
  months <- integer(0)
  month_gaps <- character(0)

  if ("ano" %in% names(dados)) {
    years <- sort(unique(as.integer(dados$ano)))
    years <- years[!is.na(years)]
    if (length(years) > 0) {
      missing_years <- setdiff(seq.int(min(years), max(years)), years)
    }
  }

  if (all(c("ano", "mes") %in% names(dados))) {
    months <- sort(unique(as.integer(dados$mes)))
    months <- months[!is.na(months)]
    year_month <- unique(data.frame(
      ano = as.integer(dados$ano),
      mes = as.integer(dados$mes)
    ))
    year_month <- year_month[
      !is.na(year_month$ano) & !is.na(year_month$mes),
      ,
      drop = FALSE
    ]
    if (nrow(year_month) > 0) {
      by_year <- split(year_month$mes, year_month$ano)
      month_gaps <- unlist(Map(function(year, year_months) {
        missing <- setdiff(1:12, sort(unique(year_months)))
        if (length(missing) == 0) {
          return(character(0))
        }
        paste0(year, ":", paste(missing, collapse = "|"))
      }, names(by_year), by_year), use.names = FALSE)
    }
  }

  list(
    anos_disponiveis = paste(years, collapse = "; "),
    anos_ausentes = paste(missing_years, collapse = "; "),
    meses_disponiveis = paste(months, collapse = "; "),
    meses_ausentes_por_ano = paste(month_gaps, collapse = "; ")
  )
}

.vigiar_rj_macro_incomplete <- function(cobertura) {
  has_macro <- "macrorregioes_incompletas" %in% names(cobertura)
  if (!is.data.frame(cobertura) || nrow(cobertura) == 0 || !has_macro) {
    return(character(0))
  }
  sort(unique(unlist(cobertura$macrorregioes_incompletas, use.names = FALSE)))
}

.vigiar_rj_campos_presente <- function(dados, has_municipality) {
  if (!isTRUE(has_municipality)) {
    return(FALSE)
  }
  col_muni <- .vigiar_coluna_municipio(dados)
  codes <- .vigiar_normalizar_codigo_municipio(dados[[col_muni]])
  330100L %in% codes
}

.vigiar_rj_write_audit_artifacts <- function(audit, dir, timestamp) {
  out_dir <- .vigiar_rj_audit_output_dir(dir, timestamp)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
  attr(audit, "vigiar_audit_dir") <- out_dir
  attr(audit, "vigiar_audit_timestamp") <- .vigiar_rj_audit_timestamp(timestamp)

  manifest <- .vigiar_rj_flatten_audit(audit)
  utils::write.csv(
    manifest,
    file.path(out_dir, "manifest.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  jsonlite::write_json(
    .vigiar_rj_audit_json(audit),
    file.path(out_dir, "manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
  saveRDS(audit, file.path(out_dir, "audit.rds"))

  for (i in seq_len(nrow(audit))) {
    tab <- audit$tabela[[i]]
    .vigiar_rj_write_optional_csv(
      audit$cobertura_geral[[i]],
      out_dir,
      paste0(tab, "-coverage-general.csv")
    )
    .vigiar_rj_write_optional_csv(
      audit$cobertura_ano[[i]],
      out_dir,
      paste0(tab, "-coverage-year.csv")
    )
    .vigiar_rj_write_optional_csv(
      audit$cobertura_ano_mes[[i]],
      out_dir,
      paste0(tab, "-coverage-year-month.csv")
    )
    .vigiar_rj_write_optional_csv(
      audit$completude_tabela[[i]],
      out_dir,
      paste0(tab, "-coverage-table-grain.csv")
    )
    .vigiar_rj_write_optional_csv(
      audit$municipios_ausentes[[i]],
      out_dir,
      paste0(tab, "-missing-municipalities.csv")
    )
    .vigiar_rj_write_optional_csv(
      data.frame(
        macrorregiao_saude = audit$macrorregioes_incompletas[[i]],
        stringsAsFactors = FALSE
      ),
      out_dir,
      paste0(tab, "-incomplete-macroregions.csv")
    )
  }

  out_dir
}

.vigiar_rj_audit_output_dir <- function(dir, timestamp) {
  root <- dir
  if (!grepl("^([A-Za-z]:|/|\\\\\\\\)", root)) {
    candidates <- unique(c(getwd(), dirname(getwd()), dirname(dirname(getwd()))))
    package_root <- candidates[file.exists(file.path(candidates, "DESCRIPTION"))][1]
    if (!is.na(package_root)) {
      root <- file.path(package_root, root)
    }
  }

  release_id <- Sys.getenv(
    "VIGIAR_VALIDATION_RELEASE",
    unset = paste0("vigiar-", utils::packageVersion("vigiar"))
  )
  file.path(
    root,
    paste(release_id, format(timestamp, "%Y%m%d-%H%M%S"), sep = "-")
  )
}

.vigiar_rj_write_optional_csv <- function(x, out_dir, filename) {
  x <- .vigiar_rj_flatten_df(x)
  utils::write.csv(
    x,
    file.path(out_dir, filename),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

.vigiar_rj_flatten_audit <- function(audit) {
  .vigiar_rj_flatten_df(audit)
}

.vigiar_rj_flatten_df <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (nm in names(x)) {
    if (is.list(x[[nm]])) {
      x[[nm]] <- vapply(x[[nm]], .vigiar_rj_flatten_value, character(1))
    }
  }
  x
}

.vigiar_rj_flatten_value <- function(value) {
  if (is.data.frame(value)) {
    return(sprintf("<%d rows x %d cols>", nrow(value), ncol(value)))
  }
  if (length(value) == 0) {
    return("")
  }
  paste(as.character(value), collapse = "; ")
}

.vigiar_rj_audit_json <- function(audit) {
  list(
    generated_at = if (nrow(audit) > 0) audit$checked_at[[1]] else NA_character_,
    package_version = if (nrow(audit) > 0) audit$package_version[[1]] else NA_character_,
    git_sha = if (nrow(audit) > 0) audit$git_sha[[1]] else NA_character_,
    tables = lapply(seq_len(nrow(audit)), function(i) {
      list(
        summary = as.list(.vigiar_rj_flatten_audit(audit[i, , drop = FALSE])),
        cobertura_geral = .vigiar_rj_df_records(audit$cobertura_geral[[i]]),
        cobertura_ano = .vigiar_rj_df_records(audit$cobertura_ano[[i]]),
        cobertura_ano_mes = .vigiar_rj_df_records(audit$cobertura_ano_mes[[i]]),
        completude_tabela = .vigiar_rj_df_records(audit$completude_tabela[[i]]),
        municipios_ausentes = .vigiar_rj_df_records(audit$municipios_ausentes[[i]]),
        macrorregioes_incompletas = audit$macrorregioes_incompletas[[i]]
      )
    })
  )
}

.vigiar_rj_df_records <- function(x) {
  if (!is.data.frame(x) || nrow(x) == 0) {
    return(list())
  }
  lapply(seq_len(nrow(x)), function(i) {
    row <- lapply(names(x), function(nm) {
      value <- x[[nm]][[i]]
      if (is.factor(value)) {
        value <- as.character(value)
      }
      value
    })
    names(row) <- names(x)
    row
  })
}

.vigiar_rj_audit_timestamp <- function(timestamp) {
  format(as.POSIXct(timestamp), "%Y-%m-%dT%H:%M:%S%z")
}

.vigiar_git_sha <- function() {
  env_sha <- Sys.getenv("GITHUB_SHA", unset = NA_character_)
  if (!is.na(env_sha) && nzchar(env_sha)) {
    return(env_sha)
  }
  out <- tryCatch(
    system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(e) NA_character_,
    warning = function(w) NA_character_
  )
  if (length(out) == 0 || is.na(out[[1]]) || !nzchar(out[[1]])) {
    return(NA_character_)
  }
  out[[1]]
}
