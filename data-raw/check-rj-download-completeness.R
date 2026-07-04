# Manual online validation for Rio de Janeiro download completeness.
#
# This script intentionally requires internet access and is not run by CI.
# Outputs are written under data-raw/rj-download-completeness-output/, which is
# ignored by Git. Set VIGIAR_VALIDATION_RELEASE to the release tag when the
# report should be archived with a formal package release.

if (file.exists("DESCRIPTION") && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".")
} else {
  library(vigiar)
}

release_id <- Sys.getenv(
  "VIGIAR_VALIDATION_RELEASE",
  unset = paste0("vigiar-", utils::packageVersion("vigiar"))
)
run_id <- paste(release_id, format(Sys.time(), "%Y%m%d-%H%M%S"), sep = "-")
out_dir <- file.path("data-raw", "rj-download-completeness-output", run_id)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
manifest <- list()

write_report <- function(x, name) {
  path <- file.path(out_dir, name)
  list_cols <- vapply(x, is.list, logical(1))
  x[list_cols] <- lapply(x[list_cols], function(col) {
    vapply(col, paste, collapse = "; ", FUN.VALUE = character(1))
  })
  utils::write.csv(x, path, row.names = FALSE)
  message("Wrote: ", normalizePath(path, winslash = "/", mustWork = FALSE))
}

temporal_summary <- function(dados) {
  years <- integer(0)
  missing_years <- integer(0)
  month_gaps <- character(0)

  if ("ano" %in% names(dados)) {
    years <- sort(unique(as.integer(dados$ano)))
    years <- years[!is.na(years)]
    if (length(years) > 0) {
      missing_years <- setdiff(seq.int(min(years), max(years)), years)
    }
  }

  if (all(c("ano", "mes") %in% names(dados))) {
    year_month <- unique(data.frame(
      ano = as.integer(dados$ano),
      mes = as.integer(dados$mes)
    ))
    year_month <- year_month[!is.na(year_month$ano) & !is.na(year_month$mes), , drop = FALSE]
    if (nrow(year_month) > 0) {
      by_year <- split(year_month$mes, year_month$ano)
      month_gaps <- unlist(Map(function(year, months) {
        missing <- setdiff(1:12, sort(unique(months)))
        if (length(missing) == 0) {
          return(character(0))
        }
        paste0(year, ":", paste(missing, collapse = "|"))
      }, names(by_year), by_year), use.names = FALSE)
    }
  }

  data.frame(
    anos_presentes = paste(years, collapse = "; "),
    anos_ausentes = paste(missing_years, collapse = "; "),
    meses_ausentes_por_ano = paste(month_gaps, collapse = "; "),
    stringsAsFactors = FALSE
  )
}

print_absent <- function(dados, label, por = "geral") {
  missing <- vigiar_rj_municipios_ausentes(dados, por = por)
  message("\nAbsent municipalities for ", label, " (", por, "):")
  if (nrow(missing) == 0) {
    message("none")
  } else {
    print(missing)
  }
  invisible(missing)
}

manifest_row <- function(tab, dados, cobertura, completude = NULL, temporal = NULL) {
  table_grade <- if (!is.null(completude) && "grade" %in% names(completude)) {
    paste(unique(completude$grade), collapse = "; ")
  } else {
    NA_character_
  }
  n_groups <- if (!is.null(completude)) nrow(completude) else NA_integer_
  n_incomplete <- if (!is.null(completude) && "completo" %in% names(completude)) {
    sum(!completude$completo)
  } else {
    NA_integer_
  }
  temporal <- temporal %||% temporal_summary(dados)

  data.frame(
    run_id = run_id,
    release = release_id,
    checked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    tabela = tab,
    completeness_grade = table_grade,
    n_rows = nrow(dados),
    n_cols = ncol(dados),
    checksum = vigiar_checksum(dados),
    n_municipios_presentes = cobertura$n_municipios_presentes[[1]],
    n_municipios_esperados = cobertura$n_municipios_esperados[[1]],
    cobertura_pct = cobertura$cobertura_pct[[1]],
    n_ausentes = cobertura$n_ausentes[[1]],
    n_completeness_groups = n_groups,
    n_incomplete_groups = n_incomplete,
    complete_at_table_grain = identical(n_incomplete, 0L),
    anos_presentes = temporal$anos_presentes[[1]],
    anos_ausentes = temporal$anos_ausentes[[1]],
    meses_ausentes_por_ano = temporal$meses_ausentes_por_ano[[1]],
    possivel_truncamento = isTRUE(attr(dados, "vigiar_possivel_truncamento")),
    stringsAsFactors = FALSE
  )
}

vigiar_conectar()
on.exit(vigiar_desconectar(), add = TRUE)

tables <- c("df_muni", "df_anual", "df_mensal", "df_dias", "df_dias_conama")

for (tab in tables) {
  message("\nChecking ", tab, "...")
  should_process <- tab %in% c("df_anual", "df_mensal", "df_dias", "df_dias_conama")
  dados <- tryCatch(
    vigiar_baixar_rj(tab, validar_cobertura = TRUE, processar = should_process),
    error = function(e) {
      warning("Download failed for ", tab, ": ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )

  if (is.null(dados)) {
    next
  }

  cov_general <- vigiar_rj_cobertura(dados)
  write_report(cov_general, paste0(tab, "-coverage-general.csv"))
  temporal <- temporal_summary(dados)
  write_report(temporal, paste0(tab, "-temporal-gaps.csv"))
  cov_table <- tryCatch(
    vigiar_rj_completude_tabela(dados, tabela = tab),
    error = function(e) {
      warning("Completeness-by-table failed for ", tab, ": ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  if (!is.null(cov_table)) {
    write_report(cov_table, paste0(tab, "-coverage-table-grain.csv"))
  }

  manifest[[tab]] <- manifest_row(tab, dados, cov_general, cov_table, temporal)
  missing_general <- print_absent(dados, tab)
  write_report(missing_general, paste0(tab, "-missing-general.csv"))

  if ("ano" %in% names(dados)) {
    cov_year <- vigiar_rj_cobertura(dados, por = "ano")
    write_report(cov_year, paste0(tab, "-coverage-year.csv"))
    missing_year <- print_absent(dados, tab, por = "ano")
    write_report(missing_year, paste0(tab, "-missing-year.csv"))
  }

  if (all(c("ano", "mes") %in% names(dados))) {
    cov_year_month <- vigiar_rj_cobertura(dados, por = "ano_mes")
    write_report(cov_year_month, paste0(tab, "-coverage-year-month.csv"))
    missing_year_month <- print_absent(dados, tab, por = "ano_mes")
    write_report(missing_year_month, paste0(tab, "-missing-year-month.csv"))
  }

  saveRDS(dados, file.path(out_dir, paste0(tab, "-rj.rds")))
}

if (length(manifest) > 0) {
  write_report(do.call(rbind, manifest), "manifest.csv")
}

message("\nValidation run archived under: ", normalizePath(out_dir, winslash = "/", mustWork = FALSE))
