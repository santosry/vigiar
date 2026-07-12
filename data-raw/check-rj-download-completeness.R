# Manual online validation for Rio de Janeiro download completeness.
#
# This script intentionally requires internet access. It is run manually for a
# release and may also be used by the separate scheduled online canary workflow.
# Outputs are written under data-raw/rj-download-completeness-output/, which is
# ignored by Git. Set VIGIAR_VALIDATION_RELEASE to the release tag when the
# report should be archived with a formal package release.

if (file.exists("DESCRIPTION") && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".")
} else {
  library(vigiar)
}

tables <- strsplit(
  Sys.getenv(
    "VIGIAR_RJ_AUDIT_TABLES",
    unset = "df_muni,df_anual,df_mensal,df_dias,df_dias_conama,pop"
  ),
  ",",
  fixed = TRUE
)[[1]]
tables <- trimws(tables)
tables <- tables[nzchar(tables)]

require_complete <- identical(
  tolower(Sys.getenv("VIGIAR_REQUIRE_COMPLETE", unset = "false")),
  "true"
)

parse_integer_env <- function(name) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    return(NULL)
  }
  parsed <- suppressWarnings(as.integer(strsplit(value, ",", fixed = TRUE)[[1]]))
  if (anyNA(parsed)) {
    stop(name, " must be a comma-separated integer vector.")
  }
  parsed
}

expected_years <- parse_integer_env("VIGIAR_EXPECTED_YEARS")
expected_months <- parse_integer_env("VIGIAR_EXPECTED_MONTHS")
expected_start <- trimws(Sys.getenv("VIGIAR_EXPECTED_PERIOD_START", unset = ""))
expected_end <- trimws(Sys.getenv("VIGIAR_EXPECTED_PERIOD_END", unset = ""))
domain_supplied <- !is.null(expected_years) || !is.null(expected_months) ||
  nzchar(expected_start) || nzchar(expected_end)
expected_domains <- if (!domain_supplied) NULL else stats::setNames(
  lapply(tables, function(table) {
    compact <- list(
      anos_esperados = expected_years,
      meses_esperados = expected_months,
      periodo_inicio = if (nzchar(expected_start)) expected_start else NULL,
      periodo_fim = if (nzchar(expected_end)) expected_end else NULL,
      inferir_periodos = FALSE
    )
    compact[!vapply(compact, is.null, logical(1))]
  }),
  tables
)

if (require_complete && is.null(expected_domains)) {
  stop(
    "VIGIAR_REQUIRE_COMPLETE=true requires an explicit expected temporal ",
    "domain. Set VIGIAR_EXPECTED_YEARS or both ",
    "VIGIAR_EXPECTED_PERIOD_START and VIGIAR_EXPECTED_PERIOD_END."
  )
}

vigiar_conectar()
on.exit(vigiar_desconectar(), add = TRUE)

audit <- vigiar_auditar_rj_online(
  tabelas = tables,
  salvar = TRUE,
  dir = file.path("data-raw", "rj-download-completeness-output"),
  require_complete = require_complete,
  timeout = 240,
  dominios_esperados = expected_domains
)

print(audit[, c(
  "tabela",
  "n_rows",
  "n_cols",
  "checksum",
  "schema_hash",
  "completeness_grade",
  "n_municipios_presentes",
  "n_municipios_esperados",
  "n_incomplete_groups",
  "truncation_status",
  "schema_status",
  "verification_status",
  "overall_status",
  "possivel_truncamento",
  "conclusion"
)])

for (i in seq_len(nrow(audit))) {
  cat("\nAbsent municipalities for ", audit$tabela[[i]], ":\n", sep = "")
  missing <- audit$municipios_ausentes[[i]]
  if (nrow(missing) == 0) {
    cat("none\n")
  } else {
    print(missing)
  }
}

message(
  "\nValidation run archived under: ",
  attr(audit, "vigiar_audit_dir")
)
