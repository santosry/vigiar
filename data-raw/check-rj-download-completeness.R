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

tables <- strsplit(
  Sys.getenv(
    "VIGIAR_RJ_AUDIT_TABLES",
    unset = "df_anual,df_mensal,df_dias,df_dias_conama,pop"
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

vigiar_conectar()
on.exit(vigiar_desconectar(), add = TRUE)

audit <- vigiar_auditar_rj_online(
  tabelas = tables,
  salvar = TRUE,
  dir = file.path("data-raw", "rj-download-completeness-output"),
  require_complete = require_complete,
  timeout = 240
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
