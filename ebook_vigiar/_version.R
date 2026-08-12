# _version.R
#
# Fonte unica da string de versao e do BibTeX citados no e-book.
# A versao canonica vive em DESCRIPTION (campo Version). Este script e
# sourced pelos capitulos que precisam citar o pacote, evitando versoes
# hardcoded divergentes.

vigiar_version <- tryCatch(
  as.character(utils::packageVersion("vigiar")),
  error = function(e) {
    # Fallback para renderizacao sem uma copia instalada do pacote.
    for (p in c("../DESCRIPTION", "DESCRIPTION", "../../DESCRIPTION")) {
      if (file.exists(p)) {
        d <- read.dcf(p, fields = "Version")
        return(unname(d[1, 1]))
      }
    }
    "0.0.0"  # nunca deve ocorrer em builds normais
  }
)

vigiar_bibtex <- paste0(
  "@Manual{santos2026vigiar,\n",
  "  title = {vigiar: VIGIAR Environmental Health Data for Rio de Janeiro},\n",
  "  author = {Ryan Santos},\n",
  "  year = {2026},\n",
  "  note = {R package version ", vigiar_version, "},\n",
  "  url = {https://github.com/santosry/vigiar},\n",
  "}\n"
)
