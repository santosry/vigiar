# Verifica a consistencia de versao entre os artefatos de citacao do pacote.
#
# A versao canonica vive em DESCRIPTION (campo Version). Este script falha
# (status != 0) se CITATION.cff ou codemeta.json divergirem de DESCRIPTION,
# evitando citacoes de uma versao diferente da instalada.
#
# Usa apenas R base: nenhuma dependencia externa.

stopifnot(
  "DESCRIPTION nao encontrado" = file.exists("DESCRIPTION"),
  "CITATION.cff nao encontrado" = file.exists("CITATION.cff"),
  "codemeta.json nao encontrado" = file.exists("codemeta.json")
)

read_version <- function(path, pattern) {
  lines <- readLines(path, warn = FALSE)
  hit <- grep(pattern, lines, value = TRUE)
  if (length(hit) == 0) {
    stop(sprintf("Padrao de versao nao encontrado em %s", path))
  }
  m <- regmatches(hit[1], regexec(pattern, hit[1]))[[1]]
  if (length(m) < 2 || is.na(m[2])) {
    stop(sprintf("Regex de versao falhou em %s", path))
  }
  m[2]
}

desc_version <- unname(read.dcf("DESCRIPTION", fields = "Version")[1, 1])
cff_version  <- read_version(
  "CITATION.cff",
  '^version:\\s*"?([0-9]+\\.[0-9]+\\.[0-9]+)'
)
cm_version   <- read_version(
  "codemeta.json",
  '"version"\\s*:\\s*"([0-9]+\\.[0-9]+\\.[0-9]+)"'
)

versions <- c(
  DESCRIPTION  = desc_version,
  CITATION_cff = cff_version,
  codemeta_json = cm_version
)

if (length(unique(versions)) != 1L) {
  cat("Versoes divergentes entre os artefatos:\n")
  print(versions)
  quit(status = 1L)
}

cat("Versoes consistentes:", unique(versions), "\n")
