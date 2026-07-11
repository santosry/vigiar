# Refresh the versioned IBGE municipality reference used by offline validation.

url <- "https://servicodados.ibge.gov.br/api/v1/localidades/municipios"
output <- file.path("inst", "extdata", "municipios_ibge_reference.csv")
metadata_output <- file.path(
  "inst", "extdata", "municipios_ibge_reference_metadata.json"
)

raw <- jsonlite::fromJSON(url)
uf <- raw$microrregiao$mesorregiao$UF
uf_fallback <- raw$`regiao-imediata`$`regiao-intermediaria`$UF
missing_uf <- is.na(uf$id)
uf$id[missing_uf] <- uf_fallback$id[missing_uf]
uf$sigla[missing_uf] <- uf_fallback$sigla[missing_uf]
reference <- data.frame(
  codigo_ibge_6 = as.integer(substr(as.character(raw$id), 1L, 6L)),
  codigo_ibge_7 = as.integer(raw$id),
  municipio = raw$nome,
  codigo_uf = as.integer(uf$id),
  sigla_uf = uf$sigla,
  stringsAsFactors = FALSE
)
reference <- reference[order(reference$codigo_ibge_7), ]

stopifnot(
  nrow(reference) >= 5570L,
  sum(reference$sigla_uf == "RJ", na.rm = TRUE) == 92L,
  !anyDuplicated(reference$codigo_ibge_6),
  !anyDuplicated(reference$codigo_ibge_7)
)

utils::write.csv(reference, output, row.names = FALSE, fileEncoding = "UTF-8")
checksum_connection <- file(output, open = "rb")
checksum <- paste(format(openssl::sha256(checksum_connection)), collapse = "")
close(checksum_connection)
metadata <- list(
  source_url = url,
  retrieved_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  n_municipalities = nrow(reference),
  n_rj_municipalities = sum(reference$sigla_uf == "RJ", na.rm = TRUE),
  sha256 = checksum
)
writeLines(
  jsonlite::toJSON(metadata, auto_unbox = TRUE, pretty = TRUE),
  metadata_output,
  useBytes = TRUE
)
