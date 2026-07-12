# Geographic reference and validation helpers

.VIGIAR_UF_CODES <- c(
  RO = 11L, AC = 12L, AM = 13L, RR = 14L, PA = 15L, AP = 16L, TO = 17L,
  MA = 21L, PI = 22L, CE = 23L, RN = 24L, PB = 25L, PE = 26L, AL = 27L,
  SE = 28L, BA = 29L, MG = 31L, ES = 32L, RJ = 33L, SP = 35L, PR = 41L,
  SC = 42L, RS = 43L, MS = 50L, MT = 51L, GO = 52L, DF = 53L
)

.VIGIAR_UF_NAMES <- c(
  RONDONIA = "RO", ACRE = "AC", AMAZONAS = "AM", RORAIMA = "RR",
  PARA = "PA", AMAPA = "AP", TOCANTINS = "TO", MARANHAO = "MA",
  PIAUI = "PI", CEARA = "CE", `RIO GRANDE DO NORTE` = "RN",
  PARAIBA = "PB", PERNAMBUCO = "PE", ALAGOAS = "AL", SERGIPE = "SE",
  BAHIA = "BA", `MINAS GERAIS` = "MG", `ESPIRITO SANTO` = "ES",
  `RIO DE JANEIRO` = "RJ", `SAO PAULO` = "SP", PARANA = "PR",
  `SANTA CATARINA` = "SC", `RIO GRANDE DO SUL` = "RS",
  `MATO GROSSO DO SUL` = "MS", `MATO GROSSO` = "MT", GOIAS = "GO",
  `DISTRITO FEDERAL` = "DF"
)

.vigiar_ibge_reference <- function() {
  cached <- .vigiar_env$ibge_municipalities %||% NULL
  if (!is.null(cached)) {
    return(cached)
  }
  path <- system.file(
    "extdata", "municipios_ibge_reference.csv", package = "vigiar"
  )
  if (!nzchar(path) || !file.exists(path)) {
    stop("Versioned IBGE municipality reference is unavailable.", call. = FALSE)
  }
  reference <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8",
    colClasses = c(
      codigo_ibge_6 = "integer",
      codigo_ibge_7 = "integer",
      municipio = "character",
      codigo_uf = "integer",
      sigla_uf = "character"
    )
  )
  .vigiar_env$ibge_municipalities <- reference
  reference
}

.vigiar_text_file_checksum <- function(path) {
  if (!file.exists(path)) {
    stop("Text file not found: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  payload <- charToRaw(enc2utf8(paste0(paste(lines, collapse = "\n"), "\n")))
  paste(format(openssl::sha256(payload)), collapse = "")
}

.vigiar_ascii_upper <- function(x) {
  converted <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  toupper(trimws(converted))
}

.vigiar_normalizar_uf <- function(x) {
  out <- rep(NA_character_, length(x))
  if (length(x) == 0L) {
    return(out)
  }
  value <- .vigiar_ascii_upper(x)
  missing <- is.na(x) | !nzchar(value)
  is_code <- !missing & grepl("^[0-9]{1,2}$", value)
  if (any(is_code)) {
    idx <- match(suppressWarnings(as.integer(value[is_code])), .VIGIAR_UF_CODES)
    out[is_code] <- names(.VIGIAR_UF_CODES)[idx]
  }
  is_label <- !missing & !is_code
  if (any(is_label)) {
    labels <- value[is_label]
    direct <- labels %in% names(.VIGIAR_UF_CODES)
    resolved <- rep(NA_character_, length(labels))
    resolved[direct] <- labels[direct]
    resolved[!direct] <- unname(.VIGIAR_UF_NAMES[labels[!direct]])
    out[is_label] <- resolved
  }
  out
}

.vigiar_filtrar_uf <- function(dados, uf) {
  target <- .vigiar_normalizar_uf(uf)
  if (length(target) != 1L || is.na(target)) {
    stop("'uf' must be one valid Brazilian UF code or abbreviation.", call. = FALSE)
  }
  col_uf <- .vigiar_coluna_uf(dados)
  before <- nrow(dados)
  if (!is.na(col_uf)) {
    observed <- .vigiar_normalizar_uf(dados[[col_uf]])
    out <- dados[!is.na(observed) & observed == target, , drop = FALSE]
    attr(out, "vigiar_uf_filter_column") <- col_uf
    attr(out, "vigiar_uf_filter") <- target
    return(tibble::as_tibble(out))
  }

  col_muni <- .vigiar_coluna_municipio(dados)
  if (!is.na(col_muni)) {
    validation <- vigiar_validar_codigo_municipio(dados[[col_muni]])
    observed <- validation$sigla_uf
    out <- dados[!is.na(observed) & observed == target, , drop = FALSE]
    attr(out, "vigiar_uf_filter_column") <- col_muni
    attr(out, "vigiar_uf_filter") <- target
    return(tibble::as_tibble(out))
  }

  warning("No UF or municipality code column was found; data were not filtered.",
          call. = FALSE)
  dados
}

#' Validate IBGE municipality codes
#'
#' Separates syntactic format, official existence, check-digit/reference
#' agreement, and optional UF membership. The versioned reference is generated
#' from the official IBGE Localities API and is used entirely offline at run
#' time.
#'
#' @param x Integer, numeric, or character municipality codes.
#' @param uf Optional UF code or abbreviation used to check membership.
#' @return A tibble with one validation row per input value.
#' @export
vigiar_validar_codigo_municipio <- function(x, uf = NULL) {
  input <- as.character(x)
  value <- trimws(input)
  value[is.na(x)] <- NA_character_
  value <- sub("\\.0+$", "", value)
  supplied <- !is.na(value) & nzchar(value)
  digits <- supplied & grepl("^[0-9]+$", value)
  width <- ifelse(digits, nchar(value), NA_integer_)
  formato_valido <- digits & width %in% c(6L, 7L)

  code6 <- rep(NA_integer_, length(x))
  code7_input <- rep(NA_integer_, length(x))
  six <- formato_valido & width == 6L
  seven <- formato_valido & width == 7L
  code6[six] <- suppressWarnings(as.integer(value[six]))
  code6[seven] <- suppressWarnings(as.integer(substr(value[seven], 1L, 6L)))
  code7_input[seven] <- suppressWarnings(as.integer(value[seven]))
  structural <- !is.na(code6) & code6 >= 110001L & code6 <= 530010L
  formato_valido <- formato_valido & structural

  reference <- .vigiar_ibge_reference()
  ref_index <- match(code6, reference$codigo_ibge_6)
  reference_exists <- !is.na(ref_index)
  official7 <- rep(NA_integer_, length(x))
  official7[reference_exists] <- reference$codigo_ibge_7[ref_index[reference_exists]]
  digito_valido <- rep(NA, length(x))
  digito_valido[seven] <- reference_exists[seven] &
    code7_input[seven] == official7[seven]
  exists <- formato_valido & reference_exists & (!seven | digito_valido)

  sigla <- rep(NA_character_, length(x))
  codigo_uf <- rep(NA_integer_, length(x))
  municipio <- rep(NA_character_, length(x))
  sigla[exists] <- reference$sigla_uf[ref_index[exists]]
  codigo_uf[exists] <- reference$codigo_uf[ref_index[exists]]
  municipio[exists] <- reference$municipio[ref_index[exists]]

  target <- NULL
  if (!is.null(uf)) {
    target <- .vigiar_normalizar_uf(uf)
    if (length(target) != 1L || is.na(target)) {
      stop("'uf' must be one valid Brazilian UF code or abbreviation.", call. = FALSE)
    }
  }
  pertence <- if (is.null(target)) {
    rep(NA, length(x))
  } else {
    !is.na(sigla) & sigla == target
  }

  status <- rep("fail", length(x))
  status[!supplied] <- "unknown"
  status[exists & (is.null(target) | pertence)] <- "pass"
  details <- ifelse(
    !supplied,
    "Municipality code is missing.",
    ifelse(
      !formato_valido,
      "Municipality code does not have a valid 6- or 7-digit format.",
      ifelse(
        seven & !is.na(digito_valido) & !digito_valido,
        "The 7-digit code does not match the official IBGE reference.",
        ifelse(
          !exists,
          "Municipality code does not exist in the versioned IBGE reference.",
          ifelse(!is.null(target) & !pertence,
                 paste0("Municipality does not belong to ", target, "."),
                 "Municipality code matches the official IBGE reference.")
        )
      )
    )
  )

  tibble::tibble(
    entrada = input,
    codigo_ibge_6 = code6,
    codigo_ibge_7 = ifelse(exists, official7, code7_input),
    formato_valido = formato_valido,
    digito_valido = digito_valido,
    existe = exists,
    codigo_uf = codigo_uf,
    sigla_uf = sigla,
    municipio = municipio,
    pertence_uf = pertence,
    status = status,
    details = details
  )
}
