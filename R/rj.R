# Package: vigiar
# Rio de Janeiro state registry, completeness checks, and RJ downloads
#
# Municipality codes use the 6-digit IBGE code as the package standard.
# The registry also stores the official 7-digit IBGE code for interoperability.
# Reference fixtures in inst/extdata lock the IBGE municipality list and the
# SES-RJ health-region names used by this registry.

# ---- RJ Municipality Registry ------------------------------------------------

RJ_MUNICIPIOS <- data.frame(
  codigo_ibge = c(
    330010, 330015, 330020, 330022, 330023, 330025, 330030, 330205,
    330040, 330045, 330050, 330060, 330070, 330080, 330090, 330100,
    330110, 330093, 330115, 330120, 330130, 330095, 330140, 330150,
    330160, 330170, 330180, 330185, 330187, 330190, 330200, 330210,
    330220, 330225, 330227, 330230, 330240, 330245, 330250, 330260,
    330270, 330280, 330285, 330290, 330300, 330310, 330320, 330330,
    330340, 330350, 330360, 330370, 330380, 330385, 330390, 330395,
    330400, 330410, 330411, 330412, 330414, 330415, 330420, 330430,
    330440, 330450, 330452, 330455, 330460, 330470, 330480, 330475,
    330490, 330500, 330510, 330513, 330515, 330520, 330530, 330540,
    330550, 330555, 330560, 330570, 330575, 330580, 330590, 330600,
    330610, 330615, 330620, 330630),
  codigo_ibge_7 = c(
    3300100, 3300159, 3300209, 3300225, 3300233, 3300258, 3300308, 3302056,
    3300407, 3300456, 3300506, 3300605, 3300704, 3300803, 3300902, 3301009,
    3301108, 3300936, 3301157, 3301207, 3301306, 3300951, 3301405, 3301504,
    3301603, 3301702, 3301801, 3301850, 3301876, 3301900, 3302007, 3302106,
    3302205, 3302254, 3302270, 3302304, 3302403, 3302452, 3302502, 3302601,
    3302700, 3302809, 3302858, 3302908, 3303005, 3303104, 3303203, 3303302,
    3303401, 3303500, 3303609, 3303708, 3303807, 3303856, 3303906, 3303955,
    3304003, 3304102, 3304110, 3304128, 3304144, 3304151, 3304201, 3304300,
    3304409, 3304508, 3304524, 3304557, 3304607, 3304706, 3304805, 3304755,
    3304904, 3305000, 3305109, 3305133, 3305158, 3305208, 3305307, 3305406,
    3305505, 3305554, 3305604, 3305703, 3305752, 3305802, 3305901, 3306008,
    3306107, 3306156, 3306206, 3306305),
  municipio = c(
    "Angra dos Reis", "Aperibe", "Araruama", "Areal",
    "Armacao dos Buzios", "Arraial do Cabo", "Barra do Pirai", "Italva",
    "Barra Mansa", "Belford Roxo", "Bom Jardim", "Bom Jesus do Itabapoana",
    "Cabo Frio", "Cachoeiras de Macacu", "Cambuci", "Campos dos Goytacazes",
    "Cantagalo", "Carapebus", "Cardoso Moreira", "Carmo",
    "Casimiro de Abreu", "Comendador Levy Gasparian", "Conceicao de Macabu", "Cordeiro",
    "Duas Barras", "Duque de Caxias", "Engenheiro Paulo de Frontin", "Guapimirim",
    "Iguaba Grande", "Itaborai", "Itaguai", "Itaocara",
    "Itaperuna", "Itatiaia", "Japeri", "Laje do Muriae",
    "Macae", "Macuco", "Mage", "Mangaratiba",
    "Marica", "Mendes", "Mesquita", "Miguel Pereira",
    "Miracema", "Natividade", "Nilopolis", "Niteroi",
    "Nova Friburgo", "Nova Iguacu", "Paracambi", "Paraiba do Sul",
    "Paraty", "Paty do Alferes", "Petropolis", "Pinheiral",
    "Pirai", "Porciuncula", "Porto Real", "Quatis",
    "Queimados", "Quissama", "Resende", "Rio Bonito",
    "Rio Claro", "Rio das Flores", "Rio das Ostras", "Rio de Janeiro",
    "Santa Maria Madalena", "Santo Antonio de Padua", "Sao Fidelis", "Sao Francisco de Itabapoana",
    "Sao Goncalo", "Sao Joao da Barra", "Sao Joao de Meriti", "Sao Jose de Uba",
    "Sao Jose do Vale do Rio Preto", "Sao Pedro da Aldeia", "Sao Sebastiao do Alto", "Sapucaia",
    "Saquarema", "Seropedica", "Silva Jardim", "Sumidouro",
    "Tangua", "Teresopolis", "Trajano de Moraes", "Tres Rios",
    "Valenca", "Varre-Sai", "Vassouras", "Volta Redonda"),
  regiao_saude = c(
    "Baia da Ilha Grande", "Noroeste", "Baixada Litoranea", "Centro-Sul",
    "Baixada Litoranea", "Baixada Litoranea", "Medio Paraiba", "Noroeste",
    "Medio Paraiba", "Metropolitana I", "Serrana", "Noroeste",
    "Baixada Litoranea", "Metropolitana II", "Noroeste", "Norte",
    "Serrana", "Norte", "Norte", "Serrana",
    "Baixada Litoranea", "Centro-Sul", "Norte", "Serrana",
    "Serrana", "Metropolitana I", "Centro-Sul", "Metropolitana II",
    "Baixada Litoranea", "Metropolitana II", "Metropolitana I", "Noroeste",
    "Noroeste", "Medio Paraiba", "Metropolitana I", "Noroeste",
    "Norte", "Serrana", "Metropolitana I", "Baia da Ilha Grande",
    "Metropolitana II", "Centro-Sul", "Metropolitana I", "Centro-Sul",
    "Noroeste", "Noroeste", "Metropolitana I", "Metropolitana II",
    "Serrana", "Metropolitana I", "Metropolitana I", "Centro-Sul",
    "Baia da Ilha Grande", "Centro-Sul", "Serrana", "Medio Paraiba",
    "Medio Paraiba", "Noroeste", "Medio Paraiba", "Medio Paraiba",
    "Metropolitana I", "Norte", "Medio Paraiba", "Metropolitana II",
    "Medio Paraiba", "Medio Paraiba", "Norte", "Metropolitana I",
    "Serrana", "Noroeste", "Norte", "Norte",
    "Metropolitana II", "Norte", "Metropolitana I", "Noroeste",
    "Serrana", "Baixada Litoranea", "Serrana", "Centro-Sul",
    "Baixada Litoranea", "Metropolitana I", "Metropolitana II", "Serrana",
    "Metropolitana II", "Serrana", "Serrana", "Centro-Sul",
    "Medio Paraiba", "Noroeste", "Centro-Sul", "Medio Paraiba"),
  stringsAsFactors = FALSE
)

RJ_MUNICIPIOS$codigo_ibge_6 <- RJ_MUNICIPIOS$codigo_ibge
RJ_MUNICIPIOS$macrorregiao_saude <- RJ_MUNICIPIOS$regiao_saude
RJ_MUNICIPIOS <- RJ_MUNICIPIOS[
  c("codigo_ibge", "codigo_ibge_6", "codigo_ibge_7", "municipio",
    "regiao_saude", "macrorregiao_saude")
]

RJ_CODIGOS_VALIDOS <- c(
  "RJ", "rj", "Rio de Janeiro", "RIO DE JANEIRO"
)

RJ_MUNI_RANGE <- c(330010L, 330630L)

# ---- RJ-specific functions ---------------------------------------------------

#' List all 92 Rio de Janeiro municipalities
#'
#' The package standard is the 6-digit IBGE municipality code
#' (`codigo_ibge` / `codigo_ibge_6`). The 7-digit official IBGE code is also
#' returned as `codigo_ibge_7` for joins with sources that use the check digit.
#'
#' @return A tibble with municipality codes, municipality names, the canonical
#'   SES-RJ `regiao_saude`, the deprecated compatibility alias
#'   `macrorregiao_saude`, and source metadata attributes.
#' @export
vigiar_rj_municipios <- function() {
  out <- tibble::as_tibble(RJ_MUNICIPIOS)
  attr(out, "vigiar_rj_sources") <- system.file(
    "extdata", "rj_official_sources.csv", package = "vigiar"
  )
  out
}

#' List Rio de Janeiro health macro-regions
#'
#' @return Character vector of the 9 macro-regions.
#' @export
vigiar_rj_macrorregioes <- function() {
  sort(unique(RJ_MUNICIPIOS$macrorregiao_saude))
}

#' List Rio de Janeiro health regions
#'
#' @return Character vector of health regions.
#' @export
vigiar_rj_regioes_saude <- function() {
  sort(unique(RJ_MUNICIPIOS$regiao_saude))
}

#' Summarise Rio de Janeiro VIGIAR data
#'
#' @param dados A processed VIGIAR tibble.
#' @param agregacao One of "municipio", "macrorregiao", or "regiao_saude".
#' @return A tibble with summary statistics.
#' @export
vigiar_rj_resumo <- function(dados, agregacao = c("municipio", "macrorregiao", "regiao_saude")) {
  agregacao <- match.arg(agregacao)
  dados_rj <- .vigiar_filtrar_rj(dados, validar = FALSE)

  if (nrow(dados_rj) == 0) {
    warning("No Rio de Janeiro municipality was found in the data.", call. = FALSE)
    return(tibble::tibble())
  }

  merged <- merge(
    dados_rj,
    RJ_MUNICIPIOS,
    by.x = "codigo_ibge_6",
    by.y = "codigo_ibge_6",
    all.x = TRUE,
    suffixes = c("", "_rj")
  )

  grp <- switch(agregacao,
    municipio = "municipio",
    macrorregiao = "macrorregiao_saude",
    regiao_saude = "regiao_saude"
  )

  num_cols <- names(merged)[vapply(merged, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, c("codigo_ibge", "codigo_ibge_6", "codigo_ibge_7",
                                  "cod_municipio", "ano", "mes"))

  if (length(num_cols) == 0) {
    return(tibble::as_tibble(merged))
  }

  result <- merged |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
    dplyr::summarise(
      n_municipios = dplyr::n_distinct(.data[["codigo_ibge_6"]]),
      dplyr::across(
        dplyr::all_of(num_cols),
        list(
          mean = ~mean(.x, na.rm = TRUE),
          sd = ~stats::sd(.x, na.rm = TRUE),
          n = ~sum(!is.na(.x))
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )

  tibble::as_tibble(result)
}

#' Aggregate Rio de Janeiro data by health region
#'
#' @param dados A processed VIGIAR tibble with municipality codes.
#' @param agregacao "macrorregiao" or "regiao_saude".
#' @return A tibble with aggregated data.
#' @export
vigiar_rj_series <- function(dados, agregacao = c("macrorregiao", "regiao_saude")) {
  agregacao <- match.arg(agregacao)
  vigiar_rj_resumo(dados, agregacao = agregacao)
}

#' Validate that data contains only RJ municipalities
#'
#' Checks municipality codes, flags non-RJ municipalities, and reports missing
#' Rio de Janeiro municipalities using the internal 92-municipality registry.
#'
#' @param dados A data frame with a municipality code column.
#' @param col_muni Name of the municipality code column (auto-detected).
#' @return Invisibly, a list with validation results.
#' @export
vigiar_validar_rj <- function(dados, col_muni = NULL) {
  col_muni <- col_muni %||% .vigiar_coluna_municipio(dados)
  if (is.na(col_muni)) {
    stop("Municipality code column not found.", call. = FALSE)
  }

  raw_codes <- dados[[col_muni]]
  validation <- vigiar_validar_codigo_municipio(raw_codes, uf = "RJ")
  raw_chr <- trimws(as.character(raw_codes))
  supplied <- !is.na(raw_codes) & nzchar(raw_chr)
  invalid_idx <- supplied & (!validation$formato_valido | !validation$existe)
  invalidos <- unique(raw_chr[invalid_idx])
  codigos <- unique(validation$codigo_ibge_6[validation$existe])
  codigos <- codigos[!is.na(codigos)]

  rj_codes <- RJ_MUNICIPIOS$codigo_ibge_6
  in_rj <- sort(intersect(codigos, rj_codes))
  fora_rj <- sort(unique(validation$codigo_ibge_6[
    validation$existe & !validation$pertence_uf
  ]))
  faltantes <- sort(setdiff(rj_codes, in_rj))
  ok <- length(in_rj) > 0L && length(fora_rj) == 0L && length(invalidos) == 0L
  details <- character()
  if (length(in_rj) == 0L) {
    details <- c(details, "No valid Rio de Janeiro municipality code was found.")
  }
  if (length(fora_rj) > 0L) {
    details <- c(details, sprintf(
      "%d municipality code(s) do not belong to Rio de Janeiro: %s",
      length(fora_rj), paste(utils::head(fora_rj, 10), collapse = ", ")
    ))
  }
  if (length(invalidos) > 0L) {
    details <- c(details, sprintf(
      "%d municipality code value(s) could not be normalized: %s",
      length(invalidos), paste(utils::head(invalidos, 10), collapse = ", ")
    ))
  }

  result <- c(.vigiar_check_result(
    if (ok) "pass" else "fail",
    details = details
  ), list(
    n_total = length(codigos),
    n_rj = length(in_rj),
    n_fora_rj = length(fora_rj),
    n_invalidos = length(invalidos),
    codigos_fora_rj = fora_rj,
    codigos_invalidos = invalidos,
    municipios_rj_faltantes = faltantes,
    valido = ok
  ))

  if (!result$ok && length(details) > 0L) {
    warning(paste(details, collapse = " "), call. = FALSE)
  }

  if (length(faltantes) > 0) {
    message(sprintf(
      "%d Rio de Janeiro municipalities are absent from the data.",
      length(faltantes)
    ))
  } else {
    message("OK: all 92 Rio de Janeiro municipalities are present.")
  }

  invisible(result)
}

#' Download Rio de Janeiro VIGIAR data with completeness metadata
#'
#' Downloads one VIGIAR table, filters Rio de Janeiro with the internal
#' 92-municipality registry, normalizes municipality codes, and attaches
#' coverage and truncation metadata to the returned tibble.
#'
#' @param tabela Table name.
#' @param colunas Optional character vector of column names.
#' @param ordenar_por Optional column used to sort the Power BI query.
#' @param limite Optional row limit passed to the Power BI query.
#' @param timeout Timeout in seconds for the HTTP request.
#' @param validar_cobertura If \code{TRUE}, report RJ coverage after download.
#' @param exigir_completo If \code{TRUE}, error unless all 92 municipalities
#'   are present.
#' @param require_complete English alias for \code{exigir_completo}. When
#'   \code{TRUE}, possible API truncation is also an error.
#' @param processar If \code{TRUE}, process the table after filtering.
#' @param tipo Optional processor type, used mainly for PM2.5 tables.
#' @param usar_cache If \code{TRUE}, reuse a local RJ-specific cache entry.
#' @param cache_max_age Maximum cache age in seconds. Defaults to one day.
#' @param snapshot If \code{TRUE}, attach a \code{vigiar_snapshot} attribute.
#' @param ... Additional arguments passed to \code{vigiar_baixar()}.
#' @return A tibble containing RJ-only data and RJ coverage attributes.
#' @export
vigiar_baixar_rj <- function(
  tabela,
  colunas = NULL,
  ordenar_por = NULL,
  limite = NULL,
  timeout = 120,
  validar_cobertura = TRUE,
  exigir_completo = FALSE,
  require_complete = exigir_completo,
  processar = FALSE,
  tipo = NULL,
  usar_cache = FALSE,
  cache_max_age = 86400,
  snapshot = FALSE,
  ...
) {
  if (is.null(.vigiar_env$sessao)) {
    stop("No active session. Run vigiar_conectar() first.", call. = FALSE)
  }
  .vigiar_check_tabela(tabela)
  complete_required <- isTRUE(exigir_completo) || isTRUE(require_complete)

  dots <- list(...)
  if (!is.null(dots$cache)) {
    usar_cache <- isTRUE(dots$cache)
    dots$cache <- NULL
    warning("Argument 'cache' is deprecated for vigiar_baixar_rj(); use 'usar_cache'.",
            call. = FALSE)
  }
  dots$uf <- NULL
  dots$strategy <- NULL
  user_filters <- dots$filtros
  dots$filtros <- NULL
  filtros <- .vigiar_combinar_filtros(
    user_filters,
    .vigiar_filtro_servidor_rj(tabela)
  )
  schema_hash <- .vigiar_schema_hash(tabela)
  cache_file <- NULL

  if (isTRUE(usar_cache)) {
    cache_file <- .vigiar_rj_cache_file(
      tabela = tabela,
      colunas = colunas,
      ordenar_por = ordenar_por,
      limite = limite,
      schema_hash = schema_hash,
      dots = c(dots, list(filtros = filtros))
    )
    if (file.exists(cache_file)) {
      age <- as.numeric(difftime(
        Sys.time(), file.info(cache_file)$mtime, units = "secs"
      ))
      cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
      valid <- inherits(cached, "vigiar_rj_cached_data") &&
        age <= as.numeric(cache_max_age) &&
        identical(cached$schema_hash, schema_hash) &&
        tryCatch(
          identical(cached$checksum, vigiar_checksum(cached$dados)),
          error = function(e) FALSE
        )
      if (isTRUE(valid)) {
        cli::cli_alert_success("RJ cache hit: {tabela}")
        .vigiar_log("INFO", "RJ cache hit", table = tabela,
                    metadata = list(age_seconds = age), event = "cache_hit")
        out <- cached$dados
        if (isTRUE(complete_required)) {
          cache_truncation <- attr(out, "vigiar_truncation_status") %||% "unknown"
          if (cache_truncation != "no_evidence" ||
                isTRUE(attr(out, "vigiar_possivel_truncamento"))) {
            stop(
              "The cached RJ response has truncation evidence; complete data cannot be guaranteed.",
              call. = FALSE
            )
          }
          if (!identical(attr(out, "vigiar_parser_status"), "pass")) {
            stop(
              "The cached RJ response was not structurally verified by the parser; complete data cannot be guaranteed.",
              call. = FALSE
            )
          }
          cache_coverage <- suppressWarnings(vigiar_rj_cobertura(out))
          if (!isTRUE(cache_coverage$completo[[1]])) {
            stop(
              sprintf(
                "Cached RJ coverage is incomplete: %d/92 municipalities present.",
                cache_coverage$n_municipios_presentes[[1]]
              ),
              call. = FALSE
            )
          }
          if (tabela %in% c(
            "df_anual", "df_mensal", "df_dias", "df_dias_conama", "pop"
          )) {
            cache_completeness <- suppressWarnings(
              attr(out, "vigiar_rj_completude") %||%
                vigiar_rj_completude_tabela(out, tabela = tabela)
            )
            if (any(cache_completeness$completo %in% FALSE)) {
              stop(
                "The cached RJ table has incomplete municipality-time groups.",
                call. = FALSE
              )
            }
          }
        }
        attr(out, "vigiar_cache_status") <- "hit"
        attr(out, "vigiar_cache_age_seconds") <- age
        return(out)
      }
      .vigiar_log("WARN", "RJ cache entry expired or failed validation",
                  table = tabela, metadata = list(age_seconds = age),
                  event = "cache_invalid")
    }
    .vigiar_log(
      "INFO", "RJ cache miss", table = tabela,
      metadata = list(cache_file = basename(cache_file)), event = "cache_miss"
    )
  }

  args <- c(
    list(
      tabela = tabela,
      colunas = colunas,
      ordenar_por = ordenar_por,
      limite = limite,
      timeout = timeout,
      uf = NULL,
      filtros = filtros
    ),
    dots
  )

  dados <- do.call(vigiar_baixar, args)
  parser_status <- attr(dados, "vigiar_parser_status") %||% "unknown"
  parser_issues <- as.character(
    attr(dados, "vigiar_parser_issues") %||% character()
  )
  dados <- .vigiar_detectar_truncamento(dados, tabela = tabela, limite = limite)
  possivel_truncamento <- isTRUE(attr(dados, "vigiar_possivel_truncamento"))
  if (isTRUE(possivel_truncamento) && isTRUE(complete_required)) {
    stop(
      "Possible API truncation was detected; complete RJ data cannot be guaranteed. ",
      "Use a validated partitioned download before running scientific analyses.",
      call. = FALSE
    )
  }
  if (isTRUE(complete_required) && !identical(parser_status, "pass")) {
    stop(
      "Power BI response parsing was not structurally verified; complete RJ data cannot be guaranteed. ",
      if (length(parser_issues) > 0L) paste(parser_issues, collapse = "; ") else
        "No parser verification status was available.",
      call. = FALSE
    )
  }

  dados_rj <- .vigiar_filtrar_rj(dados, validar = FALSE)
  has_municipality <- !is.na(.vigiar_coluna_municipio(dados_rj))
  if (!has_municipality) {
    msg <- sprintf(
      "Table '%s' has no municipality code column; RJ 92-municipality completeness cannot be evaluated.",
      tabela
    )
    if (isTRUE(validar_cobertura) || isTRUE(complete_required)) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
  }

  cobertura <- vigiar_rj_cobertura(
    dados_rj,
    por = "geral",
    exigir_coluna_municipio = has_municipality
  )

  if (isTRUE(validar_cobertura)) {
    .vigiar_emitir_cobertura_rj(cobertura)
  }

  completo <- isTRUE(cobertura$completo[[1]])
  if (isTRUE(complete_required) && !completo) {
    ausentes <- cobertura$municipios_ausentes[[1]]
    stop(
      sprintf(
        "RJ coverage is incomplete: %d/92 municipalities present. Missing: %s",
        cobertura$n_municipios_presentes[[1]],
        paste(utils::head(ausentes, 20), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  table_completeness <- NULL
  if (tabela %in% c(
    "df_anual", "df_mensal", "df_dias", "df_dias_conama", "pop"
  )) {
    table_completeness <- suppressWarnings(vigiar_rj_completude_tabela(
      dados_rj,
      tabela = tabela,
      require_complete = FALSE
    ))
    if (isTRUE(complete_required) &&
          any(table_completeness$completo %in% FALSE)) {
      stop(
        sprintf(
          "RJ table coverage is incomplete for '%s': %d municipality-time group(s) failed.",
          tabela, sum(table_completeness$completo %in% FALSE)
        ),
        call. = FALSE
      )
    }
  }

  if (isTRUE(processar)) {
    dados_rj <- .vigiar_processar_tabela_rj(dados_rj, tabela = tabela, tipo = tipo)
  }

  dados_rj <- .vigiar_anexar_metadados_rj(
    dados_rj,
    tabela = tabela,
    cobertura = cobertura,
    possivel_truncamento = possivel_truncamento,
    schema_hash = schema_hash
  )
  attr(dados_rj, "vigiar_truncation_status") <-
    attr(dados, "vigiar_truncation_status") %||% "unknown"
  attr(dados_rj, "vigiar_truncation_evidence") <-
    attr(dados, "vigiar_truncation_evidence") %||% character()
  attr(dados_rj, "vigiar_truncation_assessment") <-
    attr(dados, "vigiar_truncation_assessment") %||% list()
  attr(dados_rj, "vigiar_parser_status") <- parser_status
  attr(dados_rj, "vigiar_parser_issues") <- parser_issues
  if (!is.null(table_completeness)) {
    attr(dados_rj, "vigiar_rj_completude") <- table_completeness
  }

  if (isTRUE(snapshot)) {
    attr(dados_rj, "vigiar_snapshot") <- vigiar_snapshot(dados = dados_rj, tabela = tabela)
  }

  if (isTRUE(usar_cache) && !is.null(cache_file)) {
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    cached <- list(
      dados = dados_rj,
      timestamp = Sys.time(),
      checksum = vigiar_checksum(dados_rj),
      schema_hash = schema_hash,
      package_version = as.character(utils::packageVersion("vigiar")),
      cache_key_version = .VIGIAR_CACHE_KEY_VERSION
    )
    class(cached) <- "vigiar_rj_cached_data"
    saveRDS(cached, cache_file)
    attr(dados_rj, "vigiar_cache_status") <- "miss"
    attr(dados_rj, "vigiar_cache_age_seconds") <- 0
    cli::cli_alert_success("RJ cache saved: {tabela}")
  }

  out <- .vigiar_as_tibble_preserve(dados_rj)
  attr(out, "vigiar_parser_status") <- parser_status
  attr(out, "vigiar_parser_issues") <- parser_issues
  out
}

#' Download one Rio de Janeiro municipality
#'
#' Downloads an RJ-scoped VIGIAR table and returns rows for a single
#' municipality, identified only by IBGE code. This avoids fragile filters based
#' on municipality names.
#'
#' @param tabela Table name.
#' @param codigo_ibge 6- or 7-digit IBGE municipality code.
#' @param colunas Optional character vector of column names.
#' @param ordenar_por Optional column used to sort the Power BI query.
#' @param limite Optional row limit passed to the Power BI query.
#' @param timeout Timeout in seconds.
#' @param exigir_dados If \code{TRUE}, error when the municipality has no rows.
#' @param require_complete If \code{TRUE}, possible API truncation is an error.
#' @param processar If \code{TRUE}, process the table after filtering.
#' @param tipo Optional processor type.
#' @param usar_cache If \code{TRUE}, reuse the RJ download cache.
#' @param snapshot If \code{TRUE}, attach a \code{vigiar_snapshot} attribute.
#' @param ... Additional arguments passed to \code{vigiar_baixar_rj()}.
#' @return A tibble for one RJ municipality with municipality metadata.
#' @export
vigiar_baixar_municipio <- function(
  tabela,
  codigo_ibge,
  colunas = NULL,
  ordenar_por = NULL,
  limite = NULL,
  timeout = 120,
  exigir_dados = FALSE,
  require_complete = FALSE,
  processar = FALSE,
  tipo = NULL,
  usar_cache = FALSE,
  snapshot = FALSE,
  ...
) {
  codigo6 <- .vigiar_normalizar_codigo_municipio(codigo_ibge, formato = "6")
  if (length(codigo6) != 1L || is.na(codigo6)) {
    stop("codigo_ibge must be one valid 6- or 7-digit IBGE municipality code.",
         call. = FALSE)
  }
  if (!codigo6 %in% RJ_MUNICIPIOS$codigo_ibge_6) {
    stop("codigo_ibge does not identify a Rio de Janeiro municipality.",
         call. = FALSE)
  }

  dots <- list(...)
  municipality_filter <- .vigiar_filtro_servidor_municipio(tabela, codigo6)
  dots$filtros <- .vigiar_combinar_filtros(
    dots$filtros,
    municipality_filter
  )
  args <- c(list(
    tabela = tabela,
    colunas = colunas,
    ordenar_por = ordenar_por,
    limite = limite,
    timeout = timeout,
    validar_cobertura = FALSE,
    exigir_completo = FALSE,
    require_complete = FALSE,
    processar = processar,
    tipo = tipo,
    usar_cache = usar_cache,
    snapshot = FALSE
  ), dots)
  dados_rj <- do.call(vigiar_baixar_rj, args)
  if (is.null(municipality_filter)) {
    warning(
      "The table schema has no validated municipality field for a server-side ",
      "filter; the municipality was subset from the upstream RJ response.",
      call. = FALSE
    )
  }

  if (isTRUE(require_complete) &&
        isTRUE(attr(dados_rj, "vigiar_possivel_truncamento"))) {
    stop(
      "Possible API truncation was detected; complete municipality data cannot be guaranteed.",
      call. = FALSE
    )
  }
  if (isTRUE(require_complete) &&
        !identical(attr(dados_rj, "vigiar_parser_status"), "pass")) {
    stop(
      "Power BI response parsing was not structurally verified; complete municipality data cannot be guaranteed.",
      call. = FALSE
    )
  }

  out <- dados_rj[dados_rj$codigo_ibge_6 == codigo6, , drop = FALSE]
  reg <- RJ_MUNICIPIOS[RJ_MUNICIPIOS$codigo_ibge_6 == codigo6, ]

  if (nrow(out) == 0) {
    msg <- sprintf(
      "No rows were returned for municipality %s (%s) in table '%s'.",
      reg$municipio[[1]], codigo6, tabela
    )
    if (isTRUE(exigir_dados)) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
  }

  attr(out, "vigiar_tabela") <- tabela
  attr(out, "vigiar_uf") <- "RJ"
  attr(out, "vigiar_codigo_ibge_6") <- codigo6
  attr(out, "vigiar_codigo_ibge_7") <- reg$codigo_ibge_7[[1]]
  attr(out, "vigiar_municipio") <- reg$municipio[[1]]
  attr(out, "vigiar_macrorregiao_saude") <- reg$macrorregiao_saude[[1]]
  attr(out, "vigiar_regiao_saude") <- reg$regiao_saude[[1]]
  attr(out, "vigiar_municipio_presente") <- nrow(out) > 0
  attr(out, "vigiar_municipio_linhas") <- nrow(out)
  attr(out, "vigiar_municipality_server_filter") <- municipality_filter
  attr(out, "vigiar_query_strategy") <- if (is.null(municipality_filter)) {
    "local_subset_from_rj_response"
  } else {
    "server_side_municipality_filter_with_local_verification"
  }
  attr(out, "vigiar_possivel_truncamento") <- isTRUE(attr(dados_rj, "vigiar_possivel_truncamento"))
  attr(out, "vigiar_parser_status") <- attr(dados_rj, "vigiar_parser_status") %||% "unknown"
  attr(out, "vigiar_parser_issues") <-
    attr(dados_rj, "vigiar_parser_issues") %||% character()
  attr(out, "vigiar_download_timestamp") <- Sys.time()

  if (isTRUE(snapshot)) {
    attr(out, "vigiar_snapshot") <- vigiar_snapshot(dados = out, tabela = tabela)
  }

  tibble::as_tibble(out)
}

#' Download Rio de Janeiro VIGIAR data using smaller partitions
#'
#' Uses validated Power BI server-side filters to download smaller partitions,
#' records every partition outcome, combines successful responses, removes
#' exact duplicates, and verifies the final RJ panel. Failed partitions and
#' any truncation evidence remain explicit.
#'
#' @param tabela Table name.
#' @param por Partitioning strategy.
#' @param anos Optional years to download.
#' @param meses Optional months to download.
#' @param municipios Optional municipality codes to download.
#' @param timeout Timeout in seconds.
#' @param delay Delay between partitions.
#' @param tentativas Maximum partition-level attempts. The HTTP layer also
#'   retains its own transient-error retry policy.
#' @param validar_cobertura If \code{TRUE}, validate final RJ coverage.
#' @param exigir_completo If \code{TRUE}, error unless all expected RJ
#'   municipalities are present in the final result.
#' @param require_complete English alias for \code{exigir_completo}.
#' @param processar Process the combined table.
#' @param tipo Optional processor type.
#' @param snapshot Attach a reproducible snapshot to the combined result.
#' @param ... Additional arguments passed to \code{vigiar_baixar_rj()}.
#' @return A tibble with partition, parser, truncation, schema, and completeness
#'   metadata.
#' @export
vigiar_baixar_rj_completo <- function(
  tabela,
  por = c("auto", "ano", "mes", "municipio"),
  anos = NULL,
  meses = NULL,
  municipios = NULL,
  timeout = 120,
  delay = 0.5,
  tentativas = 2L,
  validar_cobertura = TRUE,
  exigir_completo = FALSE,
  require_complete = exigir_completo,
  processar = FALSE,
  tipo = NULL,
  snapshot = FALSE,
  ...
) {
  por <- match.arg(por)
  complete_required <- isTRUE(exigir_completo) || isTRUE(require_complete)
  if (length(delay) != 1L || !is.finite(delay) || delay < 0) {
    stop("'delay' must be one non-negative finite number.", call. = FALSE)
  }
  tentativas <- suppressWarnings(as.integer(tentativas))
  if (length(tentativas) != 1L || is.na(tentativas) || tentativas < 1L) {
    stop("'tentativas' must be one positive integer.", call. = FALSE)
  }

  if (por == "auto") {
    por <- if (!is.null(municipios)) {
      "municipio"
    } else if (!is.null(meses)) {
      "mes"
    } else if (!is.null(anos)) {
      "ano"
    } else {
      "auto"
    }
  }

  if (por == "auto") {
    if (isTRUE(complete_required) && tabela %in% c(
      "df_anual", "df_mensal", "df_dias", "df_dias_conama", "pop"
    )) {
      stop(
        "require_complete = TRUE needs an explicit partition domain in 'anos' ",
        "or 'municipios'; a single observed response cannot define completeness.",
        call. = FALSE
      )
    }
    dados <- vigiar_baixar_rj(
      tabela = tabela,
      timeout = timeout,
      validar_cobertura = validar_cobertura,
      exigir_completo = exigir_completo,
      require_complete = require_complete,
      processar = processar,
      tipo = tipo,
      snapshot = snapshot,
      ...
    )
    attr(dados, "vigiar_query_strategy") <- "single_request_unverified_domain"
    return(dados)
  }

  plan <- .vigiar_rj_partition_plan(
    tabela = tabela,
    por = por,
    anos = anos,
    meses = meses,
    municipios = municipios
  )
  dots <- list(...)
  user_filters <- dots$filtros
  dots$filtros <- NULL
  results <- vector("list", length(plan))
  reports <- vector("list", length(plan))

  for (i in seq_along(plan)) {
    spec <- plan[[i]]
    partition_filters <- .vigiar_combinar_filtros(
      user_filters, spec$filtros
    )
    value <- NULL
    error_message <- NA_character_
    attempts_used <- 0L
    for (attempt in seq_len(tentativas)) {
      attempts_used <- attempt
      args <- c(list(
        tabela = tabela,
        timeout = timeout,
        validar_cobertura = FALSE,
        exigir_completo = FALSE,
        require_complete = FALSE,
        processar = processar,
        tipo = tipo,
        usar_cache = FALSE,
        snapshot = FALSE,
        filtros = partition_filters
      ), dots)
      value <- tryCatch(
        do.call(vigiar_baixar_rj, args),
        error = function(e) {
          error_message <<- conditionMessage(e)
          NULL
        }
      )
      if (!is.null(value)) {
        error_message <- NA_character_
        break
      }
      if (attempt < tentativas && delay > 0) {
        Sys.sleep(delay)
      }
    }
    results[[i]] <- value
    reports[[i]] <- tibble::tibble(
      partition = spec$label,
      ano = spec$ano,
      mes = spec$mes,
      codigo_ibge_6 = spec$codigo_ibge_6,
      status = if (is.null(value)) "failed" else "success",
      attempts = attempts_used,
      n_rows = if (is.null(value)) 0L else nrow(value),
      checksum = if (is.null(value)) NA_character_ else
        as.character(vigiar_checksum(value)),
      parser_status = if (is.null(value)) NA_character_ else
        attr(value, "vigiar_parser_status") %||% "unknown",
      truncation_status = if (is.null(value)) NA_character_ else
        attr(value, "vigiar_truncation_status") %||% "unknown",
      error = error_message
    )
    if (i < length(plan) && delay > 0) {
      Sys.sleep(delay)
    }
  }

  report <- dplyr::bind_rows(reports)
  failed <- report$status == "failed"
  if (all(failed)) {
    stop(
      "All RJ download partitions failed: ",
      paste(unique(stats::na.omit(report$error)), collapse = "; "),
      call. = FALSE
    )
  }
  successful <- results[!vapply(results, is.null, logical(1))]
  first <- successful[[1]]
  combined <- dplyr::bind_rows(lapply(successful, as.data.frame))
  combined <- combined[!duplicated(combined), , drop = FALSE]
  combined <- tibble::as_tibble(combined)
  combined <- .vigiar_restore_data_attributes(
    combined, .vigiar_data_attributes(first)
  )
  custom_classes <- setdiff(
    class(first), c("tbl_df", "tbl", "data.frame")
  )
  class(combined) <- unique(c(custom_classes, class(combined)))

  parser_statuses <- report$parser_status[!failed]
  parser_status <- if (all(parser_statuses == "pass")) {
    "pass"
  } else if (any(parser_statuses == "issues")) {
    "issues"
  } else {
    "unknown"
  }
  truncation_status <- .vigiar_worst_truncation(
    report$truncation_status[!failed]
  )
  attr(combined, "vigiar_parser_status") <- parser_status
  attr(combined, "vigiar_truncation_status") <- truncation_status
  attr(combined, "vigiar_possivel_truncamento") <-
    truncation_status != "no_evidence"
  attr(combined, "vigiar_response_rows") <- sum(report$n_rows[!failed])
  attr(combined, "vigiar_returned_rows") <- nrow(combined)
  attr(combined, "vigiar_query_strategy") <- paste0(
    "server_side_partitioned_by_", por
  )
  attr(combined, "vigiar_partition_report") <- report
  attr(combined, "vigiar_failed_partitions") <- report[failed, , drop = FALSE]
  attr(combined, "vigiar_download_timestamp") <- Sys.time()

  coverage <- vigiar_rj_cobertura(combined, por = "geral")
  if (isTRUE(validar_cobertura)) {
    .vigiar_emitir_cobertura_rj(coverage)
  }
  expected_months <- if (tabela %in% c(
    "df_mensal", "df_dias", "df_dias_conama"
  )) meses %||% 1:12 else NULL
  completeness <- suppressWarnings(vigiar_rj_completude_tabela(
    combined,
    tabela = tabela,
    anos_esperados = anos,
    meses_esperados = expected_months
  ))
  attr(combined, "vigiar_rj_completude") <- completeness
  attr(combined, "vigiar_rj_cobertura") <- coverage
  schema <- .vigiar_rj_schema_assessment(tabela)
  attr(combined, "vigiar_schema_status") <- schema$status

  if (isTRUE(complete_required)) {
    reasons <- character()
    if (any(failed)) {
      reasons <- c(reasons, sprintf("%d partition(s) failed", sum(failed)))
    }
    if (!identical(parser_status, "pass")) {
      reasons <- c(reasons, paste0("parser status is ", parser_status))
    }
    if (!identical(truncation_status, "no_evidence")) {
      reasons <- c(reasons, paste0("truncation status is ", truncation_status))
    }
    if (!identical(schema$status, "pass")) {
      reasons <- c(reasons, paste0("critical schema status is ", schema$status))
    }
    if (any(completeness$completo %in% FALSE)) {
      reasons <- c(reasons, sprintf(
        "%d expected panel group(s) are incomplete",
        sum(completeness$completo %in% FALSE)
      ))
    }
    if (!identical(unique(completeness$overall_status)[[1]], "pass")) {
      reasons <- c(reasons, paste0(
        "completeness status is ", unique(completeness$overall_status)[[1]]
      ))
    }
    if (length(reasons) > 0L) {
      stop(
        "Partitioned RJ download is not verified complete: ",
        paste(unique(reasons), collapse = "; "), ".",
        call. = FALSE
      )
    }
  }

  attr(combined, "vigiar_verification_status") <- if (
    !any(failed) && identical(parser_status, "pass") &&
      identical(truncation_status, "no_evidence") &&
      identical(schema$status, "pass") &&
      all(completeness$completo %in% TRUE) &&
      identical(unique(completeness$overall_status)[[1]], "pass")
  ) "verified_complete" else "partitioned_unverified"

  if (isTRUE(snapshot)) {
    attr(combined, "vigiar_snapshot") <- vigiar_snapshot(
      combined, tabela = tabela
    )
  }
  combined
}

#' Measure Rio de Janeiro municipality coverage
#'
#' @param dados A data frame with a municipality code column.
#' @param por Coverage level: overall, by year, by month, by year-month,
#'   by health macro-region, or by health region.
#' @param exigir_coluna_municipio If \code{TRUE}, error when no municipality
#'   code column can be detected. If \code{FALSE}, return an explicit unknown
#'   coverage row.
#' @return A tibble with coverage metrics and list-columns for absent
#'   municipalities, absent codes, and incomplete macro-regions.
#' @export
vigiar_rj_cobertura <- function(
  dados,
  por = c("geral", "ano", "mes", "ano_mes", "macrorregiao", "regiao_saude"),
  exigir_coluna_municipio = TRUE
) {
  por <- match.arg(por)
  .vigiar_rj_cobertura_impl(
    dados = dados,
    por = por,
    exigir_coluna_municipio = exigir_coluna_municipio
  )
}

.vigiar_rj_cobertura_impl <- function(dados, por, exigir_coluna_municipio,
                                       periodos_esperados = NULL) {
  dados <- tibble::as_tibble(dados)
  col_muni <- .vigiar_coluna_municipio(dados)
  possivel_truncamento <- isTRUE(attr(dados, "vigiar_possivel_truncamento"))

  if (is.na(col_muni)) {
    msg <- "Municipality code column not found; RJ coverage is unknown."
    if (isTRUE(exigir_coluna_municipio)) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
    return(.vigiar_cobertura_sem_municipio(por, possivel_truncamento))
  }

  dados$codigo_ibge_6__vigiar <- .vigiar_normalizar_codigo_municipio(dados[[col_muni]])
  groups <- .vigiar_grupos_cobertura(dados, por, periodos_esperados)
  rows <- lapply(groups, function(g) {
    .vigiar_cobertura_linha(
      dados = dados[g$idx, , drop = FALSE],
      por = g$por,
      ano = g$ano,
      mes = g$mes,
      macrorregiao_saude = g$macrorregiao_saude,
      regiao_saude = g$regiao_saude,
      expected_codes = g$expected_codes,
      possivel_truncamento = possivel_truncamento
    )
  })

  out <- do.call(rbind.data.frame, lapply(rows, function(row) {
    data.frame(
      por = row$por,
      nivel = row$por,
      ano = row$ano,
      mes = row$mes,
      macrorregiao_saude = row$macrorregiao_saude,
      regiao_saude = row$regiao_saude,
      n_municipios_presentes = row$n_municipios_presentes,
      n_municipios_esperados = row$n_municipios_esperados,
      cobertura_pct = row$cobertura_pct,
      n_ausentes = row$n_ausentes,
      completo = row$completo,
      periodo_ausente = row$periodo_ausente,
      possivel_truncamento = row$possivel_truncamento,
      stringsAsFactors = FALSE
    )
  }))
  out$municipios_ausentes <- I(lapply(rows, `[[`, "municipios_ausentes"))
  out$codigos_ausentes <- I(lapply(rows, `[[`, "codigos_ausentes"))
  out$macrorregioes_incompletas <- I(lapply(rows, `[[`, "macrorregioes_incompletas"))
  out <- tibble::as_tibble(out)
  class(out) <- c("vigiar_coverage", class(out))
  out
}

#' Check RJ completeness using the table's expected panel grain
#'
#' This function distinguishes "data were downloaded" from "the expected RJ
#' panel is complete". For `df_mensal`, completeness is assessed by
#' municipality x year x month. For `df_anual`, completeness is assessed by
#' municipality x year. For `df_dias` and `df_dias_conama`, completeness is
#' assessed by municipality x year x month when month is available, otherwise
#' by municipality x year.
#'
#' @param dados A data frame with municipality codes.
#' @param tabela Optional table name. Defaults to the `vigiar_tabela` attribute.
#' @param require_complete If \code{TRUE}, incomplete coverage, invalid temporal
#'   values, or possible truncation is an error.
#' @param anos_esperados Optional integer vector defining the expected years.
#' @param meses_esperados Optional integer vector defining expected months.
#' @param periodo_inicio Optional first expected period as a Date or a
#'   `YYYY-MM`/`YYYY-MM-DD` string.
#' @param periodo_fim Optional last expected period in the same format as
#'   `periodo_inicio`.
#' @param inferir_periodos If \code{TRUE}, fill periods between the minimum and
#'   maximum observed values when no expected domain is supplied.
#' @return A tibble with RJ coverage metrics at the expected table grain.
#' @export
vigiar_rj_completude_tabela <- function(
  dados,
  tabela = NULL,
  require_complete = FALSE,
  anos_esperados = NULL,
  meses_esperados = NULL,
  periodo_inicio = NULL,
  periodo_fim = NULL,
  inferir_periodos = TRUE
) {
  tabela <- tabela %||% attr(dados, "vigiar_tabela") %||% "dados"
  dados <- tibble::as_tibble(dados)
  por <- .vigiar_cobertura_por_tabela(tabela, dados)
  temporal_validation <- .vigiar_validar_temporal_rj(dados, por)
  if (!temporal_validation$ok) {
    warning(paste(temporal_validation$details, collapse = " "), call. = FALSE)
    if (isTRUE(require_complete)) {
      stop(
        "Invalid or missing temporal values prevent a complete RJ panel claim.",
        call. = FALSE
      )
    }
  }

  domain <- .vigiar_domino_temporal_rj(
    dados = dados,
    por = por,
    anos_esperados = anos_esperados,
    meses_esperados = meses_esperados,
    periodo_inicio = periodo_inicio,
    periodo_fim = periodo_fim,
    inferir_periodos = inferir_periodos
  )

  cobertura <- .vigiar_rj_cobertura_impl(
    dados = dados,
    por = por,
    exigir_coluna_municipio = TRUE,
    periodos_esperados = domain$periodos
  )
  cobertura$tabela <- tabela
  cobertura$grade <- .vigiar_grade_cobertura_label(tabela, por)
  cobertura$dominio_temporal <- domain$source
  cobertura$spatial_coverage_status <- ifelse(
    cobertura$completo, "complete", "incomplete"
  )
  panel_status <- if (all(cobertura$completo)) "complete" else "incomplete"
  temporal_status <- if (!temporal_validation$ok) {
    "invalid"
  } else if (any(cobertura$periodo_ausente)) {
    "incomplete"
  } else if (domain$source == "observed") {
    "unknown"
  } else {
    "complete_within_domain"
  }
  truncation_status <- if (any(cobertura$possivel_truncamento)) {
    "possible"
  } else {
    "no_evidence"
  }
  verification_status <- if (truncation_status == "possible") {
    "possible_truncation"
  } else if (!temporal_validation$ok) {
    "invalid_temporal_values"
  } else if (panel_status == "incomplete") {
    "partial"
  } else {
    paste0("complete_within_", domain$source, "_domain")
  }
  overall_status <- if (
    truncation_status == "possible" || !temporal_validation$ok ||
      panel_status == "incomplete"
  ) {
    "fail"
  } else if (domain$source %in% c("observed", "inferred")) {
    "unknown"
  } else {
    "pass"
  }
  cobertura$panel_status <- panel_status
  cobertura$temporal_domain_status <- temporal_status
  cobertura$schema_status <- if (nzchar(attr(dados, "vigiar_schema_hash") %||% "")) {
    "recorded"
  } else {
    "unknown"
  }
  cobertura$truncation_status <- truncation_status
  cobertura$verification_status <- verification_status
  cobertura$overall_status <- overall_status
  cobertura <- cobertura[
    c("tabela", "grade", setdiff(names(cobertura), c("tabela", "grade")))
  ]
  class(cobertura) <- c(
    "vigiar_completeness",
    setdiff(class(cobertura), "vigiar_completeness")
  )

  incomplete <- any(!cobertura$completo)
  truncated <- any(cobertura$possivel_truncamento)
  if (isTRUE(require_complete) && (incomplete || truncated)) {
    if (truncated) {
      stop(
        "Possible API truncation was detected; complete RJ table coverage cannot be guaranteed.",
        call. = FALSE
      )
    }
    stop(
      sprintf("RJ table coverage is incomplete for '%s' at grain '%s'.",
              tabela, unique(cobertura$grade)[[1]]),
      call. = FALSE
    )
  }

  attr(cobertura, "vigiar_temporal_validation") <- temporal_validation
  attr(cobertura, "vigiar_expected_periods") <- domain$periodos
  attr(cobertura, "vigiar_completeness_summary") <- list(
    spatial_coverage = if (all(cobertura$completo)) "complete" else "incomplete",
    temporal_domain = temporal_status,
    panel_completeness = panel_status,
    schema = unique(cobertura$schema_status),
    truncation = truncation_status,
    verification = verification_status,
    overall = overall_status
  )
  cobertura
}

#' Print an RJ coverage result
#'
#' @param x A `vigiar_coverage` object.
#' @param ... Additional arguments passed to the tibble print method.
#' @return `x`, invisibly.
#' @export
print.vigiar_coverage <- function(x, ...) {
  complete <- sum(x$completo %in% TRUE, na.rm = TRUE)
  cat(sprintf("<vigiar_coverage> %d/%d group(s) complete\n", complete, nrow(x)))
  out <- x
  class(out) <- setdiff(class(out), "vigiar_coverage")
  print(out, ...)
  invisible(x)
}

#' Summarise an RJ coverage result
#'
#' @param object A `vigiar_coverage` object.
#' @param ... Additional arguments (unused).
#' @return A named list with machine-readable coverage status.
#' @export
summary.vigiar_coverage <- function(object, ...) {
  list(
    status = if (nrow(object) > 0L && all(object$completo %in% TRUE)) {
      "complete"
    } else {
      "incomplete"
    },
    n_groups = nrow(object),
    n_complete_groups = sum(object$completo %in% TRUE, na.rm = TRUE),
    n_incomplete_groups = sum(object$completo %in% FALSE, na.rm = TRUE),
    possible_truncation = any(object$possivel_truncamento %in% TRUE, na.rm = TRUE)
  )
}

#' Print an RJ panel-completeness result
#'
#' @param x A `vigiar_completeness` object.
#' @param ... Additional arguments passed to the coverage print method.
#' @return `x`, invisibly.
#' @export
print.vigiar_completeness <- function(x, ...) {
  status <- unique(x$overall_status %||% "unknown")
  cat(sprintf(
    "<vigiar_completeness> overall=%s, verification=%s\n",
    paste(status, collapse = ","),
    paste(unique(x$verification_status %||% "unverified"), collapse = ",")
  ))
  out <- x
  class(out) <- setdiff(class(out), "vigiar_completeness")
  print(out, ...)
  invisible(x)
}

#' Summarise an RJ panel-completeness result
#'
#' @param object A `vigiar_completeness` object.
#' @param ... Additional arguments (unused).
#' @return The structured completeness summary attached by
#'   `vigiar_rj_completude_tabela()`.
#' @export
summary.vigiar_completeness <- function(object, ...) {
  attr(object, "vigiar_completeness_summary") %||% list(
    spatial_coverage = "unknown",
    temporal_domain = "unknown",
    panel_completeness = "unknown",
    schema = "unknown",
    truncation = "unknown",
    verification = "unverified",
    overall = "unknown"
  )
}

#' List absent Rio de Janeiro municipalities
#'
#' @param dados A data frame with a municipality code column.
#' @param por Missingness level: overall, by year, by month, by year-month,
#'   by health macro-region, or by health region.
#' @return A tibble with one row per absent municipality per level.
#' @export
vigiar_rj_municipios_ausentes <- function(
  dados,
  por = c("geral", "ano", "mes", "ano_mes", "macrorregiao", "regiao_saude")
) {
  por <- match.arg(por)

  cobertura <- suppressWarnings(vigiar_rj_cobertura(
    dados,
    por = por,
    exigir_coluna_municipio = FALSE
  ))

  out <- lapply(seq_len(nrow(cobertura)), function(i) {
    codes <- cobertura$codigos_ausentes[[i]]
    reg <- RJ_MUNICIPIOS[RJ_MUNICIPIOS$codigo_ibge_6 %in% codes, ]
    if (nrow(reg) == 0) {
      return(NULL)
    }
    reg$por <- cobertura$por[[i]]
    reg$nivel <- cobertura$por[[i]]
    reg$ano <- cobertura$ano[[i]]
    reg$mes <- cobertura$mes[[i]]
    reg$grupo_macrorregiao_saude <- cobertura$macrorregiao_saude[[i]]
    reg$grupo_regiao_saude <- cobertura$regiao_saude[[i]]
    reg[c(
      "por", "nivel", "ano", "mes", "grupo_macrorregiao_saude",
      "grupo_regiao_saude", "codigo_ibge", "codigo_ibge_6",
      "codigo_ibge_7", "municipio", "macrorregiao_saude", "regiao_saude"
    )]
  })
  out <- out[!vapply(out, is.null, logical(1))]

  if (length(out) == 0) {
    return(tibble::tibble(
      por = character(0),
      nivel = character(0),
      ano = integer(0),
      mes = integer(0),
      grupo_macrorregiao_saude = character(0),
      grupo_regiao_saude = character(0),
      codigo_ibge = integer(0),
      codigo_ibge_6 = integer(0),
      codigo_ibge_7 = integer(0),
      municipio = character(0),
      macrorregiao_saude = character(0),
      regiao_saude = character(0)
    ))
  }

  tibble::as_tibble(do.call(rbind.data.frame, out))
}

#' Exploratory PM2.5 plot for Rio de Janeiro data
#'
#' @param dados A data frame already downloaded and processed by vigiar.
#' @param por Grouping level: "ano", "macrorregiao", or "municipio".
#' @param valor Optional PM2.5 value column. If \code{NULL}, it is detected.
#' @return A ggplot object.
#' @export
vigiar_plot_pm25_rj <- function(dados, por = c("ano", "macrorregiao", "municipio"), valor = NULL) {
  por <- match.arg(por)
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for vigiar_plot_pm25_rj().", call. = FALSE)
  }

  dados <- .vigiar_filtrar_rj(dados, validar = FALSE)
  if (nrow(dados) == 0) {
    stop("No RJ municipality was detected in the data.", call. = FALSE)
  }

  valor <- valor %||% .vigiar_coluna_pm25(dados)
  if (is.na(valor) || !valor %in% names(dados)) {
    stop("PM2.5 value column not found.", call. = FALSE)
  }

  dados <- merge(dados, RJ_MUNICIPIOS, by = "codigo_ibge_6", all.x = TRUE,
                 suffixes = c("", "_rj"))
  dados[[valor]] <- as.numeric(dados[[valor]])

  if (por == "ano") {
    if (!"ano" %in% names(dados)) {
      stop("Column 'ano' is required for por = 'ano'.", call. = FALSE)
    }
    plot_data <- stats::aggregate(dados[[valor]], list(ano = dados$ano), mean, na.rm = TRUE)
    names(plot_data)[2] <- "pm25"
    return(ggplot2::ggplot(plot_data, ggplot2::aes(.data[["ano"]], .data[["pm25"]])) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Year", y = "PM2.5", title = "RJ mean PM2.5 by year"))
  }

  group_col <- if (por == "macrorregiao") "macrorregiao_saude" else "municipio"
  plot_data <- stats::aggregate(dados[[valor]], list(grupo = dados[[group_col]]), mean, na.rm = TRUE)
  names(plot_data)[2] <- "pm25"
  ggplot2::ggplot(plot_data, ggplot2::aes(.data[["pm25"]], stats::reorder(.data[["grupo"]], .data[["pm25"]]))) +
    ggplot2::geom_col() +
    ggplot2::labs(x = "Mean PM2.5", y = NULL, title = paste("RJ PM2.5 by", por))
}

# ---- Internal helpers ---------------------------------------------------------

.vigiar_coluna_municipio <- function(dados) {
  intersect(
    c("codigo_ibge_6", "cod_municipio", "muni", "id_muni", "ID_MUNI",
      "codigo_ibge", "cod_ibge", "codigo_municipio", "MUN_COD",
      "cod_municipio_6", "cod_municipio_7", "codigo_ibge_7"),
    names(dados)
  )[1]
}

.vigiar_coluna_uf <- function(dados) {
  intersect(c("sigla_uf", "UF", "UF_SIGLA", "uf", "cod_uf"), names(dados))[1]
}

.vigiar_coluna_pm25 <- function(dados) {
  intersect(c("pm25_media_anual", "pm25_media", "pm25",
              "Media_pm25", "pm25_media_periodo"), names(dados))[1]
}

.vigiar_cobertura_por_tabela <- function(tabela, dados) {
  tabela <- tabela %||% ""
  if (identical(tabela, "df_mensal")) {
    if (!all(c("ano", "mes") %in% names(dados))) {
      stop("Table 'df_mensal' requires 'ano' and 'mes' columns for RJ completeness.",
           call. = FALSE)
    }
    return("ano_mes")
  }

  if (tabela %in% c("df_anual", "pop")) {
    if ("ano" %in% names(dados)) {
      return("ano")
    }
    return("geral")
  }

  if (tabela %in% c("df_dias", "df_dias_conama")) {
    if (all(c("ano", "mes") %in% names(dados))) {
      return("ano_mes")
    }
    if ("ano" %in% names(dados)) {
      return("ano")
    }
    return("geral")
  }

  if (all(c("ano", "mes") %in% names(dados))) {
    return("ano_mes")
  }
  if ("ano" %in% names(dados)) {
    return("ano")
  }
  "geral"
}

.vigiar_grade_cobertura_label <- function(tabela, por) {
  switch(por,
    ano_mes = "municipio x ano x mes",
    ano = "municipio x ano",
    mes = "municipio x mes",
    geral = "municipio",
    macrorregiao = "macrorregiao",
    regiao_saude = "regiao_saude",
    por
  )
}

.vigiar_normalizar_codigo_municipio <- function(x, formato = c("auto", "6", "7")) {
  formato <- match.arg(formato)
  if (length(x) == 0) {
    return(integer(0))
  }

  out6 <- rep(NA_integer_, length(x))
  out7 <- rep(NA_integer_, length(x))
  reference <- .vigiar_ibge_reference()
  missing <- is.na(x)
  chr <- trimws(as.character(x))
  chr[missing] <- NA_character_
  chr <- sub("\\.0+$", "", chr)
  valid_digits <- !is.na(chr) & nzchar(chr) & grepl("^[0-9]+$", chr)

  is_6 <- valid_digits & nchar(chr) == 6
  if (any(is_6)) {
    val <- suppressWarnings(as.integer(chr[is_6]))
    ok <- !is.na(val) & val >= 110001L & val <= 530010L
    idx <- which(is_6)
    out6[idx[ok]] <- val[ok]
    match_ref <- match(val[ok], reference$codigo_ibge_6)
    exists <- !is.na(match_ref)
    if (any(exists)) {
      out7[idx[ok][exists]] <- reference$codigo_ibge_7[match_ref[exists]]
    }
  }

  is_7 <- valid_digits & nchar(chr) == 7
  if (any(is_7)) {
    val7 <- suppressWarnings(as.integer(chr[is_7]))
    val6 <- suppressWarnings(as.integer(substr(chr[is_7], 1, 6)))
    ok <- !is.na(val6) & val6 >= 110001L & val6 <= 530010L
    ref_idx <- match(val6, reference$codigo_ibge_6)
    exists <- !is.na(ref_idx)
    official_7 <- rep(FALSE, length(val7))
    official_7[exists] <- val7[exists] == reference$codigo_ibge_7[ref_idx[exists]]
    ok <- ok & exists & official_7
    idx <- which(is_7)
    out6[idx[ok]] <- val6[ok]
    out7[idx[ok]] <- val7[ok]
  }

  if (formato == "7") {
    return(out7)
  }

  out6
}

.vigiar_filtrar_rj <- function(dados, validar = TRUE) {
  source_attributes <- .vigiar_data_attributes(dados)
  dados <- .vigiar_as_tibble_preserve(dados)
  col_muni <- .vigiar_coluna_municipio(dados)
  col_uf <- .vigiar_coluna_uf(dados)

  if (!is.na(col_muni)) {
    before <- nrow(dados)
    dados$codigo_ibge_6 <- .vigiar_normalizar_codigo_municipio(dados[[col_muni]])
    idx <- !is.na(dados$codigo_ibge_6) &
      dados$codigo_ibge_6 %in% RJ_MUNICIPIOS$codigo_ibge_6
    dados <- dados[idx, , drop = FALSE]
    lookup <- match(dados$codigo_ibge_6, RJ_MUNICIPIOS$codigo_ibge_6)
    dados$codigo_ibge_7 <- RJ_MUNICIPIOS$codigo_ibge_7[lookup]
    cli::cli_alert_info("RJ filter by municipality registry: {nrow(dados)} rows from {before}.")
  } else if (!is.na(col_uf)) {
    before <- nrow(dados)
    normalized_uf <- .vigiar_normalizar_uf(dados[[col_uf]])
    dados <- dados[!is.na(normalized_uf) & normalized_uf == "RJ", , drop = FALSE]
    cli::cli_alert_warning(
      "RJ filter used UF column '{col_uf}' because no municipality code column was found."
    )
    cli::cli_alert_info("RJ filter by UF: {nrow(dados)} rows from {before}.")
  } else {
    warning(
      "No municipality or UF column was found. The data were not filtered to RJ.",
      call. = FALSE
    )
  }

  if (isTRUE(validar) && nrow(dados) > 0 && !is.na(.vigiar_coluna_municipio(dados))) {
    vigiar_validar_rj(dados)
  }

  out <- tibble::as_tibble(dados)
  .vigiar_restore_data_attributes(out, source_attributes)
}

.vigiar_detectar_truncamento <- function(dados, tabela = NULL, limite = NULL) {
  n <- nrow(dados)
  table_label <- tabela %||% "<unknown>"
  power_bi_threshold <- 28500L
  metadata <- attr(dados, "vigiar_response_metadata") %||% list()
  prior_status <- attr(dados, "vigiar_truncation_status") %||% NULL
  evidence <- as.character(attr(dados, "vigiar_truncation_evidence") %||% character())
  status <- if (length(prior_status) == 1L && !is.na(prior_status) && prior_status %in% c(
    "no_evidence", "possible", "probable", "confirmed", "unknown"
  )) prior_status else "no_evidence"

  rank <- c(no_evidence = 0L, unknown = 1L, possible = 2L,
            probable = 3L, confirmed = 4L)
  promote <- function(candidate, reason) {
    if (rank[[candidate]] > rank[[status]]) {
      status <<- candidate
    }
    evidence <<- unique(c(evidence, reason))
  }

  continuation <- isTRUE(metadata$has_more) || isTRUE(metadata$hasMore) ||
    isTRUE(metadata$continuation) || isTRUE(metadata$truncated)
  if (continuation) {
    promote("confirmed", "Power BI response metadata reports continuation or truncation.")
  }
  if (!is.null(limite) && length(limite) == 1L && is.finite(limite) && limite > 0L) {
    requested <- as.integer(limite)
    if (n >= requested) {
      promote("probable", sprintf(
        "Returned rows (%d) reached the requested limit (%d).", n, requested
      ))
    } else if (n >= max(1L, floor(0.95 * requested))) {
      promote("possible", sprintf(
        "Returned rows (%d) are within 5%% of the requested limit (%d).",
        n, requested
      ))
    }
  }
  if (n >= power_bi_threshold) {
    promote("possible", sprintf(
      paste0(
        "heuristic: returned rows (%d) reached the conservative Power BI ",
        "response threshold (%d)."
      ),
      n, power_bi_threshold
    ))
  }
  if (isTRUE(attr(dados, "vigiar_possivel_truncamento")) &&
      status == "no_evidence") {
    promote("possible", "A legacy upstream truncation flag was present.")
  }

  possible <- status %in% c("possible", "probable", "confirmed", "unknown")
  assessment <- list(
    status = status,
    evidence = evidence,
    returned_rows = n,
    requested_limit = limite %||% NA_integer_,
    heuristic_threshold = power_bi_threshold
  )
  attr(dados, "vigiar_truncation_status") <- status
  attr(dados, "vigiar_truncation_evidence") <- evidence
  attr(dados, "vigiar_truncation_assessment") <- assessment
  attr(dados, "vigiar_possivel_truncamento") <- possible

  if (status %in% c("possible", "probable", "confirmed")) {
    msg <- sprintf(
      "Truncation status '%s' for table '%s': %s",
      status, table_label, paste(evidence, collapse = " ")
    )
    .vigiar_log(
      "WARN", msg, table = table_label, metadata = assessment,
      event = "possible_truncation"
    )
    warning(
      msg,
      " Use validated partitions before claiming response completeness.",
      call. = FALSE
    )
  }

  dados
}

.vigiar_schema_hash <- function(tabela) {
  if (is.null(.vigiar_env$esquema) || is.null(.vigiar_env$esquema[[tabela]])) {
    return(NA_character_)
  }
  raw <- serialize(.vigiar_env$esquema[[tabela]], NULL)
  paste(format(openssl::sha256(raw)), collapse = "")
}

.vigiar_rj_cache_file <- function(tabela, colunas, ordenar_por, limite, schema_hash, dots) {
  cache_dir <- .vigiar_env$cache_dir %||% file.path(tools::R_user_dir("vigiar", "cache"))
  .vigiar_env$cache_dir <- cache_dir
  key <- list(
    cache_key_version = .VIGIAR_CACHE_KEY_VERSION,
    canonicalization_version = .VIGIAR_CANONICALIZATION_VERSION,
    package_version = as.character(utils::packageVersion("vigiar")),
    tabela = tabela,
    uf = "RJ",
    colunas = colunas,
    ordenar_por = ordenar_por,
    limite = limite,
    schema_hash = schema_hash,
    dots = dots
  )
  key_raw <- serialize(key, NULL)
  key_hash <- as.character(openssl::sha256(key_raw))
  file.path(cache_dir, paste0("rj-", tabela, "-", substr(key_hash, 1, 16), ".rds"))
}

.vigiar_filtro_servidor_rj <- function(tabela) {
  schema <- .vigiar_env$esquema[[tabela]]
  if (is.null(schema)) {
    return(NULL)
  }

  cols <- names(schema)
  col_uf <- intersect(c("UF", "sigla_uf", "UF_SIGLA", "uf", "cod_uf"), cols)[1]
  if (!is.na(col_uf)) {
    if (identical(col_uf, "cod_uf")) {
      type <- tolower(.vigiar_schema_column_type(schema[[col_uf]]) %||% "")
      value <- if (grepl("int|numeric|double|decimal", type)) 33L else "33"
    } else {
      value <- "RJ"
    }
    filtro <- list(value)
    names(filtro) <- col_uf
    return(filtro)
  }

  col_muni <- intersect(c("muni", "cod_municipio", "ID_MUNI", "codigo_ibge", "MUN_COD"), cols)[1]
  if (!is.na(col_muni)) {
    filtro <- list(as.integer(RJ_MUNICIPIOS$codigo_ibge_6))
    names(filtro) <- col_muni
    return(filtro)
  }

  NULL
}

.vigiar_filtro_servidor_municipio <- function(tabela, codigo_ibge_6) {
  schema <- .vigiar_env$esquema[[tabela]]
  if (is.null(schema)) {
    return(NULL)
  }
  cols <- names(schema)
  col_muni <- intersect(
    c("codigo_ibge_6", "cod_municipio", "muni", "id_muni", "ID_MUNI",
      "codigo_ibge", "cod_ibge", "codigo_municipio", "MUN_COD",
      "cod_municipio_6", "cod_municipio_7", "codigo_ibge_7"),
    cols
  )[1]
  if (is.na(col_muni)) {
    return(NULL)
  }
  registry <- RJ_MUNICIPIOS[
    RJ_MUNICIPIOS$codigo_ibge_6 == as.integer(codigo_ibge_6),
  ]
  if (nrow(registry) != 1L) {
    stop("Municipality code is not present in the RJ reference.", call. = FALSE)
  }
  value <- if (grepl("_7$", col_muni)) {
    as.integer(registry$codigo_ibge_7[[1]])
  } else {
    as.integer(registry$codigo_ibge_6[[1]])
  }
  type <- tolower(.vigiar_schema_column_type(schema[[col_muni]]) %||% "")
  if (grepl("string|text", type)) {
    value <- as.character(value)
  }
  filter <- list(value)
  names(filter) <- col_muni
  filter
}

.vigiar_filter_values <- function(name, values) {
  values <- values[!is.na(values)]
  if (identical(name, "cod_uf")) {
    normalized <- .vigiar_normalizar_uf(values)
    return(unname(.VIGIAR_UF_CODES[normalized]))
  }
  if (name %in% c("UF", "sigla_uf", "UF_SIGLA", "uf")) {
    return(.vigiar_normalizar_uf(values))
  }
  as.character(values)
}

.vigiar_combinar_filtros <- function(...) {
  parts <- list(...)
  out <- list()
  for (part in parts) {
    if (is.null(part) || length(part) == 0) {
      next
    }
    if (is.null(names(part)) || any(!nzchar(names(part)))) {
      stop("Server-side filters must be named.", call. = FALSE)
    }
    for (i in seq_along(part)) {
      name <- names(part)[[i]]
      values <- part[[i]]
      if (length(values) == 0L || all(is.na(values))) {
        stop("Server-side filter '", name, "' has no non-missing values.",
             call. = FALSE)
      }
      if (is.null(out[[name]])) {
        out[[name]] <- values[!is.na(values)]
        next
      }
      existing_key <- .vigiar_filter_values(name, out[[name]])
      incoming_key <- .vigiar_filter_values(name, values)
      common <- intersect(existing_key[!is.na(existing_key)],
                          incoming_key[!is.na(incoming_key)])
      if (length(common) == 0L) {
        stop(
          sprintf("Conflicting server-side filters for column '%s'.", name),
          call. = FALSE
        )
      }
      if (identical(name, "cod_uf")) {
        out[[name]] <- as.integer(common)
      } else if (name %in% c("UF", "sigla_uf", "UF_SIGLA", "uf")) {
        out[[name]] <- common
      } else {
        keep <- .vigiar_filter_values(name, out[[name]]) %in% common
        out[[name]] <- unique(out[[name]][keep])
      }
    }
  }
  if (length(out) == 0) {
    return(NULL)
  }
  out
}

.vigiar_processar_tabela_rj <- function(dados, tabela, tipo = NULL) {
  if (!is.null(tipo) && tabela %in% c("df_anual", "df_mensal", "df_dias", "df_dias_conama")) {
    return(process_pm25(dados, tipo = tipo))
  }
  process_vigiar(dados, tabela = tabela)
}

.vigiar_anexar_metadados_rj <- function(dados, tabela, cobertura,
                                         possivel_truncamento, schema_hash) {
  attr(dados, "vigiar_tabela") <- tabela
  attr(dados, "vigiar_uf") <- "RJ"
  attr(dados, "vigiar_rj_n_municipios") <- cobertura$n_municipios_presentes[[1]]
  attr(dados, "vigiar_rj_n_esperado") <- cobertura$n_municipios_esperados[[1]]
  attr(dados, "vigiar_rj_cobertura_pct") <- cobertura$cobertura_pct[[1]]
  attr(dados, "vigiar_rj_municipios_ausentes") <- cobertura$municipios_ausentes[[1]]
  attr(dados, "vigiar_n_municipios_presentes") <- cobertura$n_municipios_presentes[[1]]
  attr(dados, "vigiar_n_municipios_esperados") <- cobertura$n_municipios_esperados[[1]]
  attr(dados, "vigiar_cobertura_rj_pct") <- cobertura$cobertura_pct[[1]]
  attr(dados, "vigiar_municipios_ausentes") <- cobertura$municipios_ausentes[[1]]
  attr(dados, "vigiar_download_timestamp") <- Sys.time()
  attr(dados, "vigiar_schema_hash") <- schema_hash
  attr(dados, "vigiar_possivel_truncamento") <- possivel_truncamento
  attr(dados, "vigiar_rj_cobertura") <- cobertura
  dados
}

.vigiar_emitir_cobertura_rj <- function(cobertura) {
  n <- cobertura$n_municipios_presentes[[1]]
  pct <- cobertura$cobertura_pct[[1]]

  if (n == 92L) {
    cli::cli_alert_success("OK: complete RJ coverage, 92/92 municipalities.")
  } else if (n == 0L) {
    warning(
      "Critical: no valid Rio de Janeiro municipality code was detected.",
      call. = FALSE
    )
  } else {
    warning(sprintf(
      "Warning: partial RJ coverage, %d/92 municipalities (%.1f%%).",
      n, pct
    ), call. = FALSE)
  }

  if (isTRUE(cobertura$possivel_truncamento[[1]])) {
    warning(
      "Possible truncation: the response reached an approximate API limit; use a validated partitioned download.",
      call. = FALSE
    )
  }
}

.vigiar_cobertura_sem_municipio <- function(por, possivel_truncamento) {
  row <- data.frame(
    por = por,
    nivel = por,
    ano = NA_integer_,
    mes = NA_integer_,
    macrorregiao_saude = NA_character_,
    regiao_saude = NA_character_,
    n_municipios_presentes = 0L,
    n_municipios_esperados = 92L,
    cobertura_pct = 0,
    n_ausentes = 92L,
    completo = FALSE,
    periodo_ausente = por %in% c("ano", "mes", "ano_mes"),
    possivel_truncamento = possivel_truncamento,
    stringsAsFactors = FALSE
  )
  row$municipios_ausentes <- I(list(RJ_MUNICIPIOS$municipio))
  row$codigos_ausentes <- I(list(RJ_MUNICIPIOS$codigo_ibge_6))
  row$macrorregioes_incompletas <- I(list(vigiar_rj_macrorregioes()))
  row <- tibble::as_tibble(row)
  class(row) <- c("vigiar_coverage", class(row))
  row
}

.vigiar_validar_temporal_rj <- function(dados, por) {
  needs_year <- por %in% c("ano", "ano_mes")
  needs_month <- por %in% c("mes", "ano_mes")
  current_year <- as.integer(format(Sys.Date(), "%Y"))

  ano_missing <- ano_invalid <- mes_missing <- mes_invalid <- 0L
  if (needs_year) {
    raw <- dados$ano
    converted <- suppressWarnings(as.integer(as.character(raw)))
    ano_missing <- sum(is.na(raw) | !nzchar(trimws(as.character(raw))))
    supplied <- !is.na(raw) & nzchar(trimws(as.character(raw)))
    ano_invalid <- sum(supplied & (
      is.na(converted) | converted < 2000L | converted > current_year
    ))
  }
  if (needs_month) {
    raw <- dados$mes
    converted <- suppressWarnings(as.integer(as.character(raw)))
    mes_missing <- sum(is.na(raw) | !nzchar(trimws(as.character(raw))))
    supplied <- !is.na(raw) & nzchar(trimws(as.character(raw)))
    mes_invalid <- sum(supplied & (
      is.na(converted) | converted < 1L | converted > 12L
    ))
  }

  details <- character()
  if (ano_missing > 0L) {
    details <- c(details, sprintf("%d row(s) have a missing year.", ano_missing))
  }
  if (ano_invalid > 0L) {
    details <- c(details, sprintf("%d row(s) have a year outside 2000-%d.",
                                  ano_invalid, current_year))
  }
  if (mes_missing > 0L) {
    details <- c(details, sprintf("%d row(s) have a missing month.", mes_missing))
  }
  if (mes_invalid > 0L) {
    details <- c(details, sprintf("%d row(s) have a month outside 1-12.", mes_invalid))
  }

  c(.vigiar_check_result(
    if (length(details) == 0L) "pass" else "fail",
    details = details
  ), list(
    n_ano_ausente = as.integer(ano_missing),
    n_ano_invalido = as.integer(ano_invalid),
    n_mes_ausente = as.integer(mes_missing),
    n_mes_invalido = as.integer(mes_invalid)
  ))
}

.vigiar_parse_periodo <- function(x, name) {
  if (is.null(x)) {
    return(NULL)
  }
  if (length(x) != 1L || is.na(x)) {
    stop("'", name, "' must identify exactly one non-missing period.",
         call. = FALSE)
  }
  if (inherits(x, "Date")) {
    return(as.Date(format(x, "%Y-%m-01")))
  }
  value <- as.character(x)
  if (grepl("^[0-9]{4}-[0-9]{2}$", value)) {
    value <- paste0(value, "-01")
  }
  parsed <- suppressWarnings(as.Date(value))
  if (is.na(parsed)) {
    stop("'", name, "' must be a Date or YYYY-MM/YYY-MM-DD string.",
         call. = FALSE)
  }
  as.Date(format(parsed, "%Y-%m-01"))
}

.vigiar_domino_temporal_rj <- function(dados, por, anos_esperados = NULL,
                                        meses_esperados = NULL,
                                        periodo_inicio = NULL,
                                        periodo_fim = NULL,
                                        inferir_periodos = TRUE) {
  temporal <- por %in% c("ano", "mes", "ano_mes")
  if (!temporal) {
    return(list(periodos = NULL, source = "not_applicable"))
  }

  start <- .vigiar_parse_periodo(periodo_inicio, "periodo_inicio")
  end <- .vigiar_parse_periodo(periodo_fim, "periodo_fim")
  if (xor(is.null(start), is.null(end))) {
    stop("'periodo_inicio' and 'periodo_fim' must be supplied together.",
         call. = FALSE)
  }
  if (!is.null(start) && start > end) {
    stop("'periodo_inicio' must not be after 'periodo_fim'.", call. = FALSE)
  }

  observed_years <- if ("ano" %in% names(dados)) {
    y <- suppressWarnings(as.integer(as.character(dados$ano)))
    sort(unique(y[!is.na(y) & y >= 2000L & y <= as.integer(format(Sys.Date(), "%Y"))]))
  } else {
    integer()
  }
  observed_months <- if ("mes" %in% names(dados)) {
    m <- suppressWarnings(as.integer(as.character(dados$mes)))
    sort(unique(m[!is.na(m) & m >= 1L & m <= 12L]))
  } else {
    integer()
  }

  user_specified <- !is.null(anos_esperados) || !is.null(meses_esperados) ||
    !is.null(start)
  if (!is.null(anos_esperados)) {
    anos_esperados <- suppressWarnings(as.integer(anos_esperados))
    if (anyNA(anos_esperados) || any(anos_esperados < 2000L)) {
      stop("'anos_esperados' must contain valid years from 2000 onward.",
           call. = FALSE)
    }
    anos_esperados <- sort(unique(anos_esperados))
  }
  if (!is.null(meses_esperados)) {
    meses_esperados <- suppressWarnings(as.integer(meses_esperados))
    if (anyNA(meses_esperados) || any(!meses_esperados %in% 1:12)) {
      stop("'meses_esperados' must contain integers from 1 to 12.",
           call. = FALSE)
    }
    meses_esperados <- sort(unique(meses_esperados))
  }

  if (!is.null(start)) {
    dates <- seq(start, end, by = "month")
    periods <- data.frame(
      ano = as.integer(format(dates, "%Y")),
      mes = as.integer(format(dates, "%m"))
    )
    if (por == "ano") {
      periods <- unique(periods["ano"])
    } else if (por == "mes") {
      periods <- unique(periods["mes"])
    }
    return(list(periodos = periods, source = "user_specified"))
  }

  if (user_specified) {
    years <- anos_esperados %||% observed_years
    months <- meses_esperados %||% observed_months
    periods <- switch(por,
      ano = data.frame(ano = years),
      mes = data.frame(mes = months),
      ano_mes = expand.grid(ano = years, mes = months)
    )
    periods <- periods[do.call(order, periods), , drop = FALSE]
    return(list(periodos = periods, source = "user_specified"))
  }

  if (por == "ano") {
    years <- if (isTRUE(inferir_periodos) && length(observed_years) > 0L) {
      seq.int(min(observed_years), max(observed_years))
    } else {
      observed_years
    }
    return(list(
      periodos = data.frame(ano = years),
      source = if (isTRUE(inferir_periodos)) "inferred" else "observed"
    ))
  }

  if (por == "mes") {
    months <- if (isTRUE(inferir_periodos) && length(observed_months) > 0L) {
      seq.int(min(observed_months), max(observed_months))
    } else {
      observed_months
    }
    return(list(
      periodos = data.frame(mes = months),
      source = if (isTRUE(inferir_periodos)) "inferred" else "observed"
    ))
  }

  valid <- !is.na(dados$ano) & !is.na(dados$mes)
  years <- suppressWarnings(as.integer(as.character(dados$ano[valid])))
  months <- suppressWarnings(as.integer(as.character(dados$mes[valid])))
  valid <- !is.na(years) & !is.na(months) & years >= 2000L & months %in% 1:12
  years <- years[valid]
  months <- months[valid]
  if (length(years) == 0L) {
    return(list(
      periodos = data.frame(ano = integer(), mes = integer()),
      source = if (isTRUE(inferir_periodos)) "inferred" else "observed"
    ))
  }
  dates <- as.Date(sprintf("%04d-%02d-01", years, months))
  if (isTRUE(inferir_periodos)) {
    dates <- seq(min(dates), max(dates), by = "month")
  } else {
    dates <- sort(unique(dates))
  }
  list(
    periodos = data.frame(
      ano = as.integer(format(dates, "%Y")),
      mes = as.integer(format(dates, "%m"))
    ),
    source = if (isTRUE(inferir_periodos)) "inferred" else "observed"
  )
}

.vigiar_grupos_cobertura <- function(dados, por, periodos_esperados = NULL) {
  all_codes <- RJ_MUNICIPIOS$codigo_ibge_6
  make_group <- function(idx, ano = NA_integer_, mes = NA_integer_,
                         macrorregiao_saude = NA_character_,
                         regiao_saude = NA_character_,
                         expected_codes = all_codes) {
    list(
      idx = idx,
      por = por,
      ano = as.integer(ano),
      mes = as.integer(mes),
      macrorregiao_saude = macrorregiao_saude,
      regiao_saude = regiao_saude,
      expected_codes = expected_codes,
      periodo_ausente = length(idx) == 0L && por %in% c("ano", "mes", "ano_mes")
    )
  }

  if (nrow(dados) == 0 && is.null(periodos_esperados)) {
    return(list(make_group(integer(0))))
  }

  if (por == "geral") {
    return(list(make_group(seq_len(nrow(dados)))))
  }

  if (por %in% c("ano", "ano_mes") && !"ano" %in% names(dados)) {
    stop("Column 'ano' is required for this RJ coverage level.", call. = FALSE)
  }
  if (por %in% c("mes", "ano_mes") && !"mes" %in% names(dados)) {
    stop("Column 'mes' is required for this RJ coverage level.", call. = FALSE)
  }

  if (por == "ano") {
    keys <- if (!is.null(periodos_esperados)) {
      as.integer(periodos_esperados$ano)
    } else {
      sort(unique(as.integer(dados$ano)))
    }
    return(lapply(keys, function(y) {
      make_group(which(as.integer(dados$ano) == y), ano = y)
    }))
  }

  if (por == "mes") {
    keys <- if (!is.null(periodos_esperados)) {
      as.integer(periodos_esperados$mes)
    } else {
      sort(unique(as.integer(dados$mes)))
    }
    return(lapply(keys, function(m) {
      make_group(which(as.integer(dados$mes) == m), mes = m)
    }))
  }

  if (por == "macrorregiao") {
    keys <- sort(unique(RJ_MUNICIPIOS$macrorregiao_saude))
    return(lapply(keys, function(regiao) {
      expected <- RJ_MUNICIPIOS$codigo_ibge_6[RJ_MUNICIPIOS$macrorregiao_saude == regiao]
      make_group(
        which(dados$codigo_ibge_6__vigiar %in% expected),
        macrorregiao_saude = regiao,
        expected_codes = expected
      )
    }))
  }

  if (por == "regiao_saude") {
    keys <- sort(unique(RJ_MUNICIPIOS$regiao_saude))
    return(lapply(keys, function(regiao) {
      expected <- RJ_MUNICIPIOS$codigo_ibge_6[RJ_MUNICIPIOS$regiao_saude == regiao]
      make_group(
        which(dados$codigo_ibge_6__vigiar %in% expected),
        regiao_saude = regiao,
        expected_codes = expected
      )
    }))
  }

  combos <- if (!is.null(periodos_esperados)) {
    unique(data.frame(
      ano = as.integer(periodos_esperados$ano),
      mes = as.integer(periodos_esperados$mes)
    ))
  } else {
    unique(data.frame(
      ano = as.integer(dados$ano),
      mes = as.integer(dados$mes)
    ))
  }
  combos <- combos[order(combos$ano, combos$mes), , drop = FALSE]
  lapply(seq_len(nrow(combos)), function(i) {
    y <- combos$ano[[i]]
    m <- combos$mes[[i]]
    make_group(
      which(as.integer(dados$ano) == y & as.integer(dados$mes) == m),
      ano = y,
      mes = m
    )
  })
}

.vigiar_cobertura_linha <- function(dados, por, ano, mes, macrorregiao_saude,
                                    regiao_saude, expected_codes,
                                    possivel_truncamento) {
  presentes <- unique(dados$codigo_ibge_6__vigiar)
  presentes <- sort(presentes[!is.na(presentes) & presentes %in% expected_codes])
  ausentes_cod <- sort(setdiff(expected_codes, presentes))
  ausentes <- RJ_MUNICIPIOS$municipio[match(ausentes_cod, RJ_MUNICIPIOS$codigo_ibge_6)]
  incomplete <- .vigiar_macrorregioes_incompletas(presentes, expected_codes)
  n_present <- length(presentes)
  n_expected <- length(expected_codes)

  list(
    por = por,
    ano = as.integer(ano),
    mes = as.integer(mes),
    macrorregiao_saude = macrorregiao_saude,
    regiao_saude = regiao_saude,
    n_municipios_presentes = as.integer(n_present),
    n_municipios_esperados = as.integer(n_expected),
    cobertura_pct = if (n_expected > 0) round(100 * n_present / n_expected, 1) else NA_real_,
    n_ausentes = as.integer(length(ausentes_cod)),
    municipios_ausentes = ausentes,
    codigos_ausentes = ausentes_cod,
    macrorregioes_incompletas = incomplete,
    completo = n_present == n_expected,
    periodo_ausente = nrow(dados) == 0L && por %in% c("ano", "mes", "ano_mes"),
    possivel_truncamento = possivel_truncamento,
    stringsAsFactors = FALSE
  )
}

.vigiar_macrorregioes_incompletas <- function(presentes, expected_codes = RJ_MUNICIPIOS$codigo_ibge_6) {
  registry <- RJ_MUNICIPIOS[RJ_MUNICIPIOS$codigo_ibge_6 %in% expected_codes, ]
  expected <- table(registry$macrorregiao_saude)
  if (length(presentes) == 0) {
    return(names(expected))
  }

  observed_registry <- registry[registry$codigo_ibge_6 %in% presentes, ]
  observed <- table(observed_registry$macrorregiao_saude)
  incomplete <- names(expected)[vapply(names(expected), function(regiao) {
    obs <- if (regiao %in% names(observed)) observed[[regiao]] else 0L
    obs < expected[[regiao]]
  }, logical(1))]
  sort(incomplete)
}
