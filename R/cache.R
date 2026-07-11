# Package: vigiar
# Reproducibility snapshots and local caching
#
# Implements data snapshots with SHA256 checksums, session
# provenance, and a local cache layer for offline reproducibility.
# Follows microdatasus's philosophy of guaranteed reproducibility.

#' Snapshot data with full provenance
#'
#' Downloads (or accepts) data and wraps it with comprehensive
#' metadata: checksums, session info, timestamps, parameters.
#' Snapshots preserve versioned data, schema, metadata, and provenance hashes.
#' They detect changes but cannot by themselves guarantee that an external
#' source remains available or scientifically complete.
#'
#' @param dados A data frame (or pass \code{tabela}, \code{...}
#'   to download).
#' @param tabela Table name (if downloading).
#' @param ... Extra arguments passed to \code{vigiar_baixar()}.
#' @param congelar_esquema If \code{TRUE}, also snapshots the
#'   conceptual schema.
#' @return A \code{vigiar_snapshot} object (list subclass).
#' @export
vigiar_snapshot <- function(dados = NULL, tabela = NULL, ...,
                             congelar_esquema = FALSE) {
  if (is.null(dados) && !is.null(tabela)) {
    if (is.null(.vigiar_env$sessao)) {
      stop("Nenhuma sessao ativa. Execute vigiar_conectar() primeiro.")
    }
    dados <- vigiar_baixar(tabela, ...)
  }
  if (is.null(dados)) {
    stop("Forneca 'dados' ou 'tabela' para criar um snapshot.")
  }

  tabela <- tabela %||% attr(dados, "vigiar_tabela") %||% "desconhecida"

  parameters <- list(...)
  data_checksum <- vigiar_checksum(dados)
  schema_checksum <- .vigiar_schema_checksum(dados)
  metadata_checksum <- .vigiar_metadata_checksum(
    dados,
    metadata = list(tabela = tabela, parametros = parameters)
  )
  snapshot <- list(
    tabela         = tabela,
    dados          = dados,
    criado_em      = Sys.time(),
    checksum_sha256 = data_checksum,
    data_checksum = data_checksum,
    schema_checksum = schema_checksum,
    metadata_checksum = metadata_checksum,
    canonicalization_version = .VIGIAR_CANONICALIZATION_VERSION,
    n_rows          = nrow(dados),
    n_cols          = ncol(dados),
    colunas         = names(dados),
    r_version       = R.version.string,
    vigiar_version  = as.character(utils::packageVersion("vigiar")),
    platform        = R.version$platform,
    parametros      = parameters,
    sessao_criada   = .vigiar_env$sessao$created_at %||% NA
  )

  if (congelar_esquema && !is.null(.vigiar_env$esquema)) {
    snapshot$esquema <- .vigiar_env$esquema
  }

  class(snapshot) <- "vigiar_snapshot"

  cli::cli_alert_success(paste0(
    "Snapshot criado: {tabela} (",
    nrow(dados), " x ", ncol(dados), ")",
    " SHA256: {substr(snapshot$checksum_sha256, 1, 16)}..."
  ))

  snapshot
}

#' Print method for vigiar_snapshot
#' @param x A vigiar_snapshot object.
#' @param ... Additional arguments (ignored).
#' @export
print.vigiar_snapshot <- function(x, ...) {
  cli::cli_h1("VIGIAR Snapshot")
  cli::cli_text("Tabela: {x$tabela}")
  cli::cli_text("Dimensoes: {x$n_rows} linha(s) x {x$n_cols} coluna(s)")
  cli::cli_text("Criado em: {format(x$criado_em)}")
  cli::cli_text("SHA256: {substr(x$checksum_sha256, 1, 32)}...")
  cli::cli_text("Canonicalization: v{x$canonicalization_version %||% 'legacy'}")
  cli::cli_text("R: {x$r_version}")
  cli::cli_text("vigiar: {x$vigiar_version}")
  cli::cli_text("Plataforma: {x$platform}")
  invisible(x)
}

#' Verify a snapshot's integrity
#'
#' Recomputes the checksum and compares to the stored value.
#'
#' @param snapshot A \code{vigiar_snapshot} object.
#' @return \code{TRUE} if checksums match, \code{FALSE} otherwise.
#' @export
vigiar_verificar_snapshot <- function(snapshot) {
  stopifnot(inherits(snapshot, "vigiar_snapshot"))

  current_data <- vigiar_checksum(snapshot$dados)
  stored_data <- snapshot$data_checksum %||% snapshot$checksum_sha256
  data_ok <- identical(current_data, stored_data)
  version_ok <- identical(
    snapshot$canonicalization_version %||% "legacy",
    .VIGIAR_CANONICALIZATION_VERSION
  )
  schema_ok <- !is.null(snapshot$schema_checksum) && identical(
    .vigiar_schema_checksum(snapshot$dados),
    snapshot$schema_checksum
  )
  metadata_ok <- !is.null(snapshot$metadata_checksum) && identical(
    .vigiar_metadata_checksum(
      snapshot$dados,
      metadata = list(tabela = snapshot$tabela, parametros = snapshot$parametros)
    ),
    snapshot$metadata_checksum
  )
  ok <- data_ok && schema_ok && metadata_ok && version_ok

  if (ok) {
    cli::cli_alert_success("Snapshot integro: checksum confere")
  } else {
    cli::cli_alert_danger(paste0(
      "Snapshot integrity verification failed.",
      "\n  Data: ", data_ok,
      "\n  Schema: ", schema_ok,
      "\n  Metadata: ", metadata_ok,
      "\n  Canonicalization version: ", version_ok
    ))
  }

  invisible(ok)
}

#' Save a snapshot to disk (RDS)
#'
#' @param snapshot A \code{vigiar_snapshot} object.
#' @param caminho File path (will add .rds if absent).
#' @param overwrite If \code{FALSE}, errors if file exists.
#' @return Invisibly, the file path.
#' @export
vigiar_salvar_snapshot <- function(snapshot, caminho, overwrite = FALSE) {
  stopifnot(inherits(snapshot, "vigiar_snapshot"))

  if (!grepl("\\.rds$", caminho, ignore.case = TRUE)) {
    caminho <- paste0(caminho, ".rds")
  }
  if (file.exists(caminho) && !overwrite) {
    stop("Arquivo ja existe: ", caminho, ". Use overwrite = TRUE.")
  }

  dir.create(dirname(caminho), showWarnings = FALSE, recursive = TRUE)
  saveRDS(snapshot, caminho)
  cli::cli_alert_success("Snapshot salvo: {caminho}")
  invisible(caminho)
}

#' Load a snapshot from disk
#'
#' @param caminho File path to an .rds snapshot.
#' @return A \code{vigiar_snapshot} object.
#' @export
vigiar_carregar_snapshot <- function(caminho) {
  if (!file.exists(caminho)) {
    stop("Arquivo nao encontrado: ", caminho)
  }
  snapshot <- readRDS(caminho)
  if (!inherits(snapshot, "vigiar_snapshot")) {
    stop("O arquivo nao contem um vigiar_snapshot valido.")
  }
  cli::cli_alert_info("Snapshot carregado: {snapshot$tabela}")
  snapshot
}

#' Compare two snapshots
#'
#' Compares two snapshots (e.g., old vs new) and reports
#' differences in dimensions, columns, and checksums.
#'
#' @param snapshot1 A vigiar_snapshot object.
#' @param snapshot2 Another vigiar_snapshot object to compare against.
#' @return Invisibly, a list of differences.
#' @export
vigiar_comparar_snapshots <- function(snapshot1, snapshot2) {
  stopifnot(inherits(snapshot1, "vigiar_snapshot"))
  stopifnot(inherits(snapshot2, "vigiar_snapshot"))

  diffs <- list()

  cli::cli_h1("Comparacao de Snapshots")
  cli::cli_text("Snapshot 1: {snapshot1$tabela} ({snapshot1$criado_em})")
  cli::cli_text("Snapshot 2: {snapshot2$tabela} ({snapshot2$criado_em})")
  cli::cli_rule()

  # Dimensions
  if (snapshot1$n_rows != snapshot2$n_rows) {
    delta <- snapshot2$n_rows - snapshot1$n_rows
    cli::cli_alert_info("Linhas: {snapshot1$n_rows} -> {snapshot2$n_rows} ({if(delta > 0) '+' else ''}{delta})")
    diffs$n_rows <- delta
  } else {
    cli::cli_alert_success("Linhas: identicas ({snapshot1$n_rows})")
  }

  if (snapshot1$n_cols != snapshot2$n_cols) {
    cli::cli_alert_info("Colunas: {snapshot1$n_cols} -> {snapshot2$n_cols}")
    diffs$n_cols <- TRUE
  }

  # Column changes
  cols_removed <- setdiff(snapshot1$colunas, snapshot2$colunas)
  cols_added <- setdiff(snapshot2$colunas, snapshot1$colunas)

  if (length(cols_removed) > 0) {
    cli::cli_alert_warning("Colunas removidas: {paste(cols_removed, collapse=', ')}")
    diffs$cols_removed <- cols_removed
  }
  if (length(cols_added) > 0) {
    cli::cli_alert_info("Colunas novas: {paste(cols_added, collapse=', ')}")
    diffs$cols_added <- cols_added
  }
  if (length(cols_removed) == 0 && length(cols_added) == 0) {
    cli::cli_alert_success("Colunas: identicas")
  }

  # Checksums
  if (snapshot1$checksum_sha256 != snapshot2$checksum_sha256) {
    cli::cli_alert_warning("Checksum diferente (dados mudaram)")
    diffs$checksum_changed <- TRUE
  } else {
    cli::cli_alert_success("Checksum identico (dados inalterados)")
    diffs$checksum_changed <- FALSE
  }

  invisible(diffs)
}

# ===============================================================================
# Local cache
# ===============================================================================

.VIGIAR_CACHE_KEY_VERSION <- "2"

.vigiar_normalize_cache_value <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) {
      x <- x[order(names(x))]
    }
    return(lapply(x, .vigiar_normalize_cache_value))
  }
  if (is.factor(x)) {
    return(as.character(x))
  }
  x
}

.vigiar_cache_key <- function(tabela, args = list()) {
  schema_hash <- .vigiar_schema_hash(tabela)
  payload <- list(
    cache_key_version = .VIGIAR_CACHE_KEY_VERSION,
    canonicalization_version = .VIGIAR_CANONICALIZATION_VERSION,
    package_version = as.character(utils::packageVersion("vigiar")),
    tabela = tabela,
    schema_hash = schema_hash,
    query = .vigiar_normalize_cache_value(args)
  )
  list(
    hash = .vigiar_sha256_object(payload),
    payload = payload,
    schema_hash = schema_hash
  )
}

#' Local cache directory for VIGIAR data
#'
#' Returns or sets the cache directory. When set, \code{vigiar_baixar_com_cache()}
#' stores downloads and reuses them on subsequent calls.
#'
#' @param dir Path to cache directory. If \code{NULL}, returns current.
#'   Use \code{"auto"} for platform-appropriate default.
#' @return The cache directory path (invisibly if setting).
#' @export
vigiar_cache_dir <- function(dir = NULL) {
  if (is.null(dir)) {
    cache_dir <- .vigiar_env$cache_dir
    if (is.null(cache_dir)) {
      cache_dir <- file.path(tools::R_user_dir("vigiar", "cache"))
    }
    return(cache_dir)
  }

  if (dir == "auto") {
    dir <- file.path(tools::R_user_dir("vigiar", "cache"))
  }

  .vigiar_env$cache_dir <- dir
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  cli::cli_alert_info("Cache VIGIAR: {dir}")
  invisible(dir)
}

#' Download with local cache
#'
#' Downloads a table, storing the result in the local cache directory.
#' Subsequent calls with the same table return the cached copy.
#'
#' @param tabela Table name.
#' @param max_age Maximum cache age. Can be a \code{difftime}, number of
#'   seconds, or \code{Inf} (never expire).
#' @param refresh If \code{TRUE}, forces a new download.
#' @param ... Additional arguments passed to \code{vigiar_baixar()}.
#' @return A tibble (cached or freshly downloaded).
#' @export
vigiar_baixar_com_cache <- function(tabela, max_age = 86400,
                                     refresh = FALSE, ...) {
  if (is.null(.vigiar_env$sessao)) {
    stop("Nenhuma sessao ativa. Execute vigiar_conectar() primeiro.")
  }

  cache_dir <- .vigiar_env$cache_dir
  if (is.null(cache_dir)) {
    cache_dir <- file.path(tools::R_user_dir("vigiar", "cache"))
    .vigiar_env$cache_dir <- cache_dir
  }

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  args <- list(...)
  key <- .vigiar_cache_key(tabela, args)
  safe_table <- gsub("[^A-Za-z0-9_.-]", "_", tabela)
  cache_file <- file.path(
    cache_dir,
    paste0("cache-", safe_table, "-", substr(key$hash, 1L, 24L), ".rds")
  )

  # Check cache
  if (!refresh && file.exists(cache_file)) {
    cache_info <- file.info(cache_file)
    cache_age <- difftime(Sys.time(), cache_info$mtime, units = "secs")

    if (is.numeric(max_age)) {
      max_age <- as.difftime(max_age, units = "secs")
    }

    if (cache_age <= max_age) {
      cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
      checksum_ok <- tryCatch(
        identical(cached$checksum, vigiar_checksum(cached$dados)),
        error = function(e) FALSE
      )
      valid_cache <- inherits(cached, "vigiar_cached_data") &&
        identical(cached$cache_key, key$hash) && checksum_ok
      if (valid_cache) {
        cli::cli_alert_success(paste0(
          "Cache hit: {tabela} (",
          round(as.numeric(cache_age, units="mins"), 1), "min atras)"
        ))
        .vigiar_log(
          "INFO", "Cache hit", table = tabela,
          metadata = list(cache_key = key$hash, age_seconds = as.numeric(cache_age))
        )
        dados <- cached$dados
        attr(dados, "vigiar_cache_status") <- "hit"
        attr(dados, "vigiar_cache_key") <- key$hash
        attr(dados, "vigiar_cache_timestamp") <- cached$timestamp
        attr(dados, "vigiar_cache_age_seconds") <- as.numeric(cache_age)
        return(dados)
      }
      .vigiar_log(
        "WARN", "Cache entry failed provenance or checksum validation",
        table = tabela, metadata = list(cache_key = key$hash)
      )
    }
  }

  # Download fresh
  cli::cli_alert_info("Cache miss: baixando '{tabela}'...")
  .vigiar_log(
    "INFO", "Cache miss", table = tabela,
    metadata = list(cache_key = key$hash)
  )
  dados <- do.call(vigiar_baixar, c(list(tabela = tabela), args))
  attr(dados, "vigiar_cache_status") <- "miss"
  attr(dados, "vigiar_cache_key") <- key$hash
  attr(dados, "vigiar_cache_timestamp") <- Sys.time()
  attr(dados, "vigiar_cache_age_seconds") <- 0

  # Cache it
  cached <- list(
    dados     = dados,
    tabela    = tabela,
    timestamp = Sys.time(),
    checksum  = vigiar_checksum(dados),
    n_rows    = nrow(dados),
    n_cols    = ncol(dados),
    cache_key = key$hash,
    cache_key_payload = key$payload,
    schema_hash = key$schema_hash,
    package_version = as.character(utils::packageVersion("vigiar")),
    canonicalization_version = .VIGIAR_CANONICALIZATION_VERSION
  )
  class(cached) <- "vigiar_cached_data"
  saveRDS(cached, cache_file)
  cli::cli_alert_success("Cache salvo: {tabela}")

  dados
}

#' List cached tables
#'
#' @return A tibble with cache contents: tabela, linhas, colunas, idade, checksum.
#' @export
vigiar_cache_info <- function() {
  cache_dir <- .vigiar_env$cache_dir
  if (is.null(cache_dir) || !dir.exists(cache_dir)) {
    cli::cli_alert_info("Cache vazio ou nao configurado.")
    return(tibble::tibble(
      tabela = character(0), linhas = integer(0), colunas = integer(0),
      idade = character(0), checksum = character(0)
    ))
  }

  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) {
    cli::cli_alert_info("Cache vazio.")
    return(tibble::tibble(
      tabela = character(0), linhas = integer(0), colunas = integer(0),
      idade = character(0), checksum = character(0)
    ))
  }

  entries <- lapply(files, function(f) {
    tryCatch({
      c <- readRDS(f)
      info <- file.info(f)
      idade <- difftime(Sys.time(), info$mtime, units = "mins")
      list(
        tabela   = c$tabela %||% basename(tools::file_path_sans_ext(f)),
        linhas   = c$n_rows %||% NA_integer_,
        colunas  = c$n_cols %||% NA_integer_,
        idade    = paste0(round(as.numeric(idade), 1), " min"),
        checksum = substr(c$checksum %||% "", 1, 16)
      )
    }, error = function(e) {
      list(tabela = basename(f), linhas = NA, colunas = NA,
           idade = "? min", checksum = "ERRO")
    })
  })

  do.call(rbind.data.frame, entries) |>
    tibble::as_tibble()
}

#' Clear the local cache
#'
#' @param tabelas Character vector of table names. \code{NULL} = all.
#' @param max_age Maximum age to keep (e.g., \code{86400} for 1 day).
#'   \code{NULL} = no age filter.
#' @export
vigiar_limpar_cache <- function(tabelas = NULL, max_age = NULL) {
  cache_dir <- .vigiar_env$cache_dir
  if (is.null(cache_dir) || !dir.exists(cache_dir)) {
    cli::cli_alert_info("Cache nao configurado.")
    return(invisible())
  }

  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)

  if (is.null(tabelas) && is.null(max_age)) {
    # Clear all
    file.remove(files)
    cli::cli_alert_success("Cache completamente limpo ({length(files)} arquivos).")
    return(invisible())
  }

  removed <- 0L
  for (f in files) {
    remove <- FALSE
    base <- tools::file_path_sans_ext(basename(f))
    cached_table <- tryCatch(readRDS(f)$tabela, error = function(e) base)

    if (!is.null(tabelas) && cached_table %in% tabelas) remove <- TRUE
    if (!is.null(max_age)) {
      info <- file.info(f)
      idade <- difftime(Sys.time(), info$mtime, units = "secs")
      if (idade > max_age) remove <- TRUE
    }

    if (remove) {
      file.remove(f)
      removed <- removed + 1L
    }
  }

  cli::cli_alert_success("Cache limpo: {removed} arquivos removidos.")
  invisible(removed)
}

# ===============================================================================
# Schema version locking
# ===============================================================================

#' Lock the current schema for reproducibility
#'
#' Saves the entire conceptual schema (table names, column names,
#' types) as a lock file. Later, \code{vigiar_esquema_verificar()}
#' compares against this lock to detect changes.
#'
#' @param caminho File path (JSON).
#' @return Invisibly, the lock data.
#' @export
vigiar_esquema_lock <- function(caminho = "vigiar_schema_lock.json") {
  if (is.null(.vigiar_env$esquema)) {
    stop("Nenhuma sessao ativa. Execute vigiar_conectar() primeiro.")
  }

  lock <- list(
    locked_at = format(Sys.time()),
    vigiar_version = as.character(utils::packageVersion("vigiar")),
    n_tables  = length(.vigiar_env$esquema),
    tabelas   = names(.vigiar_env$esquema),
    esquema   = .vigiar_env$esquema
  )
  class(lock) <- "vigiar_schema_lock"

  json <- jsonlite::toJSON(lock, auto_unbox = TRUE, pretty = TRUE,
                            null = "null", force = TRUE)

  dir.create(dirname(caminho), showWarnings = FALSE, recursive = TRUE)
  writeLines(json, caminho)
  cli::cli_alert_success("Schema lock salvo: {caminho}")
  cli::cli_alert_info("{lock$n_tables} tabelas congeladas")

  invisible(lock)
}

#' Load a schema lock
#'
#' @param caminho Path to a lock file.
#' @return A \code{vigiar_schema_lock} object.
#' @export
vigiar_esquema_carregar_lock <- function(caminho = "vigiar_schema_lock.json") {
  if (!file.exists(caminho)) {
    stop("Arquivo lock nao encontrado: ", caminho)
  }
  lock <- jsonlite::fromJSON(caminho, simplifyVector = FALSE)
  if (is.list(lock$tabelas)) {
    lock$tabelas <- unname(unlist(lock$tabelas, use.names = FALSE))
  }
  class(lock) <- "vigiar_schema_lock"
  lock
}

#' Verify schema against a lock
#'
#' Compares the live schema against a lock file and reports differences.
#'
#' @param lock_path Path to lock file, or a \code{vigiar_schema_lock} object.
#' @param error If \code{TRUE}, error when differences are found.
#' @return Invisibly, a list of differences (empty if identical).
#' @export
vigiar_esquema_verificar <- function(lock_path = "vigiar_schema_lock.json", error = FALSE) {
  if (is.null(.vigiar_env$esquema)) {
    stop("Nenhuma sessao ativa. Execute vigiar_conectar() primeiro.")
  }

  if (is.character(lock_path)) {
    lock <- vigiar_esquema_carregar_lock(lock_path)
  } else {
    lock <- lock_path
  }

  live_tables <- names(.vigiar_env$esquema)
  locked_tables <- lock$tabelas

  cli::cli_h1("Verificacao de Schema Lock")
  cli::cli_text("Lock criado em: {lock$locked_at}")
  cli::cli_text("Tabelas: {length(locked_tables)} (lock) vs {length(live_tables)} (live)")
  cli::cli_rule()

  diffs <- list()

  # New tables
  new_tables <- setdiff(live_tables, locked_tables)
  if (length(new_tables) > 0) {
    cli::cli_alert_info("Tabelas NOVAS: {paste(new_tables, collapse=', ')}")
    diffs$new_tables <- new_tables
  }

  # Removed tables
  removed_tables <- setdiff(locked_tables, live_tables)
  if (length(removed_tables) > 0) {
    cli::cli_alert_warning("Tabelas REMOVIDAS: {paste(removed_tables, collapse=', ')}")
    diffs$removed_tables <- removed_tables
  }

  # Column changes in common tables
  common <- intersect(live_tables, locked_tables)
  col_changes <- list()
  type_changes <- list()
  order_changes <- list()

  for (tab in common) {
    live_cols <- names(.vigiar_env$esquema[[tab]])
    lock_cols <- names(lock$esquema[[tab]] %||% list())

    added <- setdiff(live_cols, lock_cols)
    removed <- setdiff(lock_cols, live_cols)

    if (setequal(live_cols, lock_cols) && !identical(live_cols, lock_cols)) {
      order_changes[[tab]] <- list(locked = lock_cols, live = live_cols)
    }

    if (length(added) > 0 || length(removed) > 0) {
      col_changes[[tab]] <- list(added = added, removed = removed)
    }

    shared <- intersect(live_cols, lock_cols)
    changed <- lapply(shared, function(col) {
      live_type <- .vigiar_schema_column_type(.vigiar_env$esquema[[tab]][[col]])
      lock_type <- .vigiar_schema_column_type(lock$esquema[[tab]][[col]])
      if (is.na(live_type) || is.na(lock_type) || identical(live_type, lock_type)) {
        return(NULL)
      }
      list(column = col, live = live_type, locked = lock_type)
    })
    changed <- changed[!vapply(changed, is.null, logical(1))]
    if (length(changed) > 0) {
      type_changes[[tab]] <- changed
    }
  }

  if (length(col_changes) > 0) {
    cli::cli_alert_warning("Mudancas de colunas em {length(col_changes)} tabela(s):")
    for (tab in names(col_changes)) {
      c <- col_changes[[tab]]
      if (length(c$added) > 0) {
        cli::cli_text("  {tab}: +{paste(c$added, collapse=', ')}")
      }
      if (length(c$removed) > 0) {
        cli::cli_text("  {tab}: -{paste(c$removed, collapse=', ')}")
      }
    }
    diffs$col_changes <- col_changes
  }

  if (length(type_changes) > 0) {
    cli::cli_alert_warning("Mudancas de tipo em {length(type_changes)} tabela(s):")
    for (tab in names(type_changes)) {
      for (change in type_changes[[tab]]) {
        cli::cli_text(
          "  {tab}.{change$column}: {change$locked} -> {change$live}"
        )
      }
    }
    diffs$type_changes <- type_changes
  }
  if (length(order_changes) > 0) {
    cli::cli_alert_warning("Column order changed in {length(order_changes)} table(s).")
    diffs$order_changes <- order_changes
  }

  incompatible_columns <- any(vapply(col_changes, function(change) {
    length(change$removed) > 0L
  }, logical(1)))
  compatibility_status <- if (
    length(removed_tables) > 0L || incompatible_columns || length(type_changes) > 0L
  ) "fail" else "pass"
  attr(diffs, "equality_status") <- if (length(diffs) == 0L) "pass" else "fail"
  attr(diffs, "compatibility_status") <- compatibility_status

  if (length(diffs) == 0) {
    cli::cli_alert_success("Schema identico ao lock. Reproducibilidade garantida!")
  } else {
    cli::cli_alert_danger("Schema MUDOU. Reproducibilidade comprometida.")
    if (isTRUE(error)) {
      stop("Schema changed relative to lock.", call. = FALSE)
    }
  }

  invisible(diffs)
}

#' Verify critical VIGIAR schema columns
#'
#' Checks only the table and column subset required for RJ PM2.5, municipality,
#' coordinate, and population workflows. Extra live tables or columns are
#' ignored; missing critical columns or type changes are failures.
#'
#' @param lock_path Critical schema lock path. Defaults to the package lock.
#' @param error If \code{TRUE}, error when critical differences are found.
#' @return Invisibly, a list of critical schema differences.
#' @export
vigiar_esquema_verificar_critico <- function(lock_path = NULL, error = TRUE) {
  if (is.null(.vigiar_env$esquema)) {
    stop("Nenhuma sessao ativa. Execute vigiar_conectar() primeiro.")
  }

  if (is.null(lock_path)) {
    lock_path <- system.file(
      "extdata", "vigiar_schema_critical_lock.json", package = "vigiar"
    )
    if (!nzchar(lock_path)) {
      stop("Critical schema lock not found in package extdata.", call. = FALSE)
    }
  }

  lock <- if (is.character(lock_path)) {
    if (length(lock_path) != 1 || !nzchar(lock_path)) {
      stop("Critical schema lock path must identify one file.", call. = FALSE)
    }
    vigiar_esquema_carregar_lock(lock_path)
  } else {
    lock_path
  }

  diffs <- list()
  missing_tables <- setdiff(lock$tabelas, names(.vigiar_env$esquema))
  if (length(missing_tables) > 0) {
    diffs$missing_tables <- missing_tables
  }

  missing_columns <- list()
  type_changes <- list()
  for (tab in intersect(lock$tabelas, names(.vigiar_env$esquema))) {
    lock_cols <- names(lock$esquema[[tab]] %||% list())
    live_cols <- names(.vigiar_env$esquema[[tab]])
    removed <- setdiff(lock_cols, live_cols)
    if (length(removed) > 0) {
      missing_columns[[tab]] <- removed
    }

    shared <- intersect(lock_cols, live_cols)
    changed <- lapply(shared, function(col) {
      live_type <- .vigiar_schema_column_type(.vigiar_env$esquema[[tab]][[col]])
      lock_type <- .vigiar_schema_column_type(lock$esquema[[tab]][[col]])
      if (is.na(live_type) || is.na(lock_type) || identical(live_type, lock_type)) {
        return(NULL)
      }
      list(column = col, live = live_type, locked = lock_type)
    })
    changed <- changed[!vapply(changed, is.null, logical(1))]
    if (length(changed) > 0) {
      type_changes[[tab]] <- changed
    }
  }

  if (length(missing_columns) > 0) {
    diffs$missing_columns <- missing_columns
  }
  if (length(type_changes) > 0) {
    diffs$type_changes <- type_changes
  }

  if (length(diffs) == 0) {
    cli::cli_alert_success("Critical VIGIAR schema columns match the lock.")
  } else {
    cli::cli_alert_danger("Critical VIGIAR schema changed.")
    if (isTRUE(error)) {
      stop("Critical schema changed relative to lock.", call. = FALSE)
    }
  }

  invisible(diffs)
}

.vigiar_schema_column_type <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  if (is.list(x)) {
    out <- x$tipo %||% x$type %||% x$Type %||% x$T
  } else {
    out <- x
  }
  if (length(out) == 0 || is.null(out)) {
    return(NA_character_)
  }
  as.character(out[[1]])
}
