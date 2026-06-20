# Package: vigiar
# S3 classes for typed VIGIAR data
#
# Following the microdatasus pattern, each data domain gets its own
# S3 class that inherits from vigiar_tbl → tibble → data.frame.

#' Create a typed VIGIAR tibble
#'
#' @param x A data frame or tibble.
#' @param subclass Character vector of additional class names.
#' @param tabela Original table name.
#' @param metadados List of metadata attributes.
#' @return An object of class \code{vigiar_tbl} (and subclasses).
#' @keywords internal
new_vigiar_tbl <- function(x, subclass = character(0), tabela = NULL,
                            metadados = NULL) {
  x <- tibble::as_tibble(x)
  class(x) <- c(subclass, "vigiar_tbl", class(x))
  attr(x, "vigiar_tabela")    <- tabela
  attr(x, "vigiar_metadados") <- metadados
  attr(x, "vigiar_processado_em") <- Sys.time()
  x
}

# ── Print method ──────────────────────────────────────────────────────────────

#' @export
print.vigiar_tbl <- function(x, ...) {
  tabela <- attr(x, "vigiar_tabela") %||% "desconhecida"
  processado <- attr(x, "vigiar_processado_em")
  n_rows <- nrow(x)
  n_cols <- ncol(x)

  cat(sprintf(
    "# VIGIAR tibble: %s  |  %d linhas x %d colunas\n",
    tabela, n_rows, n_cols
  ))
  if (!is.null(processado)) {
    cat(sprintf("# Processado em: %s\n", format(processado)))
  }
  cat("\n")
  NextMethod()
}

# ── Summary method ────────────────────────────────────────────────────────────

#' @export
summary.vigiar_tbl <- function(object, ...) {
  tabela <- attr(object, "vigiar_tabela") %||% "desconhecida"
  cat(sprintf("Resumo: %s\n", tabela))
  cat(strrep("-", 50), "\n")
  cat(sprintf("Linhas:  %d\n", nrow(object)))
  cat(sprintf("Colunas: %d\n", ncol(object)))
  cat(sprintf("Classes: %s\n", paste(class(object), collapse = ", ")))

  # Missing values
  na_counts <- vapply(object, function(col) sum(is.na(col)), integer(1))
  if (any(na_counts > 0)) {
    cat("\nValores ausentes:\n")
    for (nm in names(na_counts[na_counts > 0])) {
      cat(sprintf("  %-30s %d (%.1f%%)\n",
                  nm, na_counts[[nm]],
                  100 * na_counts[[nm]] / nrow(object)))
    }
  }

  invisible(object)
}

# ── Validation method ─────────────────────────────────────────────────────────

#' @export
validate.vigiar_tbl <- function(x, ...) {
  issues <- list()

  # Check for required metadata
  if (is.null(attr(x, "vigiar_tabela"))) {
    issues$missing_table_attr <- "Atributo 'vigiar_tabela' ausente"
  }

  # Check for empty data
  if (nrow(x) == 0) {
    issues$empty <- "Tabela vazia (0 linhas)"
  }

  if (length(issues) > 0) {
    warning("Problemas de validação encontrados: ",
            paste(names(issues), collapse = ", "))
  }

  invisible(issues)
}
