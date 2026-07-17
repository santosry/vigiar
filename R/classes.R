# Package: vigiar
# S3 classes for typed VIGIAR data
#
# Following the microdatasus pattern, each data domain gets its own
# S3 class that inherits from vigiar_tbl -> tibble -> data.frame.

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

.vigiar_data_attributes <- function(x) {
  attrs <- attributes(x)
  attrs[setdiff(names(attrs), c("names", "row.names", "class"))]
}

.vigiar_restore_data_attributes <- function(x, attrs, overwrite = TRUE) {
  if (length(attrs) == 0L) {
    return(x)
  }
  for (name in names(attrs)) {
    if (isTRUE(overwrite) || is.null(attr(x, name, exact = TRUE))) {
      attr(x, name) <- attrs[[name]]
    }
  }
  x
}

.vigiar_as_tibble_preserve <- function(x) {
  attrs <- .vigiar_data_attributes(x)
  out <- tibble::as_tibble(x)
  .vigiar_restore_data_attributes(out, attrs)
}

# -- Print method --------------------------------------------------------------

#' @export
print.vigiar_tbl <- function(x, ...) {
  tabela <- attr(x, "vigiar_tabela") %||% "unknown"
  processado <- attr(x, "vigiar_processado_em")
  n_rows <- nrow(x)
  n_cols <- ncol(x)

  cat(sprintf(
    "# VIGIAR tibble: %s  |  %d rows x %d columns\n",
    tabela, n_rows, n_cols
  ))
  if (!is.null(processado)) {
  cat(sprintf("# Processed at: %s\n", format(processado)))
  }
  cat("\n")
  NextMethod()
}

# -- Summary method ------------------------------------------------------------

#' @export
summary.vigiar_tbl <- function(object, ...) {
  tabela <- attr(object, "vigiar_tabela") %||% "unknown"
  cat(sprintf("Summary: %s\n", tabela))
  cat(strrep("-", 50), "\n")
  cat(sprintf("Rows:    %d\n", nrow(object)))
  cat(sprintf("Columns: %d\n", ncol(object)))
  cat(sprintf("Classes: %s\n", paste(class(object), collapse = ", ")))

  # Missing values
  na_counts <- vapply(object, function(col) sum(is.na(col)), integer(1))
  if (any(na_counts > 0)) {
    cat("\nMissing values:\n")
    for (nm in names(na_counts[na_counts > 0])) {
      cat(sprintf("  %-30s %d (%.1f%%)\n",
                  nm, na_counts[[nm]],
                  100 * na_counts[[nm]] / nrow(object)))
    }
  }

  invisible(object)
}

# -- Validation method ---------------------------------------------------------

#' Validate a VIGIAR tibble
#'
#' Checks for required metadata attributes and empty data.
#'
#' @param x A vigiar_tbl object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly, a list of validation issues (empty if valid).
#' @export
validate.vigiar_tbl <- function(x, ...) {
  issues <- list()

  # Check for required metadata
  if (is.null(attr(x, "vigiar_tabela"))) {
    issues$missing_table_attr <- "Missing 'vigiar_tabela' attribute"
  }

  # Check for empty data
  if (nrow(x) == 0) {
    issues$empty <- "Empty table (0 rows)"
  }

  if (length(issues) > 0) {
    warning("Validation issues found: ",
            paste(names(issues), collapse = ", "))
  }

  invisible(issues)
}
