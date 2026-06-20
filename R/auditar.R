# Package: vigiar
# Dictionary coverage and validation tools
#
# These functions help maintain consistency between the documented
# variable dictionary and the live Power BI schema.

#' List all documented tables
#'
#' @return Character vector of table IDs present in the dictionary.
#' @export
vigiar_tabelas_documentadas <- function() {
  dict <- vigiar_dicionario()
  unique(dict$table_id)
}

#' Find undocumented variables
#'
#' Compares the live schema against the dictionary and reports
#' any variables present in the data but missing from the dictionary.
#'
#' @return A tibble of undocumented variables (empty if all covered).
#' @export
vigiar_variaveis_nao_documentadas <- function() {
  if (is.null(.vigiar_env$esquema)) {
    stop("Nenhuma sessao ativa. Execute vigiar_conectar() primeiro.")
  }

  dict <- vigiar_dicionario()
  undocumented <- list()

  for (tab in names(.vigiar_env$esquema)) {
    live_cols <- names(.vigiar_env$esquema[[tab]])
    dict_cols <- dict$original_name[dict$table_id == tab]
    missing <- setdiff(live_cols, dict_cols)
    if (length(missing) > 0) {
      for (col in missing) {
        undocumented[[length(undocumented) + 1]] <- data.frame(
          tabela   = tab,
          coluna   = col,
          problema = "Variavel presente nos dados mas ausente no dicionario",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(undocumented) == 0) {
    return(tibble::tibble(
      tabela = character(0), coluna = character(0),
      problema = character(0)
    ))
  }

  tibble::as_tibble(do.call(rbind, undocumented))
}

#' Find documented variables that don't exist in the live schema
#'
#' @return A tibble of orphan variables (empty if none).
#' @export
vigiar_variaveis_orfas <- function() {
  if (is.null(.vigiar_env$esquema)) {
    stop("Nenhuma sessao ativa. Execute vigiar_conectar() primeiro.")
  }

  dict <- vigiar_dicionario()
  orfaos <- list()

  for (tab in unique(dict$table_id)) {
    if (!tab %in% names(.vigiar_env$esquema)) {
      # Entire table is orphan
      orfaos[[length(orfaos) + 1]] <- data.frame(
        tabela   = tab,
        coluna   = "(tabela inteira)",
        problema = "Tabela documentada nao existe no esquema atual",
        stringsAsFactors = FALSE
      )
      next
    }
    live_cols <- names(.vigiar_env$esquema[[tab]])
    dict_cols <- dict$original_name[dict$table_id == tab]
    extra <- setdiff(dict_cols, live_cols)
    if (length(extra) > 0) {
      for (col in extra) {
        orfaos[[length(orfaos) + 1]] <- data.frame(
          tabela   = tab,
          coluna   = col,
          problema = "Variavel documentada nao existe nos dados atuais",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(orfaos) == 0) {
    return(tibble::tibble(
      tabela = character(0), coluna = character(0),
      problema = character(0)
    ))
  }

  tibble::as_tibble(do.call(rbind, orfaos))
}

#' Validate the dictionary against the live schema
#'
#' Runs all dictionary checks and returns a report.
#'
#' @return Invisibly, a list with \code{undocumented}, \code{orfas},
#'   \code{coverage_pct}.
#' @export
vigiar_validar_dicionario <- function() {
  if (is.null(.vigiar_env$esquema)) {
    stop("Nenhuma sessao ativa. Execute vigiar_conectar() primeiro.")
  }

  nao_doc <- vigiar_variaveis_nao_documentadas()
  orfas   <- vigiar_variaveis_orfas()

  dict <- vigiar_dicionario()
  n_dict_cols <- nrow(dict)

  # Count total live columns
  n_live <- sum(vapply(.vigiar_env$esquema, length, integer(1)))
  n_covered <- n_dict_cols - nrow(orfas)
  coverage <- if (n_live > 0) round(100 * n_covered / n_live, 1) else 100

  cat(sprintf("Cobertura do dicionario: %.1f%%\n", coverage))
  cat(sprintf("Variaveis documentadas: %d\n", n_dict_cols))
  cat(sprintf("Variaveis nao documentadas: %d\n", nrow(nao_doc)))
  cat(sprintf("Variaveis orfas (documentadas mas ausentes): %d\n", nrow(orfas)))

  if (nrow(nao_doc) > 0) {
    cat("\n[!]  Variaveis nao documentadas:\n")
    print(nao_doc)
  }
  if (nrow(orfas) > 0) {
    cat("\n[!]  Variaveis orfas:\n")
    print(orfas)
  }

  invisible(list(
    undocumented = nao_doc,
    orfas        = orfas,
    coverage_pct = coverage
  ))
}

#' Compare live schema with dictionary
#'
#' @return A tibble comparing live vs documented columns per table.
#' @export
vigiar_comparar_schema <- function() {
  if (is.null(.vigiar_env$esquema)) {
    stop("Nenhuma sessao ativa. Execute vigiar_conectar() primeiro.")
  }

  dict <- vigiar_dicionario()
  results <- list()

  all_tables <- union(
    names(.vigiar_env$esquema),
    unique(dict$table_id)
  )

  for (tab in sort(all_tables)) {
    live_cols <- if (tab %in% names(.vigiar_env$esquema)) {
      names(.vigiar_env$esquema[[tab]])
    } else {
      character(0)
    }
    dict_cols <- dict$original_name[dict$table_id == tab]
    documented <- intersect(live_cols, dict_cols)

    results[[length(results) + 1]] <- data.frame(
      tabela         = tab,
      colunas_live   = length(live_cols),
      colunas_dict   = length(dict_cols),
      documentadas   = length(documented),
      nao_documentadas = length(setdiff(live_cols, dict_cols)),
      orfas          = length(setdiff(dict_cols, live_cols)),
      coverage       = if (length(live_cols) > 0) {
        round(100 * length(documented) / length(live_cols), 1)
      } else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  tibble::as_tibble(do.call(rbind, results))
}
