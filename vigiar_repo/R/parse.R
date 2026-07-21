# Package: vigiar
# Parsing do formato de dados comprimido do Power BI (DSR - Data Shape Response)

#' Converte a resposta queryData em um data.frame
#'
#' O Power BI retorna dados no formato DSR (Data Shape Response) onde
#' os dados são comprimidos: linhas consecutivas compartilham valores
#' das colunas anteriores via referência (R). Colunas de texto podem
#' usar dicionários (ValueDicts) para economia de espaço.
#'
#' @param resposta Lista R com a resposta da API queryData
#' @param tabela Nome da tabela sendo consultada
#' @return data.frame com os dados descomprimidos e resolvidos
#' @keywords internal
.vigiar_parse_dados <- function(resposta, tabela) {
  data_section <- resposta$results[[1]]$result$data

  if (is.null(data_section$dsr)) {
    warning("Resposta não contém dados (dsr). Tabela pode estar vazia.")
    return(data.frame())
  }

  ds <- data_section$dsr$DS[[1]]
  descriptor <- data_section$descriptor

  # Verificar se há dados
  if (is.null(ds$PH) || length(ds$PH) == 0) {
    warning("Resposta sem dados (PH vazio).")
    return(data.frame())
  }

  ph <- ds$PH[[1]]  # Primeiro (e geralmente único) Projection Header

  # DM0 é um ARRAY de objetos, não um objeto único
  dm0_entries <- ph$DM0
  if (is.null(dm0_entries) || length(dm0_entries) == 0) {
    warning("DM0 vazio. Tabela pode estar vazia.")
    return(data.frame())
  }

  # O primeiro DM0 contém o schema (S) + primeira linha de dados
  first_entry <- dm0_entries[[1]]
  schema <- first_entry$S
  n_cols <- length(schema)

  # Mapear nomes e tipos das colunas
  col_names <- sapply(descriptor$Select, `[[`, "Name")
  col_types <- sapply(schema, function(s) {
    list(type = s$T, dn = s$DN %||% NULL)
  }, simplify = FALSE)

  # Extrair dicionários de texto (ValueDicts no nível DS)
  value_dicts <- ds$ValueDicts %||% list()

  # Função para resolver valor de dicionário
  resolve_dict <- function(val, col_idx) {
    dn <- col_types[[col_idx]]$dn
    if (is.null(dn) || is.null(value_dicts[[dn]])) return(val)
    dict <- value_dicts[[dn]]
    if (is.numeric(val) && val >= 1 && val <= length(dict)) {
      return(dict[[val]])
    }
    return(val)
  }

  # Reconstruir todas as linhas
  # Formato DM0:
  #   Entrada 0: {S: [schema], C: [valores da 1ª linha]}
  #   Entradas 1..N: {R: n, C: [novos valores]}
  #   R é 1-indexado: keep = R - 1 colunas da linha anterior
  #   C fornece os valores das colunas restantes (posição keep em diante)

  prev_row <- NULL
  rows <- list()

  for (i in seq_along(dm0_entries)) {
    entry <- dm0_entries[[i]]

    if (!is.null(entry$S)) {
      # Primeira entrada: linha completa
      values <- entry$C
    } else {
      # Entrada com referência
      r <- entry$R       # 1-indexed: colunas a MANTER da linha anterior
      new_vals <- entry$C

      keep_count <- r - 1   # número de colunas que repetem

      if (is.null(prev_row)) {
        warning(sprintf("Entrada DM0[%d] tem R=%d mas não há linha anterior.", i, r))
        values <- new_vals
      } else {
        # Manter as primeiras (r-1) colunas da linha anterior
        if (keep_count > 0) {
          values <- c(prev_row[1:keep_count], new_vals)
        } else {
          values <- new_vals
        }
      }
    }

    # Garantir número correto de colunas
    if (length(values) < n_cols) {
      values <- c(values, rep(NA, n_cols - length(values)))
    } else if (length(values) > n_cols) {
      values <- values[1:n_cols]
    }

    # Resolver dicionários de texto
    for (j in seq_len(n_cols)) {
      values[[j]] <- resolve_dict(values[[j]], j)
    }

    prev_row <- values
    rows[[length(rows) + 1]] <- values
  }

  if (length(rows) == 0) {
    return(data.frame())
  }

  # Converter lista de linhas para data.frame
  # Primeiro, construir matriz de caracteres para evitar coerção prematura
  n_rows <- length(rows)
  df <- as.data.frame(
    matrix(nrow = n_rows, ncol = n_cols),
    stringsAsFactors = FALSE
  )
  names(df) <- col_names

  for (i in seq_len(n_rows)) {
    for (j in seq_len(n_cols)) {
      val <- rows[[i]][[j]]
      if (is.null(val)) val <- NA
      df[i, j] <- val
    }
  }

  # Aplicar tipos apropriados
  for (j in seq_len(n_cols)) {
    df[[j]] <- .vigiar_converter_coluna(df[[j]], col_types[[j]]$type)
  }

  df
}

#' Converte uma coluna para o tipo R apropriado
#' @param x Vetor com os valores
#' @param type_code Código de tipo do Power BI
#' @return Vetor convertido
#' @keywords internal
.vigiar_converter_coluna <- function(x, type_code) {
  # Converter NULLs para NA
  x <- lapply(x, function(v) if (is.null(v)) NA else v)

  switch(as.character(type_code),
    "1" = as.character(unlist(x)),       # Text
    "2" = as.numeric(unlist(x)),         # Decimal/Currency
    "3" = as.numeric(unlist(x)),         # Double
    "4" = as.integer(unlist(x)),         # Integer
    "5" = as.logical(unlist(x)),         # Boolean
    "6" = as.Date(unlist(x)),            # Date
    "7" = as.POSIXct(unlist(x),          # DateTime
                     origin = "1970-01-01",
                     tz = "UTC"),
    "8" = as.numeric(unlist(x)),         # Int64 (as numeric in R)
    as.character(unlist(x))              # default
  )
}

#' Operador NULL-coalesce: x %||% y retorna x a menos que seja NULL
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
