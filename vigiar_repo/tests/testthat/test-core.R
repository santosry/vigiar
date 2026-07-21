library(testthat)
library(vigiar)

test_that("uuid_v4 gera formato correto", {
  u <- uuid_v4()
  expect_match(u, "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")
})

test_that(".vigiar_tipo_dado mapeia corretamente", {
  expect_equal(.vigiar_tipo_dado(1), "character")
  expect_equal(.vigiar_tipo_dado(3), "numeric")
  expect_equal(.vigiar_tipo_dado(4), "integer")
  expect_equal(.vigiar_tipo_dado(7), "POSIXct")
})

test_that("%||% funciona", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
  expect_equal(NA %||% 3, NA)
})

test_that(".vigiar_construir_query gera estrutura correta", {
  old_esquema <- .vigiar_env$esquema
  .vigiar_env$esquema <- list(
    teste = list(
      col1 = list(nome = "col1", tipo = "integer"),
      col2 = list(nome = "col2", tipo = "character")
    )
  )
  on.exit({ .vigiar_env$esquema <- old_esquema })

  q <- .vigiar_construir_query("teste", modelo_id = 123)
  expect_equal(q$modelId, 123)
  expect_equal(length(q$queries[[1]]$Query$Commands), 1)
  cmd <- q$queries[[1]]$Query$Commands[[1]]$SemanticQueryDataShapeCommand
  expect_equal(cmd$Query$From[[1]]$Entity, "teste")
  expect_equal(length(cmd$Query$Select), 2)
})

test_that(".vigiar_parse_dados processa formato DSR (DM0 array) com referências", {
  # Simula o formato real: DM0 é array, primeira entrada tem S+C,
  # entradas seguintes têm R+C.
  resposta <- list(
    results = list(list(
      result = list(
        data = list(
          descriptor = list(
            Select = list(
              list(Name = "a", Kind = 1, Depth = 0, Value = "G0"),
              list(Name = "b", Kind = 1, Depth = 0, Value = "G1"),
              list(Name = "c", Kind = 1, Depth = 0, Value = "G2")
            )
          ),
          dsr = list(
            DS = list(list(
              N = "DS0",
              ValueDicts = list(),
              PH = list(list(
                DM0 = list(
                  # Entrada 0: schema + 1ª linha
                  list(
                    S = list(
                      list(N = "G0", T = 4),  # integer
                      list(N = "G1", T = 1),  # text
                      list(N = "G2", T = 3)   # numeric
                    ),
                    C = list(2020L, "SP", 25.5)
                  ),
                  # Entrada 1: R=3 → mantém 2 colunas, C tem 1 valor
                  list(R = 3, C = list(30.1)),
                  # Entrada 2: R=2 → mantém 1 coluna, C tem 2 valores
                  list(R = 2, C = list("RJ", 18.2)),
                  # Entrada 3: linha completa (outra entrada com S)
                  list(
                    S = list(
                      list(N = "G0", T = 4),
                      list(N = "G1", T = 1),
                      list(N = "G2", T = 3)
                    ),
                    C = list(2021L, "MG", 22.0)
                  ),
                  # Entrada 4: R=3 → mantém 2 colunas
                  list(R = 3, C = list(19.5))
                )
              ))
            ))
          )
        )
      )
    ))
  )

  df <- .vigiar_parse_dados(resposta, "teste")

  # 5 linhas esperadas
  expect_equal(nrow(df), 5)
  expect_equal(ncol(df), 3)
  expect_equal(names(df), c("a", "b", "c"))

  # Linha 1: completa de entrada 0
  expect_equal(df$a[1], 2020L)
  expect_equal(df$b[1], "SP")
  expect_equal(df$c[1], 25.5)

  # Linha 2: R=3, mantém colunas 1-2 (a=2020, b="SP"), C=[30.1] → col 3
  expect_equal(df$a[2], 2020L)
  expect_equal(df$b[2], "SP")
  expect_equal(df$c[2], 30.1)

  # Linha 3: R=2, mantém coluna 1 (a=2020), C=["RJ", 18.2] → cols 2-3
  expect_equal(df$a[3], 2020L)
  expect_equal(df$b[3], "RJ")
  expect_equal(df$c[3], 18.2)

  # Linha 4: nova linha completa
  expect_equal(df$a[4], 2021L)
  expect_equal(df$b[4], "MG")
  expect_equal(df$c[4], 22.0)

  # Linha 5: R=3, mantém colunas 1-2, C=[19.5]
  expect_equal(df$a[5], 2021L)
  expect_equal(df$b[5], "MG")
  expect_equal(df$c[5], 19.5)
})

test_that(".vigiar_parse_dados resolve dicionários de texto", {
  resposta <- list(
    results = list(list(
      result = list(
        data = list(
          descriptor = list(
            Select = list(
              list(Name = "id", Kind = 1, Depth = 0, Value = "G0"),
              list(Name = "estado", Kind = 1, Depth = 0, Value = "G1")
            )
          ),
          dsr = list(
            DS = list(list(
              N = "DS0",
              ValueDicts = list(
                D0 = c("São Paulo", "Rio de Janeiro", "Minas Gerais")
              ),
              PH = list(list(
                DM0 = list(
                  list(
                    S = list(
                      list(N = "G0", T = 4),
                      list(N = "G1", T = 1, DN = "D0")
                    ),
                    C = list(1L, 1L)   # índice 1 → "São Paulo"
                  ),
                  list(R = 2, C = list(2L)),  # índice 2 → "Rio de Janeiro"
                  list(R = 2, C = list(3L))   # índice 3 → "Minas Gerais"
                )
              ))
            ))
          )
        )
      )
    ))
  )

  df <- .vigiar_parse_dados(resposta, "teste")
  expect_equal(df$estado, c("São Paulo", "Rio de Janeiro", "Minas Gerais"))
})

test_that(".vigiar_parse_dados lida com tabela vazia", {
  resposta <- list(
    results = list(list(
      result = list(data = list())
    ))
  )
  df <- .vigiar_parse_dados(resposta, "vazia")
  expect_equal(nrow(df), 0)
})
