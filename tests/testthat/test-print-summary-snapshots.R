# Snapshots for the S3 print and summary methods of vigiar_tbl.
#
# These methods are part of the public experience (they format diagnostics,
# audits and processed tables), so their output is locked against accidental
# changes.

construir_tbl <- function() {
  x <- vigiar:::new_vigiar_tbl(
    tibble::tibble(
      cod_municipio = c(330455L, 330010L, NA_integer_),
      pm25_media_anual = c(12.3, 10.1, NA_real_)
    ),
    subclass = "vigiar_pm25",
    tabela = "df_anual",
    metadados = list(uf = "RJ")
  )

  # Freeze the timestamp so the snapshot is deterministic.
  attr(x, "vigiar_processado_em") <- as.POSIXct(
    "2026-01-01 00:00:00",
    tz = "UTC"
  )

  x
}

test_that("print.vigiar_tbl has a stable format", {
  expect_snapshot(print(construir_tbl()))
})

test_that("summary.vigiar_tbl has a stable format", {
  expect_snapshot(summary(construir_tbl()))
})
