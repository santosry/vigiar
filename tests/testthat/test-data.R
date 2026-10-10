# vigiar: built-in sample data and its documented contract

test_that("pm25_rj_sample matches its documented structure", {
  data("pm25_rj_sample", package = "vigiar")

  expect_s3_class(pm25_rj_sample, "tbl_df")
  expect_s3_class(pm25_rj_sample, "vigiar_tbl")
  expect_s3_class(pm25_rj_sample, "vigiar_pm25")

  expect_equal(nrow(pm25_rj_sample), 15)
  expect_equal(ncol(pm25_rj_sample), 7)
  expect_true(all(c(
    "cod_municipio", "ano", "pm25_media_anual", "sigla_uf",
    "pm25_categoria", "latitude", "longitude"
  ) %in% names(pm25_rj_sample)))

  expect_type(pm25_rj_sample$cod_municipio, "integer")
  expect_type(pm25_rj_sample$ano, "integer")
  expect_type(pm25_rj_sample$pm25_media_anual, "double")
  expect_true(all(pm25_rj_sample$sigla_uf == "RJ"))
  expect_true(all(pm25_rj_sample$cod_municipio >= 330010 &
                    pm25_rj_sample$cod_municipio <= 330620))
  expect_true(all(pm25_rj_sample$pm25_media_anual > 0))
})

test_that("pm25_rj_sample carries provenance metadata", {
  data("pm25_rj_sample", package = "vigiar")
  expect_false(is.null(attr(pm25_rj_sample, "vigiar_tabela")))
  expect_false(is.null(attr(pm25_rj_sample, "vigiar_metadados")))
})
