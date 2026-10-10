# vigiar: lifecycle registry consistency and behaviour

test_that("vigiar_lifecycle returns the full registry as a tibble", {
  reg <- vigiar_lifecycle()

  expect_s3_class(reg, "tbl_df")
  expect_named(reg, c("fun", "status"))
  expect_gt(nrow(reg), 80)
  expect_false(anyDuplicated(reg$fun) > 0)
  expect_true(all(reg$status %in% c("stable", "experimental", "deprecated")))
})

test_that("vigiar_lifecycle returns statuses for a subset of functions", {
  res <- vigiar_lifecycle(c("vigiar_conectar", "vigiar_benchmark"))

  expect_type(res, "character")
  expect_named(res, c("vigiar_conectar", "vigiar_benchmark"))
  expect_equal(unname(res), c("stable", "experimental"))
})

test_that("vigiar_lifecycle errors for unregistered functions", {
  expect_error(
    vigiar_lifecycle("funcao_que_nao_existe"),
    "sem status de lifecycle"
  )
})

test_that("every exported function has a lifecycle status", {
  exports <- getNamespaceExports("vigiar")
  reg <- vigiar_lifecycle()

  missing <- setdiff(exports, reg$fun)
  expect_equal(
    missing, character(0),
    info = paste("Sem status de lifecycle:", paste(missing, collapse = ", "))
  )
})
