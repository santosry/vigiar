# Adversarial and property-style tests for the Power BI DSR parser

.adversarial_dsr <- function(select, dm0, value_dicts = list()) {
  list(results = list(list(result = list(data = list(
    descriptor = list(Select = select),
    dsr = list(DS = list(list(
      PH = list(list(DM0 = dm0)),
      ValueDicts = value_dicts
    )))
  )))))
}

.dsr_select <- function(names, types = rep(1L, length(names))) {
  Map(function(name, type) list(Name = name, Type = type), names, types)
}

test_that("DSR parser fails clearly on malformed structural envelopes", {
  expect_error(.vigiar_parse_dados(list(), "bad"),
               "Malformed Power BI DSR response")
  expect_error(
    .vigiar_parse_dados(
      list(results = list(list(result = list(data = list(dsr = list(DS = list())))))),
      "bad"
    ),
    "DSR dataset"
  )
  expect_error(
    .vigiar_parse_dados(
      list(results = list(list(result = list(data = list(
        dsr = list(DS = list(list(PH = list(list(DM0 = list(list(C = list(1))))))))
      ))))),
      "bad"
    ),
    "descriptor"
  )
})

test_that("DSR parser records compact rows without a data predecessor", {
  response <- .adversarial_dsr(
    .dsr_select(c("a", "b"), c(4L, 4L)),
    list(
      list(S = list(list(), list())),
      list(R = 1L, C = list(2L))
    )
  )
  expect_warning(
    parsed <- .vigiar_parse_dados(response, "compact"),
    "no previous data row"
  )
  expect_true(is.na(parsed$a[[1]]))
  expect_equal(parsed$b[[1]], 2L)
  expect_true(length(attr(parsed, "vigiar_parser_issues")) > 0L)
})

test_that("DSR parser detects inconsistent dictionaries and row widths", {
  response <- .adversarial_dsr(
    list(
      list(Name = "label", Type = 1L),
      list(Name = "value", Type = 4L)
    ),
    list(
      list(S = list(list(DN = "D0"), list()), C = list(5L, 1L, 99L))
    ),
    value_dicts = list(D0 = c("a", "b"))
  )
  expect_warning(
    parsed <- .vigiar_parse_dados(response, "dict"),
    "dictionary index|more changed values"
  )
  expect_true(is.na(parsed$label[[1]]))
  expect_equal(parsed$value[[1]], 1L)
})

test_that("DSR parser rejects invalid masks", {
  response <- .adversarial_dsr(
    .dsr_select("a", 4L),
    list(list(R = "not-a-mask", C = list(1L)))
  )
  expect_error(.vigiar_parse_dados(response, "mask"), "repeat mask")
})

test_that("DSR parser reconstructs random valid repeat masks", {
  set.seed(20260711)
  for (iteration in seq_len(40L)) {
    n_rows <- sample(2:12, 1)
    n_cols <- sample(1:6, 1)
    values <- matrix(
      sample(letters, n_rows * n_cols, replace = TRUE),
      nrow = n_rows,
      ncol = n_cols
    )
    entries <- vector("list", n_rows)
    entries[[1]] <- list(C = as.list(values[1, ]))
    for (i in 2:n_rows) {
      repeat_flags <- values[i, ] == values[i - 1L, ]
      mask <- sum(bitwShiftL(1L, which(repeat_flags) - 1L))
      entries[[i]] <- list(
        R = as.integer(mask),
        C = as.list(values[i, !repeat_flags, drop = TRUE])
      )
    }
    response <- .adversarial_dsr(
      .dsr_select(paste0("v", seq_len(n_cols))),
      entries
    )
    parsed <- .vigiar_parse_dados(response, paste0("fuzz-", iteration))
    expect_equal(unname(as.matrix(parsed)), unname(values))
  }
})
