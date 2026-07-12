test_that("software metadata agrees with DESCRIPTION", {
  description <- read.dcf(test_path("..", "..", "DESCRIPTION"))[1, ]
  version <- unname(description[["Version"]])
  repository <- "https://github.com/santosry/vigiar"
  title <- "vigiar: VIGIAR Environmental Health Data for Rio de Janeiro"

  codemeta <- jsonlite::fromJSON(test_path("..", "..", "codemeta.json"))
  expect_identical(codemeta$name, title)
  expect_identical(codemeta$version, version)
  expect_identical(codemeta$codeRepository, repository)
  expect_identical(codemeta$issueTracker, paste0(repository, "/issues"))
  expect_identical(codemeta$license, "https://spdx.org/licenses/MIT")
  expect_identical(
    codemeta$author$`@id`,
    "https://orcid.org/0009-0005-6770-2001"
  )

  cff <- readLines(test_path("..", "..", "CITATION.cff"), warn = FALSE)
  expect_true(any(cff == paste0("title: \"", title, "\"")))
  expect_true(any(cff == paste0("version: \"", version, "\"")))
  expect_true(any(cff == paste0("repository-code: \"", repository, "\"")))

  readme <- readLines(test_path("..", "..", "README.Rmd"), warn = FALSE)
  expect_true(any(grepl(paste0("version ", version), readme, fixed = TRUE)))
  expect_true(any(grepl(repository, readme, fixed = TRUE)))
})

test_that("obsolete package identities and unsupported completeness claims stay removed", {
  files <- c("CITATION.cff", "codemeta.json", file.path("inst", "CITATION"))
  text <- paste(vapply(files, function(path) {
    paste(readLines(test_path("..", "..", path), warn = FALSE), collapse = "\n")
  }, character(1)), collapse = "\n")

  expect_false(grepl("vigiar\\.rj|vigiar-download|version 0\\.4\\.0", text))
  expect_false(grepl("download the complete dataset", text, ignore.case = TRUE))
})
