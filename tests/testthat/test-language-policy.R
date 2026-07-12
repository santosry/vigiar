test_that("technical English is the declared narrative language", {
  root <- .vigiar_test_source_root(c("DESCRIPTION", "LANGUAGE_POLICY.md"))
  skip_if(is.null(root), "Repository language policy is not installed.")

  description <- read.dcf(file.path(root, "DESCRIPTION"))[1, ]
  expect_identical(unname(description[["Language"]]), "en-US")
  expect_true(file.exists(file.path(root, "LANGUAGE_POLICY.md")))
})

test_that("known accentless Portuguese messages do not return", {
  root <- .vigiar_test_source_root("R")
  skip_if(is.null(root), "Repository source files are not installed.")

  source_files <- list.files(
    file.path(root, "R"),
    pattern = "\\.R$",
    full.names = TRUE
  )
  source <- paste(vapply(source_files, function(path) {
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }, character(1)), collapse = "\n")
  forbidden <- c(
    "Nenhuma sessao ativa",
    "Nao foi possivel",
    "Arquivo ja existe",
    "Dados exportados",
    "Tabela nao encontrada",
    "Valores ausentes",
    "Relatorio de Auditoria",
    "Relatorio Diagnostico",
    "Reproducibilidade garantida",
    "Variaveis nao documentadas"
  )
  for (phrase in forbidden) {
    expect_false(grepl(phrase, source, fixed = TRUE), info = phrase)
  }
})

test_that("narrative files are valid UTF-8 and contain no mojibake markers", {
  root <- .vigiar_test_source_root(c(
    "README.Rmd", "README.md", "NEWS.md", "CONTRIBUTING.md",
    "LANGUAGE_POLICY.md", "CITATION.cff", "vignettes"
  ))
  skip_if(is.null(root), "Repository narrative files are not installed.")

  files <- c(
    file.path(root, c(
      "README.Rmd", "README.md", "NEWS.md", "CONTRIBUTING.md",
      "LANGUAGE_POLICY.md", "CITATION.cff"
    )),
    list.files(file.path(root, "vignettes"), pattern = "\\.Rmd$", full.names = TRUE)
  )
  for (path in files) {
    text <- readLines(path, warn = FALSE, encoding = "UTF-8")
    expect_true(all(validUTF8(text)), info = basename(path))
    expect_false(any(grepl("Ã|Â|�", text)), info = basename(path))
  }
})
