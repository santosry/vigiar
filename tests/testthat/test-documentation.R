test_that("monthly documentation surfaces stay synchronized", {
  root <- .vigiar_test_source_root(c(
    "README.Rmd", "README.md", "_pkgdown.yml", "DESCRIPTION",
    file.path("vignettes", "monthly-pm25-rj.Rmd")
  ))
  skip_if(is.null(root), "Repository documentation is not installed.")

  files <- c(
    "README.Rmd", "README.md", "_pkgdown.yml", "DESCRIPTION",
    file.path("vignettes", "monthly-pm25-rj.Rmd"),
    file.path("man", "vigiar_baixar_pm25_mensal_rj.Rd"),
    file.path("man", "vigiar_analisar_pm25_mensal_rj.Rd")
  )
  expect_true(all(file.exists(file.path(root, files))))

  readme_rmd <- paste(
    readLines(file.path(root, "README.Rmd"), warn = FALSE),
    collapse = "\n"
  )
  readme_md <- paste(
    readLines(file.path(root, "README.md"), warn = FALSE),
    collapse = "\n"
  )
  required <- c(
    "Choose the Right Workflow",
    "vigiar_baixar_pm25_mensal_rj",
    "vigiar_analisar_pm25_mensal_rj",
    "vigiar_rj_completude",
    "vigiar_partition_report",
    "require_complete = TRUE",
    "vignette(\"monthly-pm25-rj\""
  )
  for (term in required) {
    expect_true(grepl(term, readme_rmd, fixed = TRUE), info = term)
    expect_true(grepl(term, readme_md, fixed = TRUE), info = term)
  }

  pkgdown <- paste(
    readLines(file.path(root, "_pkgdown.yml"), warn = FALSE),
    collapse = "\n"
  )
  expect_true(grepl("articles/monthly-pm25-rj.html", pkgdown, fixed = TRUE))

  description <- read.dcf(file.path(root, "DESCRIPTION"))[1, "Description"]
  expect_true(grepl("monthly PM2.5", description, fixed = TRUE))
  expect_true(grepl("municipality-year-month completeness", description,
                    fixed = TRUE))

  download_help <- paste(readLines(file.path(
    root, "man", "vigiar_baixar_pm25_mensal_rj.Rd"
  ), warn = FALSE), collapse = "\n")
  analysis_help <- paste(readLines(file.path(
    root, "man", "vigiar_analisar_pm25_mensal_rj.Rd"
  ), warn = FALSE), collapse = "\n")
  expect_true(grepl("Strict completeness", download_help, fixed = TRUE))
  expect_true(grepl("Return contract", download_help, fixed = TRUE))
  expect_true(grepl("Result components", analysis_help, fixed = TRUE))
  expect_true(grepl("Scientific scope", analysis_help, fixed = TRUE))
})

test_that("executable vignette chunks never call the live service", {
  root <- .vigiar_test_source_root("vignettes")
  skip_if(is.null(root), "Repository vignettes are not installed.")

  network_calls <- paste0(
    "\\b(",
    paste(c(
      "vigiar_conectar", "vigiar_baixar", "vigiar_baixar_rj",
      "vigiar_baixar_pm25_mensal_rj", "vigiar_baixar_rj_completo",
      "vigiar_baixar_tudo", "vigiar_baixar_principais",
      "vigiar_auditar_rj_online", "vigiar_health_check"
    ), collapse = "|"),
    ")\\s*\\("
  )
  files <- list.files(
    file.path(root, "vignettes"), pattern = "\\.Rmd$", full.names = TRUE
  )
  unsafe <- character()

  for (path in files) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    global_false <- any(grepl(
      "eval\\s*=\\s*FALSE", lines[seq_len(min(25L, length(lines)))]
    ))
    in_chunk <- FALSE
    header <- ""
    chunk <- character()

    for (line in lines) {
      if (!in_chunk && grepl("^```\\{r", line)) {
        in_chunk <- TRUE
        header <- line
        chunk <- character()
      } else if (in_chunk && grepl("^```\\s*$", line)) {
        has_network <- any(grepl(network_calls, chunk, perl = TRUE))
        explicitly_disabled <- grepl(
          "eval\\s*=\\s*FALSE", header, perl = TRUE
        )
        if (has_network && !global_false && !explicitly_disabled) {
          unsafe <- c(unsafe, paste0(basename(path), ": ", header))
        }
        in_chunk <- FALSE
      } else if (in_chunk) {
        chunk <- c(chunk, line)
      }
    }
  }

  expect_identical(unsafe, character(), info = paste(unsafe, collapse = "\n"))
})
