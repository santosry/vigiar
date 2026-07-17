.vigiar_test_source_root <- function(required = "DESCRIPTION") {
  candidates <- unique(c(
    Sys.getenv("VIGIAR_SOURCE_ROOT", unset = ""),
    test_path("..", "..")
  ))

  for (candidate in candidates[nzchar(candidates)]) {
    root <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
    if (all(file.exists(file.path(root, required)))) {
      return(root)
    }
  }

  NULL
}
