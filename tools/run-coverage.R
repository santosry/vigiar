#!/usr/bin/env Rscript

threshold <- suppressWarnings(as.numeric(
  Sys.getenv("VIGIAR_COVERAGE_MIN", unset = "70")
))
if (length(threshold) != 1L || is.na(threshold) ||
    threshold < 0 || threshold > 100) {
  stop("VIGIAR_COVERAGE_MIN must be one percentage between 0 and 100.")
}

dir.create("coverage", recursive = TRUE, showWarnings = FALSE)
coverage <- covr::package_coverage(type = "tests", quiet = TRUE)
percent <- as.numeric(covr::percent_coverage(coverage))

covr::to_cobertura(coverage, filename = "coverage/coverage.xml")
saveRDS(coverage, "coverage/coverage.rds")
jsonlite::write_json(
  list(
    percent = percent,
    threshold = threshold,
    passed = percent >= threshold,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ),
  "coverage/summary.json",
  pretty = TRUE,
  auto_unbox = TRUE
)
writeLines(
  sprintf("Coverage: %.2f%% (required: %.2f%%)", percent, threshold),
  "coverage/summary.txt"
)
cat(readLines("coverage/summary.txt"), "\n")

if (percent < threshold) {
  stop(sprintf(
    "Coverage %.2f%% is below the required %.2f%% threshold.",
    percent, threshold
  ))
}
