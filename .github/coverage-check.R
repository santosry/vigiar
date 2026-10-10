# vigiar: enforce the test-coverage ratchet.
#
# Measures coverage, emits reports (Cobertura + HTML, best effort) and
# fails the build when the overall or per-file coverage drops below the
# floors recorded in .github/coverage-baseline.json.
#
# The baseline is a ratchet: values may only go up. To raise a floor,
# edit .github/coverage-baseline.json after a genuine coverage gain.

options(warn = 1)

tolerance <- 0.5  # percentage points, to absorb floating point jitter
baseline_path <- ".github/coverage-baseline.json"

if (!file.exists(baseline_path)) {
  stop("Baseline not found: ", baseline_path)
}
baseline <- jsonlite::fromJSON(baseline_path, simplifyVector = FALSE)

cov <- covr::package_coverage(type = "tests", quiet = FALSE)
pct <- covr::percent_coverage(cov)
cat(sprintf("Overall coverage: %.1f%%\n", pct))

# --- Reports (must never mask the gate) --------------------------------------
if (requireNamespace("xml2", quietly = TRUE)) {
  tryCatch(
    covr::to_cobertura(cov, filename = "cobertura.xml"),
    error = function(e) message("Could not write Cobertura report: ", conditionMessage(e))
  )
} else {
  message("Skipping Cobertura report: 'xml2' is not installed.")
}

if (requireNamespace("DT", quietly = TRUE) &&
      requireNamespace("htmltools", quietly = TRUE)) {
  tryCatch(
    covr::report(cov, file = "coverage.html"),
    error = function(e) message("Could not render HTML report: ", conditionMessage(e))
  )
} else {
  message("Skipping HTML report: 'DT' and/or 'htmltools' are not installed.")
}

# --- Overall floor ------------------------------------------------------------
overall_floor <- as.numeric(baseline$overall_pct)
if (length(overall_floor) == 1 && !is.na(overall_floor)) {
  cat(sprintf("Overall floor:    %.1f%%\n", overall_floor))
  if (pct + tolerance < overall_floor) {
    stop(sprintf(
      "Coverage regressed: overall %.1f%% is below the floor of %.1f%%.",
      pct, overall_floor
    ))
  }
}

# --- Per-file floors ----------------------------------------------------------
df <- covr::tally_coverage(cov, by = "line")
per_file <- tapply(df$value, df$filename, function(x) 100 * sum(x > 0) / length(x))

failed <- character(0)
checked <- 0L
for (nm in names(baseline$files)) {
  floor <- as.numeric(baseline$files[[nm]])
  current <- per_file[[nm]]
  if (is.null(current) || is.na(floor)) next
  checked <- checked + 1L
  status <- if (current + tolerance < floor) "FAIL" else "ok"
  cat(sprintf("  [%s] %-24s %5.1f%% (floor %.1f%%)\n", status, nm, current, floor))
  if (status == "FAIL") {
    failed <- c(failed, sprintf("%s: %.1f%% < %.1f%%", nm, current, floor))
  }
}

if (length(failed) > 0) {
  stop(
    "Per-file coverage below the baseline:\n  - ",
    paste(failed, collapse = "\n  - ")
  )
}

cat(sprintf("Coverage ratchet satisfied (%d files checked).\n", checked))
