# vigiar: online integration tests
#
# These tests require internet access and a working VIGIAR dashboard.
# Run with:  VIGIAR_RUN_ONLINE_TESTS=true  R CMD check
# Or:        withr::local_envvar(VIGIAR_RUN_ONLINE_TESTS = "true")
#            devtools::test()

library(testthat)
library(vigiar)

# Guard: skip all online tests unless explicitly enabled
# This runs at file-level load time, before any test_that() block
online_tests <- identical(tolower(Sys.getenv("VIGIAR_RUN_ONLINE_TESTS")), "true")
if (!online_tests) {
  skip("Online tests disabled. Set VIGIAR_RUN_ONLINE_TESTS=true to run.")
}

# If we get here, online tests are enabled
