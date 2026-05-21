
## moved to `test_helpers.R` for more logical access

skip_if_slow_tests_disabled <- function() {
  ## option to skip the time consuming tests
  ## Run them by first running Sys.setenv(RUN_SLOW_TESTS = "true")
  skip_if_not(
    identical(Sys.getenv("RUN_SLOW_TESTS"), "true"),
    "Skipping slow tests. Set RUN_SLOW_TESTS=true to run (`Sys.setenv(RUN_SLOW_TESTS = 'true')`)."
  )
}
