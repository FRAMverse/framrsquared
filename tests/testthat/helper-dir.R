
## moved to `test_helpers.R` for more logical access

skip_if_slow_tests_disabled <- function() {
  ## option to skip the time consuming tests
  ## Run them by first running Sys.setenv(RUN_SLOW_TESTS = "true")
  skip_if_not(
    identical(Sys.getenv("RUN_SLOW_TESTS"), "true"),
    "Skipping slow tests. Set RUN_SLOW_TESTS=true to run (`Sys.setenv(RUN_SLOW_TESTS = 'true')`)."
  )
}

## expect framrsquared error, suppress messages
expect_error_m_f <- function(x){
  expect_error(

    suppressMessages(
      x
    ),
    class = "framrsquared_error"
  )
}

## expect framrsquared erro
expect_error_f <- function(x){
  expect_error(
      x,
    class = "framrsquared_error"
  )
}
