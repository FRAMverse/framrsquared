## initialize_project ----------------------------------

test_that("initialize_project errors on bad inputs", {
  ## set up a folder to work in
  path <- tempdir()
  withr::defer(unlink(path, recursive = TRUE))

  local_mocked_bindings(
    here = function(...){paste0(path, "/", paste0(..., collapse = "/"))},
    .package = "here"
  )

  expect_error(initialize_project(folders = 10, quiet = TRUE),
               class = "framrsquared_error")
  expect_error(initialize_project(folders = list("hello", "there"), quiet = TRUE),
               class = "framrsquared_error")
  expect_error(initialize_project(folder = NULL, quiet = TRUE),
               class = "framrsquared_error")

  expect_error(initialize_project(quarto = 10, quiet = TRUE),
               class = "framrsquared_error")
  expect_error(initialize_project(quarto = c(TRUE, FALSE), quiet = TRUE),
               class = "framrsquared_error")

  expect_error(initialize_project(organization = 10, quiet = TRUE)) ## rlang::arg_match
  expect_error(initialize_project(organization = "novel", quiet = TRUE)) ## rlang::arg_match

  expect_error(initialize_project(renv = 10, quiet = TRUE),
               class = "framrsquared_error")
  expect_error(initialize_project(renv = c(TRUE, FALSE), quiet = TRUE),
               class = "framrsquared_error")

  expect_error(initialize_project(template_overwrite = 10, quiet = TRUE),
               class = "framrsquared_error")
  expect_error(initialize_project(template_overwrite = c(TRUE, FALSE), quiet = TRUE),
               class = "framrsquared_error")

  expect_error(initialize_project(color = 10, quiet = TRUE)) ## rlang::arg_match
  expect_error(suppressMessages(
    initialize_project(color = "blurgandy", quiet = TRUE),
    class = "cliMessage"
  )) ## rlang::arg_match

  expect_error(initialize_project(quiet = "blurge"),
               class = "framrsquared_error")
  expect_error(initialize_project(quiet = c(TRUE, FALSE)),
               class = "framrsquared_error")

})

test_that( "intialize_project creates desired folder structure", {
  ## set up a folder to work in
  path <- tempdir()
  withr::defer(unlink(path, recursive = TRUE))

  local_mocked_bindings(
    here = function(...){paste0(path, "/", paste0(..., collapse = "/"))},
    .package = "here"
  )

  suppressMessages(initialize_project(),
                   class = "cliMessage"
  )

  expect_true(all (sort(list.files(path)) == sort(c(
    'scripts',
    'original_data',
    'cleaned_data',
    'figures',
    'results',
    "style.css",
    "_quarto.yml"
  ))
  )
  )
  expect_true(file.exists(paste0(path, "/results/quarto_output")))
})

test_that( "intialize_project creates custom folder structure", {
  ## set up a folder to work in
  path <- tempdir()
  withr::defer(unlink(path, recursive = TRUE))

  local_mocked_bindings(
    here = function(...){paste0(path, "/", paste0(..., collapse = "/"))},
    .package = "here"
  )

  suppressMessages(
    initialize_project(folders = c("hello", "there", "general/kenobi"), quiet = TRUE),
    class = "cliMessage"
  )

  expect_true(all (sort(list.files(path)) == sort(c(
    'hello',
    'there',
    'general',
    "style.css",
    "_quarto.yml"
  ))
  )
  )
  expect_true(file.exists(paste0(path, "/general/kenobi")))
})

test_that( "intialize_project respects quarto = FALSE and renv = TRUE", {
  ## set up a folder to work in
  path <- tempdir()
  withr::defer(unlink(path, recursive = TRUE))

  local_mocked_bindings(
    here = function(...){paste0(path, "/", paste0(..., collapse = "/"))},
    .package = "here"
  )

  suppressMessages(
    initialize_project(quarto = FALSE, renv = TRUE, quiet = TRUE),
    class = "cliMessage"
  )

  expect_false(file.exists(here::here("style.css")))
  expect_false(file.exists(here::here("_quarto.yml")))

  expect_true(file.exists(here::here("renv.lock")))
  expect_true(file.exists(here::here("renv")))

})


test_that( "template colors and override work", {
  ## set up a folder to work in
  path <- tempdir()
  withr::defer(unlink(path, recursive = TRUE))

  local_mocked_bindings(
    here = function(...){paste0(path, "/", paste0(..., collapse = "/"))},
    .package = "here"
  )

  suppressMessages(
    initialize_project(quiet = TRUE),
    class = "cliMessage")


  count_of_green <- length(grep("#1D886E", readLines(here::here("_quarto.yml"))))
  count_of_coffee <- length(grep("#967259", readLines(here::here("_quarto.yml"))))
  expect_true(count_of_green == 0)
  expect_true(count_of_coffee == 1)

  suppressMessages(
    suppressWarnings(initialize_project(color = "green", quiet = TRUE)),
    class = "cliMessage"
  )

  count_of_green <- length(grep("#1D886E", readLines(here::here("_quarto.yml"))))
  count_of_coffee <- length(grep("#967259", readLines(here::here("_quarto.yml"))))
  expect_true(count_of_green == 1)
  expect_true(count_of_coffee == 0)

  suppressMessages(
    suppressWarnings(initialize_project(color = "coffee",
                                        template_overwrite = FALSE, quiet = TRUE)),
    class = "cliMessage"
  )


  count_of_green <- length(grep("#1D886E", readLines(here::here("_quarto.yml"))))
  count_of_coffee <- length(grep("#967259", readLines(here::here("_quarto.yml"))))
  expect_true(count_of_green == 1)
  expect_true(count_of_coffee == 0)

})

## fetch_quarto_templates -------------------------------------
## most testing happens through testing initialize_project

test_that( "fetch_quarto_templates handles invalid paths appropriately", {
  expect_error(fetch_quarto_templates("turtle"))

})
