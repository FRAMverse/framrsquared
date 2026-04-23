# ── Helpers ----------------------------------------------------------

# Minimal mock fram_db list (valid structure, invalid connection)
make_mock_fram_db <- function(type = "full", species = "CHINOOK", read_only = FALSE) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  list(
    fram_db_connection = con,
    fram_db_connection_id = '10',
    fram_db_type       = type,
    fram_db_species    = species,
    fram_db_medium = "mdb",
    fram_read_only     = read_only
  )
}
disconnect_mock_fram_db <- function(db){
  DBI::dbDisconnect(db$fram_db_connection)
}

# UNIT TESTS -------------------------------------------------------------------

## --- validate_fram_abort() ----------------------------------------------------


test_that("fram_abort() always includes framrsquared_error class", {
  err <- rlang::catch_cnd(fram_abort("test error"))
  expect_true("framrsquared_error" %in% class(err))
})

test_that("fram_abort() includes custom class alongside framrsquared_error", {
  err <- rlang::catch_cnd(fram_abort("test", class = "framrsquared_invalid_run_id"))
  expect_true("framrsquared_invalid_run_id" %in% class(err))
  expect_true("framrsquared_error" %in% class(err))
})

test_that("fram_abort() correctly handles internal errors", {
  expect_error(fram_abort("test error", internal_error = TRUE),
               regexp = "Unexpected")
  err <- rlang::catch_cnd(fram_abort("test error"))
  expect_true(length(err$message) == 1)
})

test_that("fram_abort() correctly handles complex error messages internal errors", {
  include_in_error <- function(x){
    fram_abort("{x} is included from argument")
  }
  expect_error(include_in_error("coconut"), regexp = "coconut")
})

test_that("fram_abort() error call reflects the calling function", {
  bar <- function() fram_abort("test error")
  err <- rlang::catch_cnd(bar())
  expect_match(as.character(err$call)[1], "bar")
})

## --- validate_data_frame() --------------------------------------------------------

test_that("validate_data_frame() passes silently for a data.frame", {
  expect_no_error(validate_data_frame(data.frame(x = 1)))
})

test_that("validate_data_frame() passes silently for a tibble", {
  expect_no_error(validate_data_frame(tibble::tibble(x = 1)))
})

test_that("validate_data_frame() errors for a non-data-frame", {
  expect_error(validate_data_frame(list(x = 1)),   class = "framrsquared_error")
  expect_error(validate_data_frame(c(1, 2, 3)),    class = "framrsquared_error")
  expect_error(validate_data_frame("a string"),    class = "framrsquared_error")
  expect_error(validate_data_frame(NULL),          class = "framrsquared_error")
})

test_that("validate_data_frame() error message includes the argument name and calling function", {
  foo <- function(my_df) validate_data_frame(my_df)
  err <- rlang::catch_cnd(foo(42))
  expect_match(err$message, "my_df")
  expect_match(as.character(err$call)[1], "foo")
})


## --- validate_numeric() -----------------------------------------------------------

test_that("validate_numeric() passes for numeric vectors", {
  expect_no_error(validate_numeric(1L))
  expect_no_error(validate_numeric(c(1.5, 2.5)))
})

test_that("validate_numeric() errors for non-numeric input", {
  expect_error(validate_numeric("a"),        class = "framrsquared_error")
  expect_error(validate_numeric(TRUE),       class = "framrsquared_error")
  expect_error(validate_numeric(list(1)),    class = "framrsquared_error")
})

test_that("validate_numeric() respects n= length check", {
  expect_no_error(validate_numeric(c(1, 2), n = 2))
  expect_error(validate_numeric(c(1, 2),    n = 1), class = "framrsquared_error")
  expect_error(validate_numeric(1,          n = 3), class = "framrsquared_error")
})

test_that("validate_numeric() allow_null = TRUE treats NULL as valid", {
  expect_no_error(validate_numeric(NULL, allow_null = TRUE))
  # allow_null = FALSE (default) should still error on NULL
  expect_error(validate_numeric(NULL), class = "framrsquared_error")
})

test_that("validate_numeric() error message includes the argument name and calling function", {
  foo <- function(my_num) validate_numeric(my_num)
  err <- rlang::catch_cnd(foo("oops"))
  expect_match(err$message, "my_num")
  expect_match(as.character(err$call)[1], "foo")
})

## ---- validate_character() ---------------------------------------------

test_that("validate_character() passes for character vectors", {
  expect_no_error(validate_character("A"))
  expect_no_error(validate_character(letters))
})

test_that("validate_character() errors for non-character input", {
  expect_error(validate_character(-10),        class = "framrsquared_error")
  expect_error(validate_character(TRUE),       class = "framrsquared_error")
  expect_error(validate_character(list(1)),    class = "framrsquared_error")
})

test_that("validate_character() respects n= length check", {
  expect_no_error(validate_character(c("A", "b"), n = 2))
  expect_error(validate_character(c("A", "b"),    n = 1), class = "framrsquared_error")
  expect_error(validate_character(c("A"),          n = 3), class = "framrsquared_error")
})

test_that("validate_character() allow_null = TRUE treats NULL as valid", {
  expect_no_error(validate_character(NULL, allow_null = TRUE))
  # allow_null = FALSE (default) should still error on NULL
  expect_error(validate_character(NULL), class = "framrsquared_error")
})

test_that("validate_character() error message includes the argument name and calling function", {
  foo <- function(my_char) validate_character(my_char)
  err <- rlang::catch_cnd(foo(42))
  expect_match(err$message, "my_char")
  expect_match(as.character(err$call)[1], "foo")
})

test_that("validate_character() error message includes the argument name", {
  foo <- function(verbose) validate_character(verbose)
  err <- rlang::catch_cnd(foo(-10))
  expect_match(err$message, "verbose")
  expect_match(as.character(err$call)[1], "foo")
})

## validate_flag -------------------------------------------
test_that("validate_flag() passes silently for TRUE and FALSE", {
  expect_no_error(validate_flag(TRUE))
  expect_no_error(validate_flag(FALSE))
})

test_that("validate_flag() errors for non-logical input", {
  expect_error(validate_flag(1),          class = "framrsquared_error")
  expect_error(validate_flag("TRUE"),     class = "framrsquared_error")
  expect_error(validate_flag(1L),         class = "framrsquared_error")
  expect_error(validate_flag(NULL),       class = "framrsquared_error")
  expect_error(validate_flag(list(TRUE)), class = "framrsquared_error")
})

test_that("validate_flag() errors for logical vectors of length != 1", {
  expect_error(validate_flag(c(TRUE, FALSE)), class = "framrsquared_error")
  expect_error(validate_flag(logical(0)),     class = "framrsquared_error")
})

test_that("validate_flag() error message includes the argument name", {
  foo <- function(verbose) validate_flag(verbose)
  err <- rlang::catch_cnd(foo("yes"))
  expect_match(err$message, "verbose")
  expect_match(as.character(err$call)[1], "foo")
})

## --- standardize species ----------------------------------------------
test_that("standardize_species works", {
  expect_equal(standardize_species("chin"), "CHINOOK")
  expect_equal(standardize_species("coHo"), "COHO")
  expect_error(standardize_species("Cooho"), class = "rlang_error")
})


test_that("validate_species works",{
  dat1 = data.frame(fishery_id = 1:5,
                    let = letters[1:5])
  dat2 = data.frame(fishery_id = 1:5,
                    let = letters[1:5])
  attr(dat2, "species") <- "CHINOOK"
  expect_equal(validate_species(dat1, "COHO"), "COHO")
  expect_equal(validate_species(dat1, "chin"), "CHINOOK")
  expect_equal(validate_species(dat2), "CHINOOK")
  expect_error(validate_spacies(dat1))
  expect_error(validate_species(dat2, "COHO"))
})

## fram_clean_tables ----------------------------------------
test_that("fram_clean_tables works", {
  test_dat = mtcars[, 1:4]
  names(test_dat) = c("CamelCase", "dot.case", "ALLCAPS", "has spaces")

  cleaned_dat = fram_clean_tables(test_dat)
  expect_equal(names(cleaned_dat), c("camel_case", "dot_case", "allcaps", "has_spaces"))

  cleaned_dat_renamed = cleaned_dat
  names(cleaned_dat_renamed) = names(test_dat)
  expect_equal(tibble::as_tibble(test_dat), cleaned_dat_renamed)
})

## validate_fram_db() ---------------------------------------
test_that("validate_fram_db works", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))
  expect_no_error(validate_fram_db(fram_db))
  withr::deferred_run()

  fram_db <- make_mock_fram_db(species = "COHO", type = "full")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_no_error(validate_fram_db(fram_db, db_species = "COHO"))
  expect_error(validate_fram_db(fram_db, db_species = "CHINOOK"),
               class = "framrsquared_error")

  expect_no_error(validate_fram_db(fram_db, db_type = "full"))
  expect_error(validate_fram_db(fram_db, db_type = "transfer"),
               class = "framrsquared_error")

})

test_that("validate_fram_db() error call reflects the calling function", {
  foo <- function(db) validate_fram_db(db)
  err <- rlang::catch_cnd(foo(list()))
  expect_match(as.character(err$call)[1], "foo")
})

## validate_not_read_only() -------------------------------
test_that("validate_not_read_only works", {

  fram_db <- make_mock_fram_db(read_only = FALSE)
  withr::defer(disconnect_mock_fram_db(fram_db))
  expect_no_error(validate_not_read_only(fram_db))
  withr::deferred_run()

  fram_db <- make_mock_fram_db(read_only = TRUE)
  withr::defer(disconnect_mock_fram_db(fram_db))
  expect_error(validate_not_read_only(fram_db),
               class = "framrsquared_error")
})

test_that("validate_not_read_only() error call reflects the calling function", {
  fram_db <- make_mock_fram_db(read_only = TRUE)
  withr::defer(disconnect_mock_fram_db(fram_db))
  foo <- function(db) validate_not_read_only(db)
  err <- rlang::catch_cnd(foo(fram_db))
  expect_match(as.character(err$call)[1], "foo")
})

## validate_run_id() --------------------------------------------

test_that("validate_run_id handles argument checking", {

  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(
    get_run_ids = function(...){return(1:5)}
  )


  expect_error(validate_run_id(fram_db, "rat"),
               class = "framrsquared_error")
  expect_error(validate_run_id(fram_db, list(1)),
               class = "framrsquared_error")

  expect_error(validate_run_id(fram_db, 1:5, n = 2),
               class = "framrsquared_error")
  expect_no_error(validate_run_id(fram_db, 1:2, n = 2),
               class = "framrsquared_error")

  expect_error(validate_run_id(fram_db, NULL),
               class = "framrsquared_error")
  expect_no_error(validate_run_id(fram_db, NULL, allow_null = TRUE),
               class = "framrsquared_error")

})

test_that("validate_run_id checks runs correctly", {

  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(
    get_run_ids = function(...){return(1:5)}
  )

  expect_no_error(validate_run_id(fram_db, 1:4))
  expect_error(validate_run_id(fram_db, 6),
               class = "framrsquared_error")

  expect_error(validate_run_id(fram_db, NULL),
               class = "framrsquared_error")
  expect_no_error(validate_run_id(fram_db, NULL, allow_null = TRUE),
                  class = "framrsquared_error")

})

test_that("validate_run_id() error call reflects the calling function", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))
  local_mocked_bindings(get_run_ids = function(...) 1:5)
  foo <- function(db, id) validate_run_id(db, id)
  err <- rlang::catch_cnd(foo(fram_db, 99))
  expect_match(as.character(err$call)[1], "foo")
})

## validate_fishery_ids ------------------------

test_that("validate_fishery_ids handles argument checking", {

  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(
    get_fishery_ids = function(...){return(1:5)}
  )


  expect_error(validate_fishery_ids(fram_db, "rat"),
               class = "framrsquared_error")
  expect_error(validate_fishery_ids(fram_db, list(1)),
               class = "framrsquared_error")

  expect_error(validate_fishery_ids(fram_db, 1:5, n = 2),
               class = "framrsquared_error")
  expect_no_error(validate_fishery_ids(fram_db, 1:2, n = 2),
                  class = "framrsquared_error")

  expect_error(validate_fishery_ids(fram_db, NULL),
               class = "framrsquared_error")
  expect_no_error(validate_fishery_ids(fram_db, NULL, allow_null = TRUE),
                  class = "framrsquared_error")

})

test_that("validate_fishery_ids checks runs correctly", {

  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(
    get_fishery_ids = function(...){return(1:5)}
  )

  expect_no_error(validate_fishery_ids(fram_db, 1:4))
  expect_error(validate_fishery_ids(fram_db, 6),
               class = "framrsquared_error")

  expect_error(validate_fishery_ids(fram_db, NULL),
               class = "framrsquared_error")
  expect_no_error(validate_fishery_ids(fram_db, NULL, allow_null = TRUE),
                  class = "framrsquared_error")

})

test_that("validate_fishery_ids() error call reflects the calling function", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))
  local_mocked_bindings(get_fishery_ids = function(...) 1:5)
  foo <- function(db, id) validate_fishery_ids(db, id)
  err <- rlang::catch_cnd(foo(fram_db, 99))
  expect_match(as.character(err$call)[1], "foo")
})

## validate_stock_ids -------------------------------

test_that("validate_stock_ids handles argument checking", {

  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(
    get_stock_ids = function(...){return(1:5)}
  )


  expect_error(validate_stock_ids(fram_db, "rat"),
               class = "framrsquared_error")
  expect_error(validate_stock_ids(fram_db, list(1)),
               class = "framrsquared_error")

  expect_error(validate_stock_ids(fram_db, 1:5, n = 2),
               class = "framrsquared_error")
  expect_no_error(validate_stock_ids(fram_db, 1:2, n = 2),
                  class = "framrsquared_error")

  expect_error(validate_stock_ids(fram_db, NULL),
               class = "framrsquared_error")
  expect_no_error(validate_stock_ids(fram_db, NULL, allow_null = TRUE),
                  class = "framrsquared_error")

})

test_that("validate_stock_ids checks runs correctly", {

  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  local_mocked_bindings(
    get_stock_ids = function(...){return(1:5)}
  )

  expect_no_error(validate_stock_ids(fram_db, 1:4))
  expect_error(validate_stock_ids(fram_db, 6),
               class = "framrsquared_error")

  expect_error(validate_stock_ids(fram_db, NULL),
               class = "framrsquared_error")
  expect_no_error(validate_stock_ids(fram_db, NULL, allow_null = TRUE),
                  class = "framrsquared_error")

})

test_that("validate_stock_ids() error call reflects the calling function", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))
  local_mocked_bindings(get_stock_ids = function(...) 1:5)
  foo <- function(db, id) validate_stock_ids(db, id)
  err <- rlang::catch_cnd(foo(fram_db, 99))
  expect_match(as.character(err$call)[1], "foo")
})



# INTEGRATION TESTS ------------------------------------------

## get_run_ids -----------------------------------------------

test_that("get_run_ids works", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections())

  fram_db <- connection_coho_transfer(quiet = TRUE)
  expect_equal(sort(get_run_ids(fram_db)),
               sort(c(157, 158, 159)))
})

## get_fishery_ids --------------------------------------------

test_that("get_fishery_id works", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections())

  fram_db <- connection_chin_pre(quiet = TRUE)
  expect_equal(sort(unique(get_fishery_ids(fram_db))),
               1:73)

  fram_db <- connection_coho_pre(quiet = TRUE)
  expect_equal(sort(unique(get_fishery_ids(fram_db))),
               1:198)
})

## get_stock_ids --------------------------------------------

test_that("get_stock_id works", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections())

  fram_db <- connection_chin_pre(quiet = TRUE)
  expect_equal(sort(unique(get_stock_ids(fram_db))),
               1:78)

  fram_db <- connection_coho_pre(quiet = TRUE)
  expect_equal(sort(unique(get_stock_ids(fram_db))),
               1:246)
})

## find_tables_by_column_ --------------------------------------

test_that("find_tables_by_column_ works", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections())
  fram_db <- connection_chin_pre(quiet = TRUE)
  ## only one table should have "Kmature" in it: the Growth table
  expect_equal(find_tables_by_column_(fram_db, "KMature")$value,
               "Growth")
  expect_true(nrow(find_tables_by_column_(fram_db, "NO MATCH")) == 0)
})

## fram_database_type() ----------------------------------------

test_that("fram_database_type() identifies databases correctly", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections())

  fram_db <- connection_chin_post(quiet = TRUE)
  expect_equal(fram_database_type(fram_db$fram_db_connection), list(type = "full"))

  fram_db <- connection_chin_pre(quiet = TRUE)
  expect_equal(fram_database_type(fram_db$fram_db_connection), list(type = "full"))

  fram_db <- connection_coho_post(quiet = TRUE)
  expect_equal(fram_database_type(fram_db$fram_db_connection), list(type = "full"))

  fram_db <- connection_coho_pre(quiet = TRUE)
  expect_equal(fram_database_type(fram_db$fram_db_connection), list(type = "full"))

  fram_db <- connection_chin_transfer(quiet = TRUE)
  expect_equal(fram_database_type(fram_db$fram_db_connection), list(type = "transfer"))

  fram_db <- connection_coho_transfer(quiet = TRUE)
  expect_equal(fram_database_type(fram_db$fram_db_connection), list(type = "transfer"))

})

test_that("fram_database_type errors for strange database connections", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections())

  with_mocked_bindings(
    fram_db <- connection_test_db("partial files/test_validate_same_bp.mdb"),
    validate_fram_db = function(...){TRUE},
    fram_database_type = function(...){list(type = "partial")}
  )

  fram_db$fram_db_species = "CHINOOK"
  expect_error(fram_database_type(fram_db$fram_db_connection), class = "framrsquared_error")
})

## fram_db_species() --------------------------------------

test_that("fram_database_species correctly gets species", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections())

  fram_db <- connection_chin_post(quiet = TRUE)
  expect_equal(fram_database_species(fram_db$fram_db_connection), "CHINOOK")

  fram_db <- connection_chin_transfer(quiet = TRUE)
  expect_equal(fram_database_species(fram_db$fram_db_connection), "CHINOOK")

  fram_db <- connection_coho_pre(quiet = TRUE)
  expect_equal(fram_database_species(fram_db$fram_db_connection), "COHO")

  fram_db <- connection_coho_transfer(quiet = TRUE)
  expect_equal(fram_database_species(fram_db$fram_db_connection), "COHO")

})

test_that("fram_database_species handles multiple species correctly", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections())

  fram_db <- connection_test_db("partial files/test_multiple_species.mdb", quiet = TRUE)
  expect_equal(fram_database_species(fram_db$fram_db_connection), sort(c("COHO", "CHINOOK")))

  expect_no_warning(fram_database_species(fram_db$fram_db_connection))
  expect_warning(fram_database_species(fram_db$fram_db_connection,
                                       warn = TRUE))

})


## validate_same_bp() -------------------------------------

test_that("validate_same_bp basic error handling", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections())

  local_mocked_bindings(
    validate_fram_db = function(...){TRUE},
    fram_database_type = function(...){list(type = "partial")}
  )
  fram_db <- connection_test_db("partial files/test_validate_same_bp.mdb", quiet = TRUE)
  fram_db$fram_db_species = "CHINOOK"

  ## basic error / noerror tests:
  expect_error(validate_same_bp(fram_db,
                                run_ids = c(139, 144)),
               class = "framrsquared_error")
  expect_error(validate_same_bp(fram_db,
                                run_ids = c(139, 144, 147, 155)))

  expect_no_error(validate_same_bp(fram_db,
                                   run_ids = c(139, 140)))
  expect_no_error(validate_same_bp(fram_db,
                                   run_ids = 153:155))

  expect_error(validate_same_bp(fram_db,
                                run_ids = c(153:155, 139)))

  expect_no_error(validate_same_bp(fram_db,
                                   run_ids = c(153:155, 139),
                                   strict = FALSE)
  )
})

test_that("validate_same_bp output structure", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections())

  local_mocked_bindings(
    validate_fram_db = function(...){TRUE},
    fram_database_type = function(...){list(type = "partial")}
  )
  fram_db <- connection_test_db("partial files/test_validate_same_bp.mdb", quiet = TRUE)
  fram_db$fram_db_species = "CHINOOK"
  ## check output structure

  res <- validate_same_bp(fram_db,
                          run_ids = 153:155)
  expect_equal(names(res),
               c("same_bp", "same_fishery_version",
                 "same_stock_version",
                 "same_time_step_version",
                 "base_periods_df"))
  ## checking the outputs when strict = FALSE or on success
  expect_all_true(c(res$same_bp,
                    res$same_stock_version,
                    res$same_fishery_version,
                    res$same_time_step_version))

  stocks_differ <- validate_same_bp(fram_db,
                                    run_ids = c(139, 144),
                                    strict = FALSE)
  expect_all_true(c(!stocks_differ$same_bp,
                    !stocks_differ$same_stock_version,
                    stocks_differ$same_fishery_version,
                    stocks_differ$same_time_step_version))

  fisheries_differ <- validate_same_bp(fram_db,
                                       run_ids = c(139, 147),
                                       strict = FALSE)
  expect_all_true(c(!fisheries_differ$same_bp,
                    fisheries_differ$same_stock_version,
                    !fisheries_differ$same_fishery_version,
                    fisheries_differ$same_time_step_version))

  timesteps_differ <- validate_same_bp(fram_db,
                                       run_ids = c(139, 150),
                                       strict = FALSE)
  expect_all_true(c(!timesteps_differ$same_bp,
                    timesteps_differ$same_stock_version,
                    timesteps_differ$same_fishery_version,
                    !timesteps_differ$same_time_step_version))
})

test_that("validate_same_bp dataframe values", {
  skip_if_no_test_db()
  withr::defer(disconnect_all_fram_connections())

  local_mocked_bindings(
    validate_fram_db = function(...){TRUE},
    fram_database_type = function(...){list(type = "partial")}
  )
  fram_db <- connection_test_db("partial files/test_validate_same_bp.mdb", quiet = TRUE)
  fram_db$fram_db_species = "CHINOOK"
  ## Check dataframe of outputs
  each_different <- validate_same_bp(fram_db,
                                     run_ids = c(139, 144, 147, 150, 153),
                                     strict = FALSE)
  expect_equal(each_different$base_periods_df$base_period_id,
               4:8)
  expect_equal(each_different$base_periods_df$stock_version,
               c(5, 6, 5, 5, 6))
  expect_equal(each_different$base_periods_df$fishery_version,
               c(1, 1, 2, 1, 1))
  expect_equal(each_different$base_periods_df$time_step_version,
               c(1, 1, 1, 2, 2))
})
