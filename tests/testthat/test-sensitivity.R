## sensitivity.R tests

## Helpers ------------------------------------------------------------------

## A mock DB that supports sensitivity functions. It needs:
##   - RunID / StockRecruit for validate_run_id and table operations
##   - The five tables copy_run touches (StockFisheryRateScaler, NonRetention,
##     SizeLimits, SLRatio, FisheryScalers)
make_sensitivity_db <- function(return_list = FALSE) {
  stock_recruit <- data.frame(
    PrimaryKey         = 1:2,
    RunID              = c(1L, 1L),
    StockID            = c(1L, 2L),
    Age                = c(3L, 3L),
    RecruitScaleFactor = c(1.0, 4.0),
    RecruitCohortSize  = c(100.0, 400.0),
    Comment            = c("", "")
  )
  run_id_tbl <- data.frame(
    PrimaryKey  = 1L,
    RunID       = 1L,
    RunName     = "TestRun",
    RunComments = ""
  )
  minimal_run_table <- data.frame(PrimaryKey = 1L, RunID = 1L, vals = 1.0)

  table_list <- list(
    StockRecruit           = stock_recruit,
    RunID                  = run_id_tbl,
    StockFisheryRateScaler = minimal_run_table,
    NonRetention           = minimal_run_table,
    SizeLimits             = minimal_run_table,
    SLRatio                = minimal_run_table,
    FisheryScalers         = minimal_run_table
  )
  if (return_list) return(table_list)
  make_queryable_mock_db_list(table_list)
}

## Creates an empty .xlsx stub in a temp file (for TAMM argument requirements).
make_tamm_stub <- function() {
  tmp <- withr::local_tempfile(fileext = ".xlsx", .local_envir = parent.frame())
  writeBin(raw(0), tmp)
  tmp
}


## get_unique_filename() ----------------------------------------------------

test_that("get_unique_filename() returns first counter name when base doesn't exist", {
  tmp_dir  <- withr::local_tempdir()
  base     <- file.path(tmp_dir, "mylog.csv")

  result <- get_unique_filename(base)

  expect_equal(result, file.path(tmp_dir, "mylog_001.csv"))
})

test_that("get_unique_filename() skips counters whose files already exist", {
  tmp_dir <- withr::local_tempdir()
  base    <- file.path(tmp_dir, "mylog.csv")

  # Create _001 and _002 so the function must return _003
  writeLines("x", file.path(tmp_dir, "mylog_001.csv"))
  writeLines("x", file.path(tmp_dir, "mylog_002.csv"))

  result <- get_unique_filename(base)

  expect_equal(result, file.path(tmp_dir, "mylog_003.csv"))
})

test_that("get_unique_filename() works for non-csv extensions", {
  tmp_dir <- withr::local_tempdir()
  base    <- file.path(tmp_dir, "logfile.rds")

  result <- get_unique_filename(base)

  expect_equal(result, file.path(tmp_dir, "logfile_001.rds"))
})


## sensitivity_scaled() -----------------------------------------------------

### input validation ---------------------------------------------------------

test_that("sensitivity_scaled() errors for invalid fram_db", {
  expect_error(
    sensitivity_scaled(list(),
                       template_run  = 1L,
                       table_name    = "StockRecruit",
                       match_df      = data.frame(StockID = 1L),
                       scale_values  = c(0.5, 1.0),
                       cols_to_vary  = "RecruitScaleFactor"),
    class = "framrsquared_error"
  )
})

test_that("sensitivity_scaled() errors for non-existent template_run", {
  fram_db <- make_sensitivity_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    sensitivity_scaled(fram_db,
                       template_run  = 999L,
                       table_name    = "StockRecruit",
                       match_df      = data.frame(StockID = 1L),
                       scale_values  = c(0.5, 1.0),
                       cols_to_vary  = "RecruitScaleFactor"),
    class = "framrsquared_error"
  )
})

test_that("sensitivity_scaled() errors for invalid table_name", {
  fram_db <- make_sensitivity_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  # validate_table() delegates to rlang::arg_match(), which raises an
  # rlang_error rather than a framrsquared_error.
  expect_error(
    sensitivity_scaled(fram_db,
                       template_run  = 1L,
                       table_name    = "NotARealTable",
                       match_df      = data.frame(StockID = 1L),
                       scale_values  = c(0.5, 1.0),
                       cols_to_vary  = "RecruitScaleFactor")
  )
})

test_that("sensitivity_scaled() errors when match_df columns don't match table columns", {
  fram_db <- make_sensitivity_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    sensitivity_scaled(fram_db,
                       template_run  = 1L,
                       table_name    = "StockRecruit",
                       match_df      = data.frame(NonexistentCol = 1L),
                       scale_values  = c(0.5, 1.0),
                       cols_to_vary  = "RecruitScaleFactor"),
    class = "framrsquared_error"
  )
})

test_that("sensitivity_scaled() errors when match_df uses snake_case instead of CamelCase", {
  fram_db <- make_sensitivity_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  # framrsquared uses snake_case internally but FRAM DB uses CamelCase column names
  expect_error(
    sensitivity_scaled(fram_db,
                       template_run  = 1L,
                       table_name    = "StockRecruit",
                       match_df      = data.frame(stock_id = 1L),
                       scale_values  = c(0.5, 1.0),
                       cols_to_vary  = "RecruitScaleFactor"),
    class = "framrsquared_error"
  )
})

test_that("sensitivity_scaled() errors for non-numeric scale_values", {
  fram_db <- make_sensitivity_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    sensitivity_scaled(fram_db,
                       template_run  = 1L,
                       table_name    = "StockRecruit",
                       match_df      = data.frame(StockID = 1L),
                       scale_values  = c("half", "double"),
                       cols_to_vary  = "RecruitScaleFactor"),
    class = "framrsquared_error"
  )
})

test_that("sensitivity_scaled() errors for negative scale_values", {
  fram_db <- make_sensitivity_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    sensitivity_scaled(fram_db,
                       template_run  = 1L,
                       table_name    = "StockRecruit",
                       match_df      = data.frame(StockID = 1L),
                       scale_values  = c(-1, 0.5),
                       cols_to_vary  = "RecruitScaleFactor"),
    class = "framrsquared_error"
  )
})

test_that("sensitivity_scaled() errors when exactly one TAMM arg is provided", {
  fram_db    <- make_sensitivity_db()
  tamm_stub  <- make_tamm_stub()
  withr::defer(disconnect_mock_fram_db(fram_db))

  # tamm_template provided but tamm_target_folder not
  expect_error(
    sensitivity_scaled(fram_db,
                       template_run       = 1L,
                       table_name         = "StockRecruit",
                       match_df           = data.frame(StockID = 1L),
                       scale_values       = c(0.5, 1.0),
                       cols_to_vary       = "RecruitScaleFactor",
                       tamm_template      = tamm_stub,
                       tamm_target_folder = NULL,
                       save_log           = FALSE),
    class = "framrsquared_error"
  )
})

test_that("sensitivity_scaled() errors when label is not a single string", {
  fram_db   <- make_sensitivity_db()
  tamm_stub <- make_tamm_stub()
  withr::defer(disconnect_mock_fram_db(fram_db))

  # label/save_log are validated after the file.exists(tamm_template) check,
  # so a real TAMM stub is required to reach those validations.
  expect_error(
    sensitivity_scaled(fram_db,
                       template_run  = 1L,
                       table_name    = "StockRecruit",
                       match_df      = data.frame(StockID = 1L),
                       scale_values  = c(0.5, 1.0),
                       cols_to_vary  = "RecruitScaleFactor",
                       tamm_template = tamm_stub,
                       label         = 42L),
    class = "framrsquared_error"
  )

  expect_error(
    sensitivity_scaled(fram_db,
                       template_run  = 1L,
                       table_name    = "StockRecruit",
                       match_df      = data.frame(StockID = 1L),
                       scale_values  = c(0.5, 1.0),
                       cols_to_vary  = "RecruitScaleFactor",
                       tamm_template = tamm_stub,
                       label         = c("a", "b")),
    class = "framrsquared_error"
  )
})

test_that("sensitivity_scaled() errors when save_log is not a single logical", {
  fram_db   <- make_sensitivity_db()
  tamm_stub <- make_tamm_stub()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    sensitivity_scaled(fram_db,
                       template_run  = 1L,
                       table_name    = "StockRecruit",
                       match_df      = data.frame(StockID = 1L),
                       scale_values  = c(0.5, 1.0),
                       cols_to_vary  = "RecruitScaleFactor",
                       tamm_template = tamm_stub,
                       save_log      = "yes"),
    class = "framrsquared_error"
  )
})



### behavior -----------------------------------------------------------------

test_that("sensitivity_scaled() auto-prefixes match_df column names", {
  fram_db       <- make_sensitivity_db()
  tamm_stub     <- make_tamm_stub()
  tamm_dir      <- withr::local_tempdir()
  withr::defer(disconnect_mock_fram_db(fram_db))

  # Columns without "match_" prefix should be silently renamed.
  # If they were NOT renamed the validate step would reject them (wrong FRAM
  # column name after stripping), but since they are valid CamelCase names here
  # the rename allows the function to proceed.
  fetch_table(fram_db, "StockRecruit", label = FALSE) |>
    dplyr::pull("recruit_scale_factor")


  suppressMessages(
    result <- sensitivity_scaled(
      fram_db,
      template_run       = 1L,
      table_name         = "StockRecruit",
      match_df           = data.frame(StockID = 1L),   # no "match_" prefix
      scale_values       = c(2.0),
      cols_to_vary       = "RecruitScaleFactor",
      tamm_template      = tamm_stub,
      tamm_target_folder = tamm_dir,
      save_log           = FALSE
    )
  )

  expect_true("match_StockID" %in% names(result$full_df))
})

test_that("sensitivity_scaled() creates one run per scale value", {
  fram_db   <- make_sensitivity_db()
  tamm_stub <- make_tamm_stub()
  tamm_dir  <- withr::local_tempdir()
  withr::defer(disconnect_mock_fram_db(fram_db))

  n_scales <- 3L

  suppressMessages(
    sensitivity_scaled(
      fram_db,
      template_run       = 1L,
      table_name         = "StockRecruit",
      match_df           = data.frame(StockID = 1L),
      scale_values       = seq(0.5, 1.5, length.out = n_scales),
      cols_to_vary       = "RecruitScaleFactor",
      tamm_template      = tamm_stub,
      tamm_target_folder = tamm_dir,
      save_log           = FALSE
    )
  )

  # Original run (1) + 3 new sensitivity runs
  run_count <- nrow(fetch_table_(fram_db, "RunID"))
  expect_equal(run_count, 1L + n_scales)
})

test_that("sensitivity_scaled() returns list with $scales_by_runs and $full_df", {
  fram_db   <- make_sensitivity_db()
  tamm_stub <- make_tamm_stub()
  tamm_dir  <- withr::local_tempdir()
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages(
    result <- sensitivity_scaled(
      fram_db,
      template_run       = 1L,
      table_name         = "StockRecruit",
      match_df           = data.frame(StockID = 1L),
      scale_values       = c(0.5, 2.0),
      cols_to_vary       = "RecruitScaleFactor",
      tamm_template      = tamm_stub,
      tamm_target_folder = tamm_dir,
      save_log           = FALSE
    )
  )

  expect_true(is.list(result))
  expect_named(result, c("scales_by_runs", "full_df"), ignore.order = TRUE)
  expect_equal(nrow(result$scales_by_runs), 2L)
  expect_true("match_RunID" %in% names(result$scales_by_runs))
  expect_true("scale_RecruitScaleFactor" %in% names(result$scales_by_runs))
})

test_that("sensitivity_scaled() applies scale factors to DB values", {
  fram_db   <- make_sensitivity_db()
  tamm_stub <- make_tamm_stub()
  tamm_dir  <- withr::local_tempdir()
  withr::defer(disconnect_mock_fram_db(fram_db))

  # Original RecruitScaleFactor for StockID 1 is 1.0; scaling by 3 -> 3.0
  suppressMessages(
    result <- sensitivity_scaled(
      fram_db,
      template_run       = 1L,
      table_name         = "StockRecruit",
      match_df           = data.frame(StockID = 1L),
      scale_values       = 3.0,
      cols_to_vary       = "RecruitScaleFactor",
      tamm_template      = tamm_stub,
      tamm_target_folder = tamm_dir,
      save_log           = FALSE
    )
  )

  new_run_id <- result$scales_by_runs$match_RunID

  scaled_val <- fetch_table_(fram_db, "StockRecruit") |>
    dplyr::filter(run_id == new_run_id, stock_id == 1L) |>
    dplyr::pull(recruit_scale_factor)

  expect_equal(scaled_val, 3.0)
})


## sensitivity_exact() ------------------------------------------------------

### input validation ---------------------------------------------------------

test_that("sensitivity_exact() errors for invalid fram_db", {
  expect_error(
    sensitivity_exact(list(),
                      template_run = 1L,
                      table_name   = "StockRecruit",
                      match_df     = data.frame(StockID = 1L),
                      exact_values = c(0.5, 1.0),
                      cols_to_vary = "RecruitScaleFactor"),
    class = "framrsquared_error"
  )
})

test_that("sensitivity_exact() errors for non-existent template_run", {
  fram_db <- make_sensitivity_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    sensitivity_exact(fram_db,
                      template_run = 999L,
                      table_name   = "StockRecruit",
                      match_df     = data.frame(StockID = 1L),
                      exact_values = c(0.5, 1.0),
                      cols_to_vary = "RecruitScaleFactor"),
    class = "framrsquared_error"
  )
})

test_that("sensitivity_exact() errors when match_df columns don't match table columns", {
  fram_db <- make_sensitivity_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    sensitivity_exact(fram_db,
                      template_run = 1L,
                      table_name   = "StockRecruit",
                      match_df     = data.frame(NoSuchColumn = 1L),
                      exact_values = c(0.5, 1.0),
                      cols_to_vary = "RecruitScaleFactor"),
    class = "framrsquared_error"
  )
})

test_that("sensitivity_exact() errors for non-numeric exact_values", {
  fram_db <- make_sensitivity_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    sensitivity_exact(fram_db,
                      template_run = 1L,
                      table_name   = "StockRecruit",
                      match_df     = data.frame(StockID = 1L),
                      exact_values = c("low", "high"),
                      cols_to_vary = "RecruitScaleFactor"),
    class = "framrsquared_error"
  )
})

test_that("sensitivity_exact() errors when exactly one TAMM arg is provided", {
  fram_db   <- make_sensitivity_db()
  tamm_stub <- make_tamm_stub()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    sensitivity_exact(fram_db,
                      template_run       = 1L,
                      table_name         = "StockRecruit",
                      match_df           = data.frame(StockID = 1L),
                      exact_values       = c(5.0, 10.0),
                      cols_to_vary       = "RecruitScaleFactor",
                      tamm_template      = tamm_stub,
                      tamm_target_folder = NULL,
                      save_log           = FALSE),
    class = "framrsquared_error"
  )
})


### behavior -----------------------------------------------------------------

test_that("sensitivity_exact() creates one run per exact value", {
  fram_db   <- make_sensitivity_db()
  tamm_stub <- make_tamm_stub()
  tamm_dir  <- withr::local_tempdir()
  withr::defer(disconnect_mock_fram_db(fram_db))

  n_vals <- 4L

  suppressMessages(
    sensitivity_exact(
      fram_db,
      template_run       = 1L,
      table_name         = "StockRecruit",
      match_df           = data.frame(StockID = 1L),
      exact_values       = seq(1.0, 4.0, length.out = n_vals),
      cols_to_vary       = "RecruitScaleFactor",
      tamm_template      = tamm_stub,
      tamm_target_folder = tamm_dir,
      save_log           = FALSE
    )
  )

  run_count <- nrow(fetch_table_(fram_db, "RunID"))
  expect_equal(run_count, 1L + n_vals)
})

test_that("sensitivity_exact() writes exact values to DB (not scaled)", {
  fram_db   <- make_sensitivity_db()
  tamm_stub <- make_tamm_stub()
  tamm_dir  <- withr::local_tempdir()
  withr::defer(disconnect_mock_fram_db(fram_db))

  # Original RecruitScaleFactor for StockID 1 is 1.0; exact value should
  # overwrite it with 99.0 regardless of the original.
  suppressMessages(
    sensitivity_exact(
      fram_db,
      template_run       = 1L,
      table_name         = "StockRecruit",
      match_df           = data.frame(StockID = 1L),
      exact_values       = 99.0,
      cols_to_vary       = "RecruitScaleFactor",
      tamm_template      = tamm_stub,
      tamm_target_folder = tamm_dir,
      save_log           = FALSE
    )
  )

  # The only new run ID will be the largest RunID in the table after insertion.
  new_run_ids <- fetch_table_(fram_db, "RunID") |>
    dplyr::filter(run_id != 1L) |>
    dplyr::pull(run_id)

  result_val <- fetch_table_(fram_db, "StockRecruit") |>
    dplyr::filter(run_id %in% new_run_ids, stock_id == 1L) |>
    dplyr::pull(recruit_scale_factor)

  expect_equal(result_val, 99.0)
})


## sensitivity_custom() -----------------------------------------------------

### input validation ---------------------------------------------------------

test_that("sensitivity_custom() errors for invalid fram_db", {
  expect_error(
    sensitivity_custom(list(),
                       template_run  = 1L,
                       table_name    = "StockRecruit",
                       scenario_list = list(data.frame(match_StockID = 1L,
                                                       replace_RecruitScaleFactor = 2.0))),
    class = "framrsquared_error"
  )
})

test_that("sensitivity_custom() errors for non-existent template_run", {
  fram_db <- make_sensitivity_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    sensitivity_custom(fram_db,
                       template_run  = 999L,
                       table_name    = "StockRecruit",
                       scenario_list = list(data.frame(match_StockID = 1L,
                                                       replace_RecruitScaleFactor = 2.0))),
    class = "framrsquared_error"
  )
})

test_that("sensitivity_custom() errors when scenario_list contains non-data-frames", {
  fram_db <- make_sensitivity_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    sensitivity_custom(fram_db,
                       template_run  = 1L,
                       table_name    = "StockRecruit",
                       scenario_list = list("not a dataframe", 42L)),
    class = "framrsquared_error"
  )
})

test_that("sensitivity_custom() errors when exactly one TAMM arg is provided", {
  fram_db   <- make_sensitivity_db()
  tamm_stub <- make_tamm_stub()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    sensitivity_custom(fram_db,
                       template_run       = 1L,
                       table_name         = "StockRecruit",
                       scenario_list      = list(
                         data.frame(match_StockID = 1L,
                                    replace_RecruitScaleFactor = 5.0)
                       ),
                       tamm_template      = tamm_stub,
                       tamm_target_folder = NULL,
                       save_log           = FALSE),
    class = "framrsquared_error"
  )
})


### behavior -----------------------------------------------------------------

test_that("sensitivity_custom() creates one run per scenario", {
  fram_db   <- make_sensitivity_db()
  tamm_stub <- make_tamm_stub()
  tamm_dir  <- withr::local_tempdir()
  withr::defer(disconnect_mock_fram_db(fram_db))

  scenarios <- list(
    data.frame(match_StockID = 1L, replace_RecruitScaleFactor = 2.0),
    data.frame(match_StockID = 1L, replace_RecruitScaleFactor = 5.0),
    data.frame(match_StockID = 2L, replace_RecruitScaleFactor = 0.5)
  )

  suppressMessages(
    sensitivity_custom(
      fram_db,
      template_run       = 1L,
      table_name         = "StockRecruit",
      scenario_list      = scenarios,
      tamm_template      = tamm_stub,
      tamm_target_folder = tamm_dir,
      save_log           = FALSE
    )
  )

  run_count <- nrow(fetch_table_(fram_db, "RunID"))
  expect_equal(run_count, 1L + length(scenarios))
})

test_that("sensitivity_custom() applies each scenario's replace values to its run", {
  fram_db   <- make_sensitivity_db()
  tamm_stub <- make_tamm_stub()
  tamm_dir  <- withr::local_tempdir()
  withr::defer(disconnect_mock_fram_db(fram_db))

  suppressMessages(
    sensitivity_custom(
      fram_db,
      template_run       = 1L,
      table_name         = "StockRecruit",
      scenario_list      = list(
        data.frame(match_StockID = 1L, replace_RecruitScaleFactor = 7.0)
      ),
      tamm_template      = tamm_stub,
      tamm_target_folder = tamm_dir,
      save_log           = FALSE
    )
  )

  new_run_id <- fetch_table_(fram_db, "RunID") |>
    dplyr::filter(run_id != 1L) |>
    dplyr::pull(run_id)

  modified_val <- fetch_table_(fram_db, "StockRecruit") |>
    dplyr::filter(run_id == new_run_id, stock_id == 1L) |>
    dplyr::pull(recruit_scale_factor)

  expect_equal(modified_val, 7.0)
})
