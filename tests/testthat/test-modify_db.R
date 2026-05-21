## modify_table ------------------------------------------------------------

## helpers ------------------------------------------------------------------

## Builds a minimal in-memory DB with a StockRecruit-like table and a RunID
## table, so modify_table() can resolve column names and execute UPDATEs.
make_modify_db <- function(return_tables = FALSE) {
  stock_recruit <- data.frame(
    base_period_id = c(1, 1, 1),
    PrimaryKey       = 1:3,
    RunID            = c(1L, 1L, 1L),
    StockID          = c(10L, 20L, 30L),
    Age              = c(3L, 4L, 5L),
    RecruitScaleFactor = c(1.0, 2.0, 3.0),
    RecruitCohortSize  = c(100, 200, 300),
    Comment          = c("a", "b", "c")
  )
  run_id_tbl <- data.frame(
    PrimaryKey   = 1L,
    RunID        = 1L,
    RunName      = "TestRun",
    RunComments  = "",
    RunYear      = 2024L
  )
  table_list = list(StockRecruit = stock_recruit,
                    RunID = run_id_tbl)
  if(return_tables){

    return(table_list)

  } else {

    return(make_queryable_mock_db_list(table_list))

  }
}

## Modify_table() ------------------------------------------------------------

### input validation ---------------------------------------------------------

test_that("modify_table() errors for invalid fram_db", {
  expect_error(modify_table(list(),
                            table_name = "StockRecruit",
                            df = data.frame()),
               class = "framrsquared_error")
})

test_that("modify_table() errors on read-only db", {
  fram_db <- make_mock_fram_db(read_only = TRUE)
  withr::defer(disconnect_mock_fram_db(fram_db))

  df <- data.frame(match_RunID = 1L, replace_RecruitScaleFactor = 5.0)
  expect_error(modify_table(fram_db,
                            table_name = "StockRecruit",
                            df = df),
               class = "framrsquared_error")
})

test_that("modify_table() errors when df columns lack match_/replace_ prefix", {
  fram_db <- make_modify_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  df_bad <- data.frame(RunID = 1L, RecruitScaleFactor = 5.0)
  expect_error(modify_table(fram_db,
                            table_name = "StockRecruit",
                            df = df_bad),
               class = "framrsquared_error")

  df_bad <- data.frame(match_RunID = 1L, RecruitScaleFactor = 5.0)
  expect_error(modify_table(fram_db,
                            table_name = "StockRecruit",
                            df = df_bad),
               class = "framrsquared_error")

})

test_that("modify_table() errors when df has no match_ columns", {
  fram_db <- make_modify_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  df_bad <- data.frame(replace_RecruitScaleFactor = 5.0)
  expect_error(modify_table(fram_db,
                            table_name = "StockRecruit",
                            df = df_bad),
               class = "framrsquared_error")
})

test_that("modify_table() errors when df has no replace_ columns", {
  fram_db <- make_modify_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  df_bad <- data.frame(match_RunID = 1L)
  expect_error(modify_table(fram_db,
                            table_name = "StockRecruit",
                            df = df_bad),
               class = "framrsquared_error")
})

test_that("modify_table() errors when match_ and replace_ share a column name", {
  fram_db <- make_modify_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  df_bad <- data.frame(
    match_RunID            = 1L,
    match_RecruitScaleFactor  = 1.0,
    replace_RecruitScaleFactor = 5.0,
    replace_RecruitCohortSize  = 6.0
  )
  expect_error(modify_table(fram_db,
                            table_name = "StockRecruit",
                            df = df_bad),
               class = "framrsquared_error")
})

test_that("modify_table() errors when df references columns not in table", {
  fram_db <- make_modify_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  df_bad <- data.frame(match_RunID = 1L,
                       replace_NonExistentColumn = 99L)
  expect_error(modify_table(fram_db,
                            table_name = "StockRecruit",
                            df = df_bad),
               class = "framrsquared_error")
})

### behaviour ----------------------------------------------------------------

test_that("modify_table() updates matched rows and returns rows_affected", {
  fram_db <- make_modify_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  df <- data.frame(
    match_RunID   = 1L,
    match_StockID = 10L,
    replace_RecruitScaleFactor = 99.0
  )

  result <- modify_table(fram_db, "StockRecruit", df)

  expect_true("rows_affected" %in% names(result))
  expect_equal(result$rows_affected, 1L)

  updated <- fetch_table_(fram_db, "StockRecruit") |>
    dplyr::filter(stock_id == 10) |>
    dplyr::pull(recruit_scale_factor)
  expect_equal(updated, 99.0)
})

test_that("modify_table() updates multiple rows when multiple match rows given", {
  fram_db <- make_modify_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  df <- data.frame(
    match_RunID   = c(1L, 1L),
    match_StockID = c(10L, 20L),
    replace_RecruitCohortSize = c(500, 600)
  )
  result <- modify_table(fram_db, "StockRecruit", df)
  expect_equal(sum(result$rows_affected), 2L)

  updated <- fetch_table_(fram_db,
                          "StockRecruit") |>
    dplyr::filter(stock_id %in% c(10, 20))

  expect_equal(sort(updated$recruit_cohort_size), c(500, 600))
})


## calc_fram_scaling --------------------------------------------------------

### helpers ------------------------------------------------------------------

make_scaling_db <- function(return_list = FALSE) {
  stock_recruit <- data.frame(
    PrimaryKey         = 1:2,
    RunID              = c(31L, 31L),
    StockID            = c(1L, 2L),
    Age                = c(3L, 3L),
    TimeStep           = c(1L, 1L),
    RecruitScaleFactor = c(1.0, 4.0),
    RecruitCohortSize  = c(100.0, 400.0)
  )
  if(return_list){
    return(list(StockRecruit = stock_recruit))
  } else {
    return(make_queryable_mock_db_list(list(StockRecruit = stock_recruit)))
  }
}

### input validation --------------------------------------------

test_that("calc_fram_scaling() errors for invalid fram_db", {
  expect_error(calc_fram_scaling(list(),
                                 table_name = "StockRecruit",
                                 df = data.frame()),
               class = "framrsquared_error")
})


test_that("calc_fram_scaling() errors when df columns lack match_/scale_ prefix", {
  fram_db <- make_modify_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  df_bad <- data.frame(RunID = 1L, RecruitScaleFactor = 5.0)
  expect_error(calc_fram_scaling(fram_db,
                                 table_name = "StockRecruit",
                                 df = df_bad),
               class = "framrsquared_error")

  df_bad <- data.frame(RunID = 1L, scale_RecruitScaleFactor = 5.0)
  expect_error(calc_fram_scaling(fram_db,
                                 table_name = "StockRecruit",
                                 df = df_bad),
               class = "framrsquared_error")

  df_bad <- data.frame(match_RunID = 1L, RecruitScaleFactor = 5.0)
  expect_error(calc_fram_scaling(fram_db,
                                 table_name = "StockRecruit",
                                 df = df_bad),
               class = "framrsquared_error")
})

test_that("calc_fram_scaling() errors when match_ and scale_ share a column name", {
  fram_db <- make_modify_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  df_bad <- data.frame(
    match_RunID            = 1L,
    match_RecruitScaleFactor  = 1.0,
    scale_RecruitScaleFactor = 5.0,
    scale_RecruitCohortSize  = 6.0
  )
  expect_error(calc_fram_scaling(fram_db,
                                 table_name = "StockRecruit",
                                 df = df_bad),
               class = "framrsquared_error")
})

test_that("calc_fram_scaling() errors when df references columns not in table", {
  fram_db <- make_modify_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  df_bad <- data.frame(match_RunID = 1L,
                       scale_NonExistentColumn = 99L)
  expect_error(calc_fram_scaling(fram_db,
                                 table_name = "StockRecruit",
                                 df = df_bad),
               class = "framrsquared_error")
})

test_that("calc_fram_scaling() errors when RecruitScaleFactor and RecruitCohortSize are provided with different scalings", {
  fram_db <- make_modify_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  df_bad <- data.frame(match_RunID = 1L,
                       scale_RecruitScaleFactor = 0.5,
                       scale_RecruitCohortSize = 1.1)
  expect_error(calc_fram_scaling(fram_db,
                                 table_name = "StockRecruit",
                                 df = df_bad),
               class = "framrsquared_error")

  df_good <- data.frame(match_RunID = 1L,
                        scale_RecruitScaleFactor = 0.5,
                        scale_RecruitCohortSize = 0.5)
  expect_no_error(calc_fram_scaling(fram_db,
                                    table_name = "StockRecruit",
                                    df = df_good),
                  class = "framrsquared_error")

})


### behaviour ----------------------------------------------------------------

test_that("calc_fram_scaling() returns replace_ columns with scaled values", {
  fram_db <- make_scaling_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  df <- data.frame(
    match_RunID              = c(31L, 31L),
    match_StockID            = c(1L, 2L),
    scale_RecruitScaleFactor = c(2.0, 0.5)
  )
  expect_message(
    result <- calc_fram_scaling(fram_db, "StockRecruit", df),
    "Adding scaling for RecruitCohortSize equal to RecruitScaleFactor."
  )

  expect_true(all(c("replace_RecruitScaleFactor", "replace_RecruitCohortSize") %in%
                    names(result)))

  rsf_vals <- result$replace_RecruitScaleFactor
  expect_equal(rsf_vals[result$match_StockID == 1L], 2.0)
  expect_equal(rsf_vals[result$match_StockID == 2L], 2.0)
})

test_that("calc_fram_scaling() auto-adds paired RecruitCohortSize scaler", {
  fram_db <- make_scaling_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  df <- data.frame(
    match_RunID              = 31L,
    match_StockID            = 1L,
    scale_RecruitScaleFactor = 3.0
  )

  # Should warn about adding the paired column
  expect_message(
    result <- calc_fram_scaling(fram_db, "StockRecruit", df),
    regexp = "Adding scaling"   # no error; warning is acceptable — remove this line to check
  )
  expect_true("replace_RecruitCohortSize" %in% names(result))
  expect_equal(result$replace_RecruitCohortSize, 300.0)
})

## change_run_id ------------------------------------------------------------

make_change_run_db <- function(return_list = FALSE) {
  run_id_tbl <- data.frame(
    PrimaryKey  = 1L,
    RunID       = 1L,
    RunName     = "TestRun",
    RunComments = ""
  )
  stock_recruit <- data.frame(
    PrimaryKey         = 1L,
    RunID              = 1L,
    StockID            = 10L,
    RecruitScaleFactor = 1.0
  )
  table_list = list(RunID = run_id_tbl, StockRecruit = stock_recruit)

  if(return_list){
    return(table_list)
  } else {
    return(make_queryable_mock_db_list(table_list))
  }
}

test_that("change_run_id() errors on invalid fram_db", {
  expect_error(change_run_id(list(), 1L, 99L), class = "framrsquared_error")
})

test_that("change_run_id() errors on read-only db", {
  fram_db <- make_mock_fram_db(read_only = TRUE)
  withr::defer(disconnect_mock_fram_db(fram_db))
  expect_error(change_run_id(fram_db, 1L, 99L), class = "framrsquared_error")
})

test_that("change_run_id() errors when new_run_id is already being used", {
  fram_db <- make_change_run_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  # run_id 1 is both the old and the new — should abort
  expect_error(change_run_id(fram_db, 1L, 1L), class = "framrsquared_error")
})

test_that("change_run_id() updates RunID across tables", {
  fram_db <- make_change_run_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  change_run_id(fram_db, old_run_id = 1L, new_run_id = 99L)

  run_ids_after <- fetch_table_(fram_db, "RunID")$run_id
  expect_equal(run_ids_after, 99L)

  sr_run_ids <- fetch_table_(fram_db, "StockRecruit")$run_id
  expect_equal(sr_run_ids, 99L)
})


## remove_run ---------------------------------------------------------------

make_remove_run_db <- function(return_list = FALSE) {
  run_id_tbl <- data.frame(
    PrimaryKey  = 1:2,
    RunID       = c(1L, 2L),
    RunName     = c("Run1", "Run2"),
    RunComments = c("", "")
  )
  stock_recruit <- data.frame(
    PrimaryKey         = 1:2,
    RunID              = c(1L, 2L),
    StockID            = c(10L, 10L),
    RecruitScaleFactor = c(1.0, 2.0)
  )
  table_list = list(RunID = run_id_tbl, StockRecruit = stock_recruit)
  if(return_list){
    return(table_list)
  } else{
    return(make_queryable_mock_db_list(table_list))
  }
}

test_that("remove_run() errors on invalid fram_db", {
  expect_error(remove_run(list(), 1L), class = "framrsquared_error")
})

test_that("remove_run() errors on read-only db", {
  fram_db <- make_mock_fram_db(read_only = TRUE)
  withr::defer(disconnect_mock_fram_db(fram_db))
  expect_error(remove_run(fram_db, 1L), class = "framrsquared_error")
})

test_that("remove_run() deletes matching rows across tables", {
  fram_db <- make_remove_run_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  remove_run(fram_db, run_id = 1L)

  remaining_run_ids <- fetch_table_(fram_db, "RunID") |>
    dplyr::pull(run_id)
  expect_equal(remaining_run_ids, 2L)

  remaining_sr <- fetch_table_(fram_db, "StockRecruit")$run_id
  expect_equal(remaining_sr, 2L)
})

test_that("remove_run() can remove multiple run IDs at once", {
  fram_db <- make_remove_run_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  remove_run(fram_db, run_id = c(1L, 2L))

  suppressWarnings({
    remaining <- fetch_table_(fram_db, "RunID")
  })
  expect_equal(nrow(remaining), 0L)
})

## copy_fishery_scalers ----------------------------------------------------


make_scalers_db <- function(return_list = FALSE) {

  fishery_scalers <- data.frame(
    PrimaryKey         = 1:4,
    RunID              = c(1, 1, 2, 2),
    FisheryID            = c(1, 2, 1, 2),
    TimeStep           = rep(1, 4),
    FisheryFlag = c(1, 2, 7, 8),
    FisheryScaleFactor = 1:4,
    Quota = (1:4)*10,
    MSFFisheryScaleFactor = 5:8,
    MSFQuota = (5:8)*10,
    MarkReleaseRate = (1:4)/10,
    MarkMisIDRate = (1:4)/100,
    UnMarkMisIDRate = (5:8)/10,
    MarkIncidentalRate = (5:8)/100
  )

  fishery = data.frame(fishery_id = 1:2)

  run_id <- data.frame(RunID = 1:2, RunComments = c("A", "B"))

  tables_list = list(FisheryScalers = fishery_scalers,
                     Fishery = fishery,
                     RunID = run_id)

  if(return_list){
    return(list(tables_list))
  } else {
    return(make_queryable_mock_db_list(tables_list))
  }
}




### input validation ---------------------------------------------

test_that("copy_fishery_scalers() errors for invalid fram_db", {
  expect_error(copy_fishery_scalers(list(),
                                    from_run = 1,
                                    to_run = 2,
                                    fishery_id = 1),
               class = "framrsquared_error")
})

test_that("copy_fishery_scalers() errors for invalid run_ids, fishery_id", {
  fram_db <- make_scalers_db()
  withr::defer(disconnect_mock_fram_db(fram_db))
  expect_error(copy_fishery_scalers(fram_db,
                                    from_run = list(1),
                                    to_run = 2,
                                    fishery_id = 1),
               class = "framrsquared_error")

  expect_error(copy_fishery_scalers(fram_db,
                                    from_run = 1,
                                    to_run = "hello",
                                    fishery_id = 1),
               class = "framrsquared_error")

  expect_error(copy_fishery_scalers(fram_db,
                                    from_run = 3,
                                    to_run = 2,
                                    fishery_id = 1),
               class = "framrsquared_error")

  expect_error(copy_fishery_scalers(fram_db,
                                    from_run = 1,
                                    to_run = 3,
                                    fishery_id = 1),
               class = "framrsquared_error")

  expect_error(copy_fishery_scalers(fram_db,
                                    from_run = 1,
                                    to_run = 2,
                                    fishery_id = 10),
               class = "framrsquared_error")

})


### behavior ------------------------------------------------------

## correctly copies scalers
test_that("copy_fishery_scalers() correctly transfers fishery scalers", {
  fram_db <- make_scalers_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  original_scalers <- fetch_table_(fram_db, "FisheryScalers")

  suppressMessages(
    record <- copy_fishery_scalers(fram_db,
                                   from_run = 1,
                                   to_run = 2,
                                   fishery_id = 1)
  )
  new_scalers <- fetch_table_(fram_db, "FisheryScalers")

  ## non-selected fisheries respected, original run respected
  unchanged_rows_orig <- original_scalers |> dplyr::filter(!(run_id == 2 & fishery_id == 1))
  unchanged_rows_new <- new_scalers |> dplyr::filter(!(run_id == 2 & fishery_id == 1))
  expect_equal(unchanged_rows_orig, unchanged_rows_new)

  ## changed fishery is changed
  copy_from_row <- original_scalers |> dplyr::filter(run_id == 1, fishery_id == 1) |> dplyr::select(-primary_key, -run_id)
  copy_to_row <- new_scalers |> dplyr::filter(run_id == 2, fishery_id == 1)|> dplyr::select(-primary_key, -run_id)
  expect_equal(copy_from_row, copy_to_row)

  ## primary key and run_id are unchanged
  expect_equal(original_scalers$primary_key, new_scalers$primary_key)
  expect_equal(original_scalers$run_id, new_scalers$run_id)

  ## run_comments updated appropriately
  run_comments <- fetch_table(fram_db, "RunID")$run_comments
  expect_true(grepl("B\n\n FISHERY SCALERS COPIED PROGRAMMATICALLY FROM RUN 1", run_comments[2]))

  ## invisibly output correct information
  expected_row <- original_scalers |>
    dplyr::filter(run_id == 1, fishery_id == 1) |>
    dplyr::mutate(rows_affected = 1) |>
    dplyr::rowwise()
  expect_equal(expected_row, record)
})


test_that("copy_fishery_scalers() correctly responds to user input when no fishery_id is provided", {
  fram_db <- make_scalers_db()
  withr::defer(disconnect_mock_fram_db(fram_db))


  original_scalers <- fetch_table_(fram_db, "FisheryScalers")

  ## first, what if user types 'n'? should abort
  local_mocked_bindings(
    readline = function(prompt = "") "n",
    .package = "base"
  )

  suppressMessages({
    expect_error( copy_fishery_scalers(fram_db,
                                       from_run = 1,
                                       to_run = 2),
                  class = "framrsquared_error")
  })

  ## second, what if user types 'y'? Should go through.
  local_mocked_bindings(
    readline = function(prompt = "") "y",
    .package = "base"
  )

  suppressMessages(
    expect_message(record <- copy_fishery_scalers(fram_db,
                                                  from_run = 1,
                                                  to_run = 2),
                   c("updated 2")
    )
  )
  new_scalers <- fetch_table_(fram_db, "FisheryScalers")
  unchanged_rows_orig <- original_scalers |> dplyr::filter(run_id == 1)
  unchanged_rows_new <- new_scalers |> dplyr::filter(run_id == 1)
  expect_equal(unchanged_rows_orig, unchanged_rows_new)

  ## changed fishery is changed
  copy_from_rows <- original_scalers |> dplyr::filter(run_id == 1) |> dplyr::select(-primary_key, -run_id)
  copy_to_rows <- new_scalers |> dplyr::filter(run_id == 2)|> dplyr::select(-primary_key, -run_id)
  expect_equal(copy_from_rows, copy_to_rows)

  ## primary key and run_id are unchanged
  expect_equal(original_scalers$primary_key, new_scalers$primary_key)
  expect_equal(original_scalers$run_id, new_scalers$run_id)
})

## correctly warns on missing fisheries

test_that("copy_fishery_scalers warns if 'from' run is missing matches", {
  fishery_scalers <- data.frame(
    PrimaryKey         = 1:4,
    RunID              = c(1, 1, 2, 2),
    FisheryID          = c(1, 2, 1, 2),
    TimeStep           = c(1, 1, 1, 2), ## no timestep match for fishery 2
    FisheryFlag = c(1, 2, 7, 8),
    FisheryScaleFactor = 1:4,
    Quota = (1:4)*10,
    MSFFisheryScaleFactor = 5:8,
    MSFQuota = (5:8)*10,
    MarkReleaseRate = (1:4)/10,
    MarkMisIDRate = (1:4)/100,
    UnMarkMisIDRate = (5:8)/10,
    MarkIncidentalRate = (5:8)/100
  )

  fishery = data.frame(fishery_id = 1:2)

  run_id <- data.frame(RunID = 1:2, RunComments = c("A", "B"))

  tables_list = list(FisheryScalers = fishery_scalers,
                     Fishery = fishery,
                     RunID = run_id)
  fram_db <- make_queryable_mock_db_list(tables_list)

  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_message(
    record <- copy_fishery_scalers(fram_db,
                                   from_run = 1,
                                   to_run = 2,
                                   fishery_id = 2),
    "rows were not changed"
  )

  expect_equal(record$rows_affected, 0)

})

test_that("copy_fishery_scalers errors if problem with 'from' row identifiability", {
  fishery_scalers <- data.frame(
    PrimaryKey         = 1:4,
    RunID              = c(1, 1, 2, 2),
    FisheryID          = c(1, 1, 1, 2), ##two entries for run_id 1, fishery_id 1, ts 1
    TimeStep           = rep(1, 4),
    FisheryFlag = c(1, 2, 7, 8),
    FisheryScaleFactor = 1:4,
    Quota = (1:4)*10,
    MSFFisheryScaleFactor = 5:8,
    MSFQuota = (5:8)*10,
    MarkReleaseRate = (1:4)/10,
    MarkMisIDRate = (1:4)/100,
    UnMarkMisIDRate = (5:8)/10,
    MarkIncidentalRate = (5:8)/100
  )

  fishery = data.frame(fishery_id = 1:2)

  run_id <- data.frame(RunID = 1:2, RunComments = c("A", "B"))

  tables_list = list(FisheryScalers = fishery_scalers,
                     Fishery = fishery,
                     RunID = run_id)
  fram_db <- make_queryable_mock_db_list(tables_list)

  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(
    record <- copy_fishery_scalers(fram_db,
                                   from_run = 1,
                                   to_run = 2,
                                   fishery_id = 1),
    class = "framrsquared_error")
})

test_that("copy_fishery_scalers warns if multiple matches for 'to' run rows", {
  fishery_scalers <- data.frame(
    PrimaryKey         = 1:4,
    RunID              = c(1, 1, 2, 2),
    FisheryID          = c(1, 2, 1, 1), ##two entries for run_id 1, fishery_id 1, ts 1
    TimeStep           = rep(1, 4),
    FisheryFlag = c(1, 2, 7, 8),
    FisheryScaleFactor = 1:4,
    Quota = (1:4)*10,
    MSFFisheryScaleFactor = 5:8,
    MSFQuota = (5:8)*10,
    MarkReleaseRate = (1:4)/10,
    MarkMisIDRate = (1:4)/100,
    UnMarkMisIDRate = (5:8)/10,
    MarkIncidentalRate = (5:8)/100
  )

  fishery = data.frame(fishery_id = 1:2)

  run_id <- data.frame(RunID = 1:2, RunComments = c("A", "B"))

  tables_list = list(FisheryScalers = fishery_scalers,
                     Fishery = fishery,
                     RunID = run_id)
  fram_db <- make_queryable_mock_db_list(tables_list)

  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_message(
    record <- copy_fishery_scalers(fram_db,
                                   from_run = 1,
                                   to_run = 2,
                                   fishery_id = 1),
    "DON'T USE")

  expect_equal(record$rows_affected, 2)
})

## copy_run ----------------------------------------------------------------

make_copyable_db <- function(return_tables = FALSE) {
  stock_recruit <- data.frame(
    base_period_id = c(1, 1),
    PrimaryKey       = 1:2,
    RunID            = 1:2,
    StockID          = c(10L, 20L),
    Age              = c(3L, 4L),
    RecruitScaleFactor = c(1.0, 2.0),
    RecruitCohortSize  = c(100, 200),
    Comment          = c("a", "b")
  )
  run_id = data.frame(PrimaryKey = 1:2,
                      RunID = 1:2,
                      RunName = c("first", "second"))
  stock_fishery_rate_scaler =
    non_retention =
    size_limits =
    sl_ratio =
    fishery_scalers = data.frame(RunID = 1:2,
                                 vals = 1:2,
                                 PrimaryKey = 1:2)


  table_list = list(StockRecruit = stock_recruit,
                    RunID = run_id,
                    StockFisheryRateScaler = stock_fishery_rate_scaler,
                    NonRetention = non_retention,
                    SizeLimits = size_limits,
                    SLRatio = sl_ratio,
                    FisheryScalers = fishery_scalers
  )
  if(return_tables){

    return(table_list)

  } else {

    return(make_queryable_mock_db_list(table_list))

  }
}

### input validation -------------------------------------------------------

test_that("copy_run validates inputs", {
  fram_db <- make_copyable_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(copy_run(list(1), target_run = 1))

  ## target_run
  expect_error(copy_run(fram_db, "hello"),
               class = "framrsquared_error")
  expect_error(copy_run(fram_db, target_run = 99),
               class = "framrsquared_error")
  expect_error(copy_run(fram_db, target_run = c(1, 2)),
               class = "framrsquared_error")

  ## times
  expect_error(copy_run(fram_db, target_run = 1, times = "ten"),
               class = "framrsquared_error")
  expect_error(copy_run(fram_db, target_run = 1, times = 1:10),
               class = "framrsquared_error")

  ## label
  expect_error(copy_run(fram_db, target_run = 1, label = 10),
               class = "framrsquared_error")
  expect_error(copy_run(fram_db, target_run = 1, label = letters),
               class = "framrsquared_error")

  ## force_many_runs
  expect_error(copy_run(fram_db, target_run = 1, force_many_runs = letters),
               class = "framrsquared_error")
  expect_error(copy_run(fram_db, target_run = 1, force_many_runs = c(TRUE, FALSE)),
               class = "framrsquared_error")


  ## verbose
  expect_error(copy_run(fram_db, target_run = 1, verbose = letters),
               class = "framrsquared_error")
  expect_error(copy_run(fram_db, target_run = 1, verbose = c(TRUE, FALSE)),
               class = "framrsquared_error")

})

### behavior ---------------------------------------------------------------

## Note: Access auto-populates the Primary Key, so even though we see NAs when testing on a SQL db in
## memory, these are behaving correctly.

test_that("copy_run successfully copies", {
  fram_db <- make_copyable_db()
  withr::defer(disconnect_mock_fram_db(fram_db))


  copy_run(fram_db, target_run = 1, times = 4, label = "test")

  copies_and_orig = fetch_table_(fram_db, "StockRecruit") |>
    dplyr::filter(run_id != 2)

  expect_all_equal(copies_and_orig$stock_id, 10)
  expect_all_equal(copies_and_orig$age, 3)
  expect_all_equal(copies_and_orig$recruit_scale_factor, 1)
  expect_all_equal(copies_and_orig$recruit_cohort_size, 100)
  expect_all_equal(copies_and_orig$comment, "a")

  run_id_comments = fetch_table_(fram_db, "RunID")$run_name

  expect_equal(run_id_comments,
               c("first",
                 "second",
                 "first test 1",
                 "first test 2",
                 "first test 3",
                 "first test 4"))
  ## check that we have the updated number of rows for each of our tables
  expect_equal(nrow(fetch_table_(fram_db, "StockFisheryRateScaler")), 6)
  expect_equal(nrow(fetch_table_(fram_db, "NonRetention")), 6)
  expect_equal(nrow(fetch_table_(fram_db, "SizeLimits")), 6)
  expect_equal(nrow(fetch_table_(fram_db, "SLRatio")), 6)
  expect_equal(nrow(fetch_table_(fram_db, "FisheryScalers")), 6)

  ## check `vals` column was copied correctly
  expect_equal(fetch_table_(fram_db, "StockFisheryRateScaler")$vals, c(1, 2, 1, 1, 1, 1))
  expect_equal(fetch_table_(fram_db, "NonRetention")$vals, c(1, 2, 1, 1, 1, 1))
  expect_equal(fetch_table_(fram_db, "SizeLimits")$vals, c(1, 2, 1, 1, 1, 1))
  expect_equal(fetch_table_(fram_db, "SLRatio")$vals, c(1, 2, 1, 1, 1, 1))
  expect_equal(fetch_table_(fram_db, "FisheryScalers")$vals, c(1, 2, 1, 1, 1, 1))
})




test_that("copy_run returns run ids of copies copies", {
  fram_db <- make_copyable_db()
  withr::defer(disconnect_mock_fram_db(fram_db))


  out <- copy_run(fram_db, target_run = 1, times = 4, label = "test")

  ## did we correctly return
  expect_equal(out, 3:6)
})


test_that("copy_run respects verbose", {
  skip_if_slow_tests_disabled()
  fram_db <- make_copyable_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  output <- capture_messages(
    copy_run(fram_db, target_run = 1, times = 200, verbose = TRUE)
  )
  expect_true(grepl("fficial FRAM cannot currently read databases with >150 run ids.", output[1]))

  output <- capture_messages(
    copy_run(fram_db, target_run = 1, times = 200, verbose = FALSE)
  )
  expect_false(grepl("fficial FRAM cannot currently read databases with >150 run ids.", output[1]))
})

test_that("copy_run respects force_many_runs", {
  skip_if_slow_tests_disabled()
  fram_db <- make_copyable_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(copy_run(fram_db, target_run = 1, times = 550, verbose = FALSE),
               regexp = "FRAM databases expected to exceed .mdb memory limits at ~500 runs",
               class = "framrsquared_error")

  output <- capture_messages(
    expect_no_error(
      copy_run(fram_db, target_run = 1, times = 550, verbose = FALSE, force_many_runs = TRUE)
    )
  )
  expect_true(grepl("`force_many_runs` is TRUE, so overriding this failsafe.", output[1]))
})


## copy_tamm ----------------------------------------------------------------

test_that("copy_tamm() errors when tamm_name does not exist", {
  expect_error(
    copy_tamm("nonexistent_file.xlsx", tempdir(), run_id_vec = 1:3),
    class = "framrsquared_error"
  )
})

test_that("copy_tamm() errors on non-Excel file extension", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines("x,y", tmp)
  expect_error(
    copy_tamm(tmp, tempdir(), run_id_vec = 1:3),
    class = "framrsquared_error"
  )
})

test_that("copy_tamm() creates correctly named copies in target folder", {
  tmp_tamm <- withr::local_tempfile(fileext = ".xlsx")
  writeBin(raw(0), tmp_tamm)  # empty but existent xlsx

  target <- withr::local_tempdir()

  copy_tamm(tmp_tamm, target, run_id_vec = 10:12)

  tamm_base <- tools::file_path_sans_ext(basename(tmp_tamm))
  expected <- file.path(target, paste0(tamm_base, "-", 10:12, ".xlsx"))
  expect_true(all(file.exists(expected)))
})

test_that("copy_tamm() respects overwrite = FALSE", {
  tmp_tamm <- withr::local_tempfile(fileext = ".xlsx")
  writeBin(raw(0), tmp_tamm)

  target <- withr::local_tempdir()

  copy_tamm(tmp_tamm, target, run_id_vec = 1L)

  tamm_base <- tools::file_path_sans_ext(basename(tmp_tamm))
  dest_file <- file.path(target, paste0(tamm_base, "-1.xlsx"))

  # Write something distinguishable into the existing copy
  writeLines("original", dest_file)
  original_content <- readLines(dest_file)

  suppressMessages({
    copy_tamm(tmp_tamm, target, run_id_vec = 1L, overwrite = FALSE)
  })

  expect_equal(readLines(dest_file), original_content)
})

test_that("copy_tamm() works with overwrite = TRUE", {
  tmp_tamm <- withr::local_tempfile(fileext = ".xlsx")
  writeLines("firsttamm", tmp_tamm)

  target <- withr::local_tempdir()

  copy_tamm(tmp_tamm, target, run_id_vec = 1L)

  tamm_base <- tools::file_path_sans_ext(basename(tmp_tamm))
  dest_file <- file.path(target, paste0(tamm_base, "-1.xlsx"))

  # Write something distinguishable into the existing copy
  writeLines("secondtamm", tmp_tamm)
  new_content <- readLines(tmp_tamm)

  suppressMessages({
    copy_tamm(tmp_tamm, target, run_id_vec = 1L, overwrite = TRUE)
  })

  expect_equal(readLines(dest_file), new_content)
})


test_that("copy_tamm() creates target_folder if it does not exist", {
  tmp_tamm <- withr::local_tempfile(fileext = ".xlsx")
  writeBin(raw(0), tmp_tamm)

  dir_exist <- withr::local_tempdir()
  new_dir <- file.path(dir_exist, "new_subfolder")
  expect_true(dir.exists(dir_exist))
  expect_false(dir.exists(new_dir))

  copy_tamm(tmp_tamm, new_dir, run_id_vec = 5L)

  expect_true(dir.exists(new_dir))
})




## Make_batch_runs ---------------------------------------


### input validation -------------------------------------
test_that("make_batch_runs() validates fram_db", {
  expect_error(make_batch_runs("not_a_db", 1, "tamm.xlsx", tempdir()))
  expect_error(make_batch_runs(NULL, 1, "tamm.xlsx", tempdir()))
})

test_that("make_batch_runs() validates target_run", {
  fram_db <- make_copyable_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(make_batch_runs(fram_db, 99999, "tamm.xlsx", tempdir()),
               class = "framrsquared_error")  # non-existent run
  expect_error(make_batch_runs(fram_db, "1", "tamm.xlsx", tempdir()),
               class = "framrsquared_error")    # wrong type
  expect_error(make_batch_runs(fram_db, c(1, 2), "tamm.xlsx", tempdir()),
               class = "framrsquared_error") # length > 1
  expect_error(make_batch_runs(fram_db, NULL, "tamm.xlsx", tempdir()),
               class = "framrsquared_error")
})

test_that("make_batch_runs() validates times", {
  fram_db <- make_copyable_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), times = "1"),
               class = "framrsquared_error")
  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), times = 1.5),
               class = "framrsquared_error")
  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), times = -1),
               class = "framrsquared_error")
  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), times = c(1, 2)),
               class = "framrsquared_error")
  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), times = NULL),
               class = "framrsquared_error")
})

test_that("make_batch_runs() validates label", {
  fram_db <- make_copyable_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), label = 1),
               class = "framrsquared_error")
  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), label = c("a", "b")),
               class = "framrsquared_error")
  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), label = NULL),
               class = "framrsquared_error")
  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), label = NA_character_),
               class = "framrsquared_error")
})

test_that("make_batch_runs() validates tamm_name", {
  fram_db <- make_copyable_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(make_batch_runs(fram_db, 139, 123, tempdir()),
               class = "framrsquared_error")
  expect_error(make_batch_runs(fram_db, 139, c("a.xlsx", "b.xlsx"), tempdir()),
               class = "framrsquared_error")
  expect_error(make_batch_runs(fram_db, 139, NULL, tempdir()),
               class = "framrsquared_error")
  expect_error(make_batch_runs(fram_db, 139, NA_character_, tempdir()),
               class = "framrsquared_error")
})

test_that("make_batch_runs() validates force_many_runs and verbose", {
  fram_db <- make_copyable_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), force_many_runs = "true"),
               class = "framrsquared_error")
  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), force_many_runs = 1),
               class = "framrsquared_error")
  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), force_many_runs = NA),
               class = "framrsquared_error")
  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), force_many_runs = c(TRUE, FALSE)),
               class = "framrsquared_error")

  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), verbose = "true"),
               class = "framrsquared_error")
  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), verbose = 1),
               class = "framrsquared_error")
  expect_error(make_batch_runs(fram_db, 139, "tamm.xlsx", tempdir(), verbose = c(TRUE, FALSE)),
               class = "framrsquared_error")
})

### behavior ---------------------------------------------
  test_that("make_batch_runs labels tamm files correctly", {
  fram_db <- make_copyable_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  ## already has run ids 1:2. So new files will be given run ids of 3-7

  tmp_tamm <- withr::local_tempfile(fileext = ".xlsx")
  writeBin(raw(0), tmp_tamm)  # empty but existent xlsx


  target <- withr::local_tempdir()

  suppressMessages({
    make_batch_runs(fram_db, target_run = 1,
                    tamm_name = tmp_tamm,
                    target_folder = target,
                    times = 5)
  })

  file_nums <- gsub(".*-", "", list.files(target))
  file_nums <- as.numeric(gsub("[.]xlsx", "", file_nums))
  expect_equal(file_nums, 3:7)

})

