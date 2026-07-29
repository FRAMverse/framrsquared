# 2023-01-01 is a Sunday  → wday == 1, no +1 adjustment
# 2024-01-01 is a Monday → wday == 2, +1 adjustment applied

test_that("management_week() returns double output", {
  # as.integer(...) + 1 (double literal) promotes to double
  expect_type(management_week(as.Date("2023-06-15")), "double")
})

test_that("management_week() returns a vector of the same length as input", {
  dates <- as.Date(c("2023-01-01", "2023-06-15", "2023-12-31"))
  expect_length(management_week(dates), 3L)
})

# --- Date method ---

test_that("management_week.Date() gives correct result when year starts on Sunday (no adjustment)", {
  # 2023-01-01 is Sunday; %U = 1 (first Sunday = week 1), no +1 → 1
  expect_equal(management_week(as.Date("2023-01-01")), 1)
  # 2023-01-08: second Sunday, %U = 2 → 2
  expect_equal(management_week(as.Date("2023-01-07")), 1)
  expect_equal(management_week(as.Date("2023-01-08")), 2)
})

test_that("management_week.Date() gives correct result when year starts on non-Sunday (+1 adjustment)", {
  # 2024-01-01 is Monday; %U = 0 (before first Sunday), +1 → 1
  expect_equal(management_week(as.Date("2024-01-01")), 1)
  # 2024-01-07 is first Sunday; %U = 1, +1 → 2
  expect_equal(management_week(as.Date("2024-01-07")), 2)
})

test_that("management_week.Date() handles a vector of dates", {
  dates <- as.Date(c("2023-01-01", "2023-01-08"))
  expect_equal(management_week(dates), c(1, 2))
})

test_that("management_week.Date() propagates NAs", {
  expect_equal(management_week(as.Date(NA)), NA_real_)
  result <- management_week(as.Date(c("2023-01-01", NA)))
  expect_equal(result, c(1, NA_real_))
})

# --- POSIXct method ---

test_that("management_week.POSIXct() produces the same result as the Date method", {
  d  <- as.Date("2024-03-15")
  ct <- as.POSIXct("2024-03-15 14:30:00")
  expect_equal(management_week(ct), management_week(d))
})

test_that("management_week.POSIXct() handles a vector of POSIXct values", {
  ct_vec <- as.POSIXct(c("2023-01-01 00:00:00", "2023-01-08 12:00:00"))
  expect_equal(management_week(ct_vec), c(1, 2))
})

# --- character method ---

test_that("management_week.character() parses ISO format (YYYY-MM-DD)", {
  expect_equal(management_week("2023-01-01"), 1)
  expect_equal(management_week("2024-01-01"), 1)
})

test_that("management_week.character() parses US format (MM/DD/YYYY)", {
  expect_equal(management_week("01/01/2023"), 1)
  expect_equal(management_week("01/01/2024"), 1)
})

test_that("management_week.character() ISO and US formats give identical results", {
  expect_equal(
    management_week("2024-06-15"),
    management_week("06/15/2024")
  )
})

test_that("management_week.character() handles a character vector", {
  expect_equal(
    management_week(c("2023-01-01", "2023-01-08")),
    c(1, 2)
  )
})

test_that("management_week.character() errors on ambiguous/unrecognized formats", {
  expect_error(management_week("July 4, 2023"),      class = "framrsquared_error")
  expect_error(management_week("2023.07.04"),         class = "framrsquared_error")
  expect_error(management_week("not-a-date"),         class = "framrsquared_error")
})

# --- mid-year pin ---

test_that("management_week.Date() pins July 4 to week 27 for both branch years", {
  # 2023-01-01 is Sunday (no +1 adjustment); 2024-01-01 is Monday (+1 adjustment)
  # Both should normalize to the same management week for the same calendar date
  expect_equal(management_week(as.Date("2023-07-04")), 27)
  expect_equal(management_week(as.Date("2024-07-04")), 27)
})

# --- dispatch ---

test_that("management_week() dispatches correctly for all supported types", {
  expect_no_error(management_week(as.Date("2024-01-01")))
  expect_no_error(management_week(as.POSIXct("2024-01-01")))
  expect_no_error(management_week("2024-01-01"))
})
