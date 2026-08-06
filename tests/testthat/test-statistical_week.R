# 2024-01-01 is a Monday → wday == 2, no +1 adjustment
# 2023-01-01 is a Sunday → wday == 1, +1 adjustment applied
# 2025-01-01 is a Wednesday → wday == 4, +1 adjustment applied

test_that("statistical_week() returns double output", {
  # as.integer(...) + 1 (double literal) promotes to double via dplyr::if_else
  expect_type(statistical_week(as.Date("2023-06-15")), "double")
})

test_that("statistical_week() returns a vector of the same length as input", {
  dates <- as.Date(c("2024-01-01", "2024-06-15", "2024-12-31"))
  expect_length(statistical_week(dates), 3L)
})

# --- Date method ---

test_that("statistical_week.Date() gives correct result when year starts on Monday (no adjustment)", {
  # 2024-01-01 is Monday; %W = 1, no +1 → 1
  expect_equal(statistical_week(as.Date("2024-01-01")), 1)
  # 2024-01-07 is Sunday (last day of week 1); %W = 1 → 1
  expect_equal(statistical_week(as.Date("2024-01-07")), 1)
  # 2024-01-08 is next Monday; %W = 2 → 2
  expect_equal(statistical_week(as.Date("2024-01-08")), 2)
})

test_that("statistical_week.Date() gives correct result when year starts on non-Monday (+1 adjustment)", {
  # 2023-01-01 is Sunday; %W = 0, +1 → 1
  expect_equal(statistical_week(as.Date("2023-01-01")), 1)
  # 2023-01-02 is first Monday of year; %W = 1, +1 → 2
  expect_equal(statistical_week(as.Date("2023-01-02")), 2)
  # 2023-01-08 is Sunday; %W = 1, +1 → 2 (still in same week)
  expect_equal(statistical_week(as.Date("2023-01-08")), 2)
  # 2023-01-09 is second Monday; %W = 2, +1 → 3
  expect_equal(statistical_week(as.Date("2023-01-09")), 3)
})

test_that("statistical_week.Date() gives correct result when year starts on mid-week (+1 adjustment)", {
  # 2025-01-01 is Wednesday; %W = 0, +1 → 1
  expect_equal(statistical_week(as.Date("2025-01-01")), 1)
  # 2025-01-06 is first Monday; %W = 1, +1 → 2
  expect_equal(statistical_week(as.Date("2025-01-06")), 2)
  # 2025-01-13 is second Monday; %W = 2, +1 → 3
  expect_equal(statistical_week(as.Date("2025-01-13")), 3)
})

test_that("statistical_week.Date() handles a vector of dates", {
  dates <- as.Date(c("2024-01-01", "2024-01-08"))
  expect_equal(statistical_week(dates), c(1, 2))
})

test_that("statistical_week.Date() propagates NAs", {
  expect_equal(statistical_week(as.Date(NA)), NA_real_)
  result <- statistical_week(as.Date(c("2024-01-01", NA)))
  expect_equal(result, c(1, NA_real_))
})

# --- POSIXct method ---

test_that("statistical_week.POSIXct() produces the same result as the Date method", {
  d  <- as.Date("2024-03-15")
  ct <- as.POSIXct("2024-03-15 14:30:00")
  expect_equal(statistical_week(ct), statistical_week(d))
})

test_that("statistical_week.POSIXct() handles a vector of POSIXct values", {
  ct_vec <- as.POSIXct(c("2024-01-01 00:00:00", "2024-01-08 12:00:00"))
  expect_equal(statistical_week(ct_vec), c(1, 2))
})

# --- character method ---

test_that("statistical_week.character() parses ISO format (YYYY-MM-DD)", {
  expect_equal(statistical_week("2024-01-01"), 1)
  expect_equal(statistical_week("2023-01-01"), 1)
})

test_that("statistical_week.character() parses US format (MM/DD/YYYY)", {
  expect_equal(statistical_week("01/01/2024"), 1)
  expect_equal(statistical_week("01/01/2023"), 1)
})

test_that("statistical_week.character() ISO and US formats give identical results", {
  expect_equal(
    statistical_week("2024-06-15"),
    statistical_week("06/15/2024")
  )
})

test_that("statistical_week.character() handles a character vector", {
  expect_equal(
    statistical_week(c("2024-01-01", "2024-01-08")),
    c(1, 2)
  )
})

test_that("statistical_week.character() errors on ambiguous/unrecognized formats", {
  expect_error(statistical_week("July 4, 2024"),  class = "framrsquared_error")
  expect_error(statistical_week("2024.07.04"),    class = "framrsquared_error")
  expect_error(statistical_week("not-a-date"),    class = "framrsquared_error")
})

# --- mid-year pin ---

test_that("statistical_week.Date() pins July 4 correctly across branch years", {
  # 2023-01-01 Sunday (+1):   2023-07-04 (Tue) → %W = 27, +1 = 28
  expect_equal(statistical_week(as.Date("2023-07-04")), 28)
  # 2024-01-01 Monday (no +1): 2024-07-04 (Thu) → %W = 27, no +1 = 27
  expect_equal(statistical_week(as.Date("2024-07-04")), 27)
  # 2025-01-01 Wednesday (+1): 2025-07-04 (Fri) → %W = 26, +1 = 27
  expect_equal(statistical_week(as.Date("2025-07-04")), 27)
})

# --- dispatch ---

test_that("statistical_week() dispatches correctly for all supported types", {
  expect_no_error(statistical_week(as.Date("2024-01-01")))
  expect_no_error(statistical_week(as.POSIXct("2024-01-01")))
  expect_no_error(statistical_week("2024-01-01"))
})
