# ── Helpers ----------------------------------------------------------

make_scalers_df <- function() {
  data.frame(
    fishery_flag             = c(0, 1, 2, 7, 8, 17, 18, 27, 28),
    fishery_scale_factor     = rep(1.0, 9),
    msf_fishery_scale_factor = rep(1.0, 9),
    quota                    = rep(1000L, 9),
    msf_quota                = rep(1000L, 9)
  )
}

make_nr_df <- function(species) {
  dat <- data.frame(
    non_retention_flag = 0:4,
    cnr_input1         = rep(100L, 5),
    cnr_input2         = rep(100L, 5),
    cnr_input3         = rep(100L, 5),
    cnr_input4         = rep(100L, 5)
  )
  attr(dat, "species") <- species
  dat
}

# UNIT TESTS -------------------------------------------------------------------

## --- translate_nr_flag() ------------------------------------------------------

test_that("translate_nr_flag() returns the correct translation for each flag value", {
  expect_equal(translate_nr_flag(0), "ZERO")
  expect_equal(translate_nr_flag(1), "Computed CNR")
  expect_equal(translate_nr_flag(2), "Ratio of CNR Days")
  expect_equal(translate_nr_flag(3), "Legal/Sublegal Encounters")
  expect_equal(translate_nr_flag(4), "Total Encounters")
})

test_that("translate_nr_flag() handles a vector of all valid values", {
  result <- translate_nr_flag(0:4)
  expect_equal(result, c("ZERO", "Computed CNR", "Ratio of CNR Days",
                         "Legal/Sublegal Encounters", "Total Encounters"))
  expect_type(result, "character")
  expect_length(result, 5L)
})

test_that("translate_nr_flag() errors on non-numeric input", {
  expect_error(translate_nr_flag("1"),     class = "framrsquared_error")
  expect_error(translate_nr_flag(TRUE),    class = "framrsquared_error")
  expect_error(translate_nr_flag(list(1)), class = "framrsquared_error")
})

test_that("translate_nr_flag() errors on out-of-range values", {
  expect_error(translate_nr_flag(5),        class = "framrsquared_error")
  expect_error(translate_nr_flag(-1),       class = "framrsquared_error")
  expect_error(translate_nr_flag(c(1, 99)), class = "framrsquared_error")
})

## --- translate_scalers_flag() -------------------------------------------------

test_that("translate_scalers_flag() returns the correct translation for each flag value", {
  expect_equal(translate_scalers_flag(0),  "ZERO")
  expect_equal(translate_scalers_flag(1),  "Fishery Scaler")
  expect_equal(translate_scalers_flag(2),  "Fishery Quota")
  expect_equal(translate_scalers_flag(7),  "MSF Scaler")
  expect_equal(translate_scalers_flag(8),  "MSF Quota")
  expect_equal(translate_scalers_flag(17), "Scaler + MSF Scaler")
  expect_equal(translate_scalers_flag(18), "Scaler + MSF Quota")
  expect_equal(translate_scalers_flag(27), "Quota + MSF Scaler")
  expect_equal(translate_scalers_flag(28), "Quota + MSF Quota")
})

test_that("translate_scalers_flag() handles a vector of all valid values", {
  vals   <- c(0, 1, 2, 7, 8, 17, 18, 27, 28)
  result <- translate_scalers_flag(vals)
  expect_type(result, "character")
  expect_length(result, length(vals))
})

test_that("translate_scalers_flag() errors on non-numeric input", {
  expect_error(translate_scalers_flag("1"),     class = "framrsquared_error")
  expect_error(translate_scalers_flag(TRUE),    class = "framrsquared_error")
  expect_error(translate_scalers_flag(list(1)), class = "framrsquared_error")
})

test_that("translate_scalers_flag() errors on invalid flag values", {
  expect_error(translate_scalers_flag(3),        class = "framrsquared_error")
  expect_error(translate_scalers_flag(9),        class = "framrsquared_error")
  expect_error(translate_scalers_flag(-1),       class = "framrsquared_error")
  expect_error(translate_scalers_flag(c(1, 99)), class = "framrsquared_error")
})

## --- na_scalers_from_flag() ---------------------------------------------------

test_that("na_scalers_from_flag() errors on non-dataframe input", {
  expect_error(na_scalers_from_flag(list(fishery_flag = 1)), class = "framrsquared_error")
  expect_error(na_scalers_from_flag(c(1, 2, 3)),             class = "framrsquared_error")
})

test_that("na_scalers_from_flag() errors when required columns are missing", {
  bad_df <- data.frame(fishery_flag = 1, fishery_scale_factor = 1.0)
  expect_error(na_scalers_from_flag(bad_df), class = "framrsquared_error")
})

test_that("na_scalers_from_flag() preserves the species attribute", {
  df <- make_scalers_df()
  attr(df, "species") <- "CHINOOK"
  expect_equal(attr(na_scalers_from_flag(df), "species"), "CHINOOK")
})

test_that("na_scalers_from_flag() correctly NAs unused columns for each flag", {
  result <- na_scalers_from_flag(make_scalers_df())
  # Rows correspond to flags: 0, 1, 2, 7, 8, 17, 18, 27, 28

  # flag 0: all scaler columns NA
  expect_true(all(is.na(result[1, c("fishery_scale_factor", "msf_fishery_scale_factor",
                                     "quota", "msf_quota")])))

  # flag 1: only fishery_scale_factor kept
  expect_false(is.na(result$fishery_scale_factor[2]))
  expect_true(is.na(result$msf_fishery_scale_factor[2]))
  expect_true(is.na(result$quota[2]))
  expect_true(is.na(result$msf_quota[2]))

  # flag 2: only quota kept
  expect_true(is.na(result$fishery_scale_factor[3]))
  expect_true(is.na(result$msf_fishery_scale_factor[3]))
  expect_false(is.na(result$quota[3]))
  expect_true(is.na(result$msf_quota[3]))

  # flag 7: only msf_fishery_scale_factor kept
  expect_true(is.na(result$fishery_scale_factor[4]))
  expect_false(is.na(result$msf_fishery_scale_factor[4]))
  expect_true(is.na(result$quota[4]))
  expect_true(is.na(result$msf_quota[4]))

  # flag 8: only msf_quota kept
  expect_true(is.na(result$fishery_scale_factor[5]))
  expect_true(is.na(result$msf_fishery_scale_factor[5]))
  expect_true(is.na(result$quota[5]))
  expect_false(is.na(result$msf_quota[5]))

  # flag 17: fishery_scale_factor + msf_fishery_scale_factor kept
  expect_false(is.na(result$fishery_scale_factor[6]))
  expect_false(is.na(result$msf_fishery_scale_factor[6]))
  expect_true(is.na(result$quota[6]))
  expect_true(is.na(result$msf_quota[6]))

  # flag 18: fishery_scale_factor + msf_quota kept
  expect_false(is.na(result$fishery_scale_factor[7]))
  expect_true(is.na(result$msf_fishery_scale_factor[7]))
  expect_true(is.na(result$quota[7]))
  expect_false(is.na(result$msf_quota[7]))

  # flag 27: msf_fishery_scale_factor + quota kept
  expect_true(is.na(result$fishery_scale_factor[8]))
  expect_false(is.na(result$msf_fishery_scale_factor[8]))
  expect_false(is.na(result$quota[8]))
  expect_true(is.na(result$msf_quota[8]))

  # flag 28: quota + msf_quota kept
  expect_true(is.na(result$fishery_scale_factor[9]))
  expect_true(is.na(result$msf_fishery_scale_factor[9]))
  expect_false(is.na(result$quota[9]))
  expect_false(is.na(result$msf_quota[9]))
})

## --- na_non_retention_from_flag() --------------------------------------------

test_that("na_non_retention_from_flag() errors on non-dataframe input", {
  expect_error(na_non_retention_from_flag(list(non_retention_flag = 1)), class = "framrsquared_error")
  expect_error(na_non_retention_from_flag(c(1, 2, 3)),                   class = "framrsquared_error")
})

test_that("na_non_retention_from_flag() errors when required columns are missing", {
  bad_df <- data.frame(non_retention_flag = 1:3, cnr_input1 = 1:3)
  attr(bad_df, "species") <- "CHINOOK"
  expect_error(na_non_retention_from_flag(bad_df), class = "framrsquared_error")
})

test_that("na_non_retention_from_flag() errors for unrecognised species", {
  df <- make_nr_df("SALMON")
  expect_error(na_non_retention_from_flag(df), class = "framrsquared_error")
})

test_that("na_non_retention_from_flag() preserves the species attribute", {
  chin_result <- na_non_retention_from_flag(make_nr_df("CHINOOK"))
  expect_equal(attr(chin_result, "species"), "CHINOOK")

  coho_result <- na_non_retention_from_flag(make_nr_df("COHO"))
  expect_equal(attr(coho_result, "species"), "COHO")
})

test_that("na_non_retention_from_flag() COHO: cnr_input2:4 always NA, cnr_input1 preserved", {
  result <- na_non_retention_from_flag(make_nr_df("COHO"))
  expect_true(all(!is.na(result$cnr_input1)))
  expect_true(all(is.na(result$cnr_input2)))
  expect_true(all(is.na(result$cnr_input3)))
  expect_true(all(is.na(result$cnr_input4)))
})

test_that("na_non_retention_from_flag() CHINOOK: correct NAs per flag value", {
  result <- na_non_retention_from_flag(make_nr_df("CHINOOK"))
  # Rows correspond to flags 0:4

  # flag 0: all NA
  expect_true(all(is.na(result[1, c("cnr_input1", "cnr_input2",
                                     "cnr_input3", "cnr_input4")])))

  # flag 1: cnr_input3 and cnr_input4 kept; cnr_input1 and cnr_input2 NA
  expect_true(is.na(result$cnr_input1[2]))
  expect_true(is.na(result$cnr_input2[2]))
  expect_false(is.na(result$cnr_input3[2]))
  expect_false(is.na(result$cnr_input4[2]))

  # flag 2: all four inputs kept
  expect_true(all(!is.na(result[3, c("cnr_input1", "cnr_input2",
                                      "cnr_input3", "cnr_input4")])))

  # flag 3: cnr_input1 and cnr_input2 kept; cnr_input3 and cnr_input4 NA
  expect_false(is.na(result$cnr_input1[4]))
  expect_false(is.na(result$cnr_input2[4]))
  expect_true(is.na(result$cnr_input3[4]))
  expect_true(is.na(result$cnr_input4[4]))

  # flag 4: only cnr_input1 kept
  expect_false(is.na(result$cnr_input1[5]))
  expect_true(is.na(result$cnr_input2[5]))
  expect_true(is.na(result$cnr_input3[5]))
  expect_true(is.na(result$cnr_input4[5]))
})
