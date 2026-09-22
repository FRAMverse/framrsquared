# INTEGRATION TESTS: tamm_convergence -----------------------------------------
#
# All tests require a local test database directory (FRAMRSQUARED_TEST_DIR).
# Coho FramCheck file  : path_framcheck_coho()
# Chinook FramCheck file: path_framcheck_chin()

# ==============================================================================
# parse_fram_check()
# ==============================================================================

test_that("parse_fram_check() errors on a non-existent file", {
  skip_if_no_test_db()
  expect_error(
    parse_fram_check("nonexistent/path/FramCheck.Txt"),
    class = "framrsquared_error"
  )
})

test_that("parse_fram_check() runs without error on a Chinook FramCheck file", {
  skip_if_no_test_db()
  expect_no_error(parse_fram_check(path_framcheck_chin()))
})

test_that("parse_fram_check() returns a named list with expected elements", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_coho())
  expect_type(result, "list")
  expect_named(result, c("species", "tamm_name", "trs", "taa_etrs"))
})

test_that("parse_fram_check() identifies species as COHO for a Coho file", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_coho())
  expect_equal(result$species, "COHO")
})

test_that("parse_fram_check() trs element has expected columns", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_coho())
  expect_named(result$trs, c("tamm_iteration", "trs_id", "escapement", "total"))
})

test_that("parse_fram_check() taa_etrs element has expected columns", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_coho())
  expect_named(
    result$taa_etrs,
    c("tamm_iteration", "fishery_name", "fishery_id", "trs_type",
      "time_step", "quota", "target_local")
  )
})

test_that("parse_fram_check() trs contains multiple TAMM iterations", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_coho())
  expect_gt(dplyr::n_distinct(result$trs$tamm_iteration), 1L)
})

test_that("parse_fram_check() trs_type column contains only 'TAA' or 'ETRS'", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_coho())
  expect_true(all(result$taa_etrs$trs_type %in% c("TAA", "ETRS")))
})

test_that("parse_fram_check() returns consistent results", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_coho())
  expect_snapshot_value(result, style = "json2")
})

# ------------------------------------------------------------------------------
# parse_fram_check() – Chinook
# ------------------------------------------------------------------------------

test_that("parse_fram_check() identifies species as CHINOOK for a Chinook file", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_chin())
  expect_equal(result$species, "CHINOOK")
})

test_that("parse_fram_check() returns a named list with expected Chinook elements", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_chin())
  expect_type(result, "list")
  expect_named(
    result,
    c("species", "tamm_name", "final_iteration_count",
      "time_step_sections", "comp_catch_df",
      "hood_canal_nonzero_catch", "tamm_fishery_scaling",
      "negative_escapement")
  )
})

test_that("parse_fram_check() Chinook final_iteration_count is a non-negative numeric scalar", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_chin())
  expect_true(is.numeric(result$final_iteration_count))
  expect_length(result$final_iteration_count, 1L)
  expect_gte(result$final_iteration_count, 0)
})

test_that("parse_fram_check() Chinook time_step_sections is a data frame with expected columns", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_chin())
  expect_s3_class(result$time_step_sections, "data.frame")
  expect_true(all(c("area", "time_step", "tamm_estimate", "tamm_catch",
                     "tamm_scaler", "tamm_chinook_convergence",
                     "tamm_iteration") %in% names(result$time_step_sections)))
})

test_that("parse_fram_check() Chinook time_step_sections contains multiple iterations", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_chin())
  expect_gt(dplyr::n_distinct(result$time_step_sections$tamm_iteration), 1L)
})

test_that("parse_fram_check() Chinook hood_canal_nonzero_catch has expected columns", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_chin())
  expect_s3_class(result$hood_canal_nonzero_catch, "data.frame")
  expect_named(
    result$hood_canal_nonzero_catch,
    c("stock_id", "fishery_id", "landed_catch", "msf_landed_catch")
  )
})

test_that("parse_fram_check() Chinook comp_catch_df is a data frame or NULL", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_chin())
  expect_true(is.data.frame(result$comp_catch_df) || is.null(result$comp_catch_df))
  if (is.data.frame(result$comp_catch_df)) {
    expect_true(all(c("fishery", "time_step", "total_landed_catch",
                       "tamm_iteration") %in% names(result$comp_catch_df)))
  }
})

test_that("parse_fram_check() Chinook tamm_fishery_scaling is a data frame or NULL", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_chin())
  expect_true(is.data.frame(result$tamm_fishery_scaling) || is.null(result$tamm_fishery_scaling))
  if (is.data.frame(result$tamm_fishery_scaling)) {
    expect_true(all(c("fishery", "time_step", "fishery_scaler",
                       "fishery_flag", "tamm_scaler",
                       "tamm_iteration") %in% names(result$tamm_fishery_scaling)))
  }
})

test_that("parse_fram_check() Chinook negative_escapement is NULL or a data frame with expected columns", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_chin())
  expect_true(is.null(result$negative_escapement) ||
                (is.data.frame(result$negative_escapement) &&
                   all(c("stock", "age", "escapement") %in% names(result$negative_escapement))))
})

test_that("parse_fram_check() Chinook returns consistent results", {
  skip_if_no_test_db()
  result <- parse_fram_check(path_framcheck_chin())
  expect_snapshot_value(result, style = "serialize")
})


# ==============================================================================
# check_tamm_convergence()
# ==============================================================================

test_that("check_tamm_convergence() errors on a non-existent file", {
  skip_if_no_test_db()
  expect_error(
    check_tamm_convergence("nonexistent/path/FramCheck.Txt"),
    class = "framrsquared_error"
  )
})

test_that("check_tamm_convergence() errors on a non-numeric threshold_fish", {
  skip_if_no_test_db()
  expect_error(
    check_tamm_convergence(path_framcheck_coho(), threshold_fish = "20"),
    class = "framrsquared_error"
  )
})

test_that("check_tamm_convergence() errors on a non-logical quiet", {
  skip_if_no_test_db()
  expect_error(
    check_tamm_convergence(path_framcheck_coho(), quiet = "yes"),
    class = "framrsquared_error"
  )
})

test_that("check_tamm_convergence() runs without error on a Chinook FramCheck file", {
  skip_if_no_test_db()
  expect_no_error(check_tamm_convergence(path_framcheck_chin(), quiet = TRUE))
})

test_that("check_tamm_convergence() runs without error on a Coho file", {
  skip_if_no_test_db()
  expect_no_error(
    check_tamm_convergence(path_framcheck_coho(), quiet = TRUE)
  )
})

test_that("check_tamm_convergence() returns a named list", {
  skip_if_no_test_db()
  result <- check_tamm_convergence(path_framcheck_coho(), quiet = TRUE)
  expect_type(result, "list")
  expect_named(result, c("trs_diff", "taa_etrs_diff"))
})

test_that("check_tamm_convergence() trs_diff has expected columns", {
  skip_if_no_test_db()
  result <- check_tamm_convergence(path_framcheck_coho(), quiet = TRUE)
  expect_named(result$trs_diff, c("trs_id", "change_in_fish", "relative_change"))
})

test_that("check_tamm_convergence() taa_etrs_diff has expected columns", {
  skip_if_no_test_db()
  result <- check_tamm_convergence(path_framcheck_coho(), quiet = TRUE)
  expect_named(
    result$taa_etrs_diff,
    c("fishery_name", "fishery_id", "time_step", "trs_type",
      "change_in_harvest", "relative_change")
  )
})

test_that("check_tamm_convergence() trs_diff is arranged by descending absolute change", {
  skip_if_no_test_db()
  result <- check_tamm_convergence(path_framcheck_coho(), quiet = TRUE)
  abs_changes <- abs(result$trs_diff$change_in_fish)
  expect_true(all(diff(abs_changes) <= 0))
})

test_that("check_tamm_convergence() returns consistent results", {
  skip_if_no_test_db()
  result <- check_tamm_convergence(path_framcheck_coho(), quiet = TRUE)
  # style = "serialize" used because computed relative_change values are floating-point
  # ratios that cannot round-trip through JSON without precision loss
  expect_snapshot_value(result, style = "serialize")
})

# ------------------------------------------------------------------------------
# check_tamm_convergence() – Chinook
# ------------------------------------------------------------------------------

test_that("check_tamm_convergence() Chinook invisibly returns the parsed list", {
  skip_if_no_test_db()
  result <- check_tamm_convergence(path_framcheck_chin(), quiet = TRUE)
  expect_type(result, "list")
  expect_named(
    result,
    c("species", "tamm_name", "final_iteration_count",
      "time_step_sections", "comp_catch_df",
      "hood_canal_nonzero_catch", "tamm_fishery_scaling",
      "negative_escapement")
  )
})

test_that("check_tamm_convergence() Chinook result matches parse_fram_check() output", {
  skip_if_no_test_db()
  expect_identical(
    check_tamm_convergence(path_framcheck_chin(), quiet = TRUE),
    parse_fram_check(path_framcheck_chin())
  )
})

test_that("check_tamm_convergence() Chinook threshold_fish and quiet args do not cause errors", {
  skip_if_no_test_db()
  # These args are validated before dispatch; confirm validation still runs for Chinook
  expect_error(
    check_tamm_convergence(path_framcheck_chin(), threshold_fish = "20"),
    class = "framrsquared_error"
  )
  expect_error(
    check_tamm_convergence(path_framcheck_chin(), quiet = "yes"),
    class = "framrsquared_error"
  )
})


# ==============================================================================
# plot_tamm_convergence_trs()
# ==============================================================================

test_that("plot_tamm_convergence_trs() errors on a non-existent file", {
  skip_if_no_test_db()
  expect_error(
    plot_tamm_convergence_trs("nonexistent/path/FramCheck.Txt"),
    class = "framrsquared_error"
  )
})

test_that("plot_tamm_convergence_trs() errors on non-numeric n", {
  skip_if_no_test_db()
  expect_error(
    plot_tamm_convergence_trs(path_framcheck_coho(), n = "five"),
    class = "framrsquared_error"
  )
})

test_that("plot_tamm_convergence_trs() errors on non-logical split", {
  skip_if_no_test_db()
  expect_error(
    plot_tamm_convergence_trs(path_framcheck_coho(), split = "yes"),
    class = "framrsquared_error"
  )
})

test_that("plot_tamm_convergence_trs() returns a ggplot on a Coho file", {
  skip_if_no_test_db()
  result <- plot_tamm_convergence_trs(path_framcheck_coho())
  expect_s3_class(result, "ggplot")
})

test_that("plot_tamm_convergence_trs() returns a ggplot when split = TRUE", {
  skip_if_no_test_db()
  result <- plot_tamm_convergence_trs(path_framcheck_coho(), split = TRUE)
  expect_s3_class(result, "ggplot")
})


# ==============================================================================
# plot_tamm_convergence_taa()
# ==============================================================================

test_that("plot_tamm_convergence_taa() errors on a non-existent file", {
  skip_if_no_test_db()
  expect_error(
    plot_tamm_convergence_taa("nonexistent/path/FramCheck.Txt"),
    class = "framrsquared_error"
  )
})

test_that("plot_tamm_convergence_taa() errors on non-numeric n", {
  skip_if_no_test_db()
  expect_error(
    plot_tamm_convergence_taa(path_framcheck_coho(), n = "five"),
    class = "framrsquared_error"
  )
})

test_that("plot_tamm_convergence_taa() errors on non-logical split", {
  skip_if_no_test_db()
  expect_error(
    plot_tamm_convergence_taa(path_framcheck_coho(), split = "yes"),
    class = "framrsquared_error"
  )
})

test_that("plot_tamm_convergence_taa() returns a ggplot on a Coho file", {
  skip_if_no_test_db()
  result <- plot_tamm_convergence_taa(path_framcheck_coho())
  expect_s3_class(result, "ggplot")
})

test_that("plot_tamm_convergence_taa() returns a ggplot when split = TRUE", {
  skip_if_no_test_db()
  result <- plot_tamm_convergence_taa(path_framcheck_coho(), split = TRUE)
  expect_s3_class(result, "ggplot")
})

