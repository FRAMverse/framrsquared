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

test_that("parse_fram_check() errors on a Chinook FramCheck file", {
  skip_if_no_test_db()
  expect_error(
    parse_fram_check(path_framcheck_chin()),
    class = "framrsquared_error"
  )
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

test_that("check_tamm_convergence() errors on a Chinook FramCheck file", {
  skip_if_no_test_db()
  expect_error(
    check_tamm_convergence(path_framcheck_chin()),
    class = "framrsquared_error"
  )
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
