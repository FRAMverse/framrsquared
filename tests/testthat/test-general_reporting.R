# ── Helpers -------------------------------------------------------------------

# Builds a minimal mortality data frame containing every column that
# add_total_mortality() and coho_mark_rates() require.
make_mort_row <- function(run_id     = 1L,
                          fishery_id = 1L,
                          stock_id   = 1L,
                          age        = 3L,
                          time_step  = 1L,
                          landed_catch  = 0.0,
                          encounter     = 0.0,
                          msf_encounter = 0.0) {
  data.frame(
    primary_key       = 1L,
    run_id            = as.integer(run_id),
    fishery_id        = as.integer(fishery_id),
    stock_id          = as.integer(stock_id),
    age               = as.integer(age),
    time_step         = as.integer(time_step),
    landed_catch      = as.double(landed_catch),
    non_retention     = 0.0,
    shaker            = 0.0,
    drop_off          = 0.0,
    msf_landed_catch  = 0.0,
    msf_non_retention = 0.0,
    msf_shaker        = 0.0,
    msf_drop_off      = 0.0,
    encounter         = as.double(encounter),
    msf_encounter     = as.double(msf_encounter)
  )
}

# ------ cohort_abundance() mock ----------------------------------------------
# Needs: RunID, StockRecruit, BaseCohort.
# Uses label_stocks() from framrosetta, so stock_id must be in its valid range.
# CHINOOK stock IDs: 1–78; COHO stock IDs: 1–246.
make_cohort_abundance_mock_db <- function(species = "CHINOOK",
                                          scale_factor = 1.0,
                                          base_cohort  = 10000.0,
                                          stock_id     = 1L) {
  run_id_tbl <- data.frame(run_id = 1L, base_period_id = 10L,
                            run_name = "TestRun")

  stock_recruit <- data.frame(run_id              = 1L,
                               stock_id            = stock_id,
                               age                 = 3L,
                               recruit_scale_factor = scale_factor)

  base_cohort_tbl <- data.frame(base_period_id  = 10L,
                                 stock_id        = stock_id,
                                 age             = 3L,
                                 base_cohort_size = base_cohort)

  make_queryable_mock_db_list(
    table_list = list(RunID       = run_id_tbl,
                      StockRecruit = stock_recruit,
                      BaseCohort  = base_cohort_tbl),
    species = species
  )
}

# ------ stock_fate() mock (shared Chinook / Coho) ----------------------------
# Needs: RunID, BaseID, Stock (label_stocks_db), Cohort, Escapement, Mortality.
# stock_id 1 is valid in the framrosetta CHINOOK lookup, but label_stocks_db
# uses the DB Stock table, so any ID is fine here.
make_stock_fate_mock_db <- function(species = "CHINOOK") {
  run_id_tbl <- data.frame(run_id = c(1L, 2L), base_period_id = 10L,
                            run_name = c("Run A", "Run B"))
  base_id    <- data.frame(base_period_id    = 10L,
                            fishery_version   = 1L,
                            stock_version     = 1L,
                            time_step_version = 1L,
                            species_name      = species,
                            base_period_name  = "BP")
  stock_tbl  <- data.frame(stock_id        = 1L,
                            stock_version   = 1L,
                            species         = species,
                            stock_long_name = "Test Stock",
                            stock_name      = "TS")

  cohort <- data.frame(run_id       = c(1L, 2L),
                        stock_id     = 1L,
                        age          = 3L,
                        time_step    = 1L,
                        start_cohort  = 1000.0,  # -> starting_cohort
                        working_cohort = 900.0,  # -> post_nat_mort
                        cohort        = 800.0,   # -> post_pre_terminal (= age_up for Chinook)
                        mature_cohort  = 100.0)

  escapement <- data.frame(primary_key = c(1L, 2L),
                            run_id     = c(1L, 2L),
                            stock_id   = 1L,
                            age        = 3L,
                            time_step  = 1L,
                            escapement = 200.0)

  # natural_mortality = start - working = 100
  # fishery_mortality = landed_catch = 100
  # age_up (Chinook) = min(cohort) = 800; escapement_to_river = 200
  # total = 100+100+800+200 = 1200  -> percentages sum to 1
  mort <- rbind(make_mort_row(run_id = 1L, fishery_id = 1L, stock_id = 1L,
                               age = 3L, time_step = 1L, landed_catch = 100.0),
                make_mort_row(run_id = 2L, fishery_id = 1L, stock_id = 1L,
                               age = 3L, time_step = 1L, landed_catch = 100.0))

  make_queryable_mock_db_list(
    table_list = list(RunID      = run_id_tbl,
                      BaseID     = base_id,
                      Stock      = stock_tbl,
                      Cohort     = cohort,
                      Escapement = escapement,
                      Mortality  = mort),
    species = species
  )
}

# ------ coho_mark_rates() mock -----------------------------------------------
# Needs: Mortality, Fishery, RunID, FisheryScalers.
# Uses label_fisheries(species = "COHO") from framrosetta, so fishery_id must
# be a valid COHO ID (1–198). Using fishery_id 1 (NS, flag 1) and 2 (MSF, flag 7).
# stock_id 2 (even) -> AD; stock_id 1 (odd) -> UM.
make_coho_mark_rates_mock_db <- function() {
  run_id_tbl <- data.frame(run_id = 1L, run_year = 2024L,
                            base_period_id = 10L)

  fishery_tbl <- data.frame(fishery_id     = c(1L, 2L),
                              version_number = 1L,
                              species        = "COHO",
                              fishery_title  = c("F1", "F2"),
                              fishery_name   = c("F1", "F2"))

  fishery_scalers <- data.frame(run_id       = c(1L, 1L),
                                 fishery_id   = c(1L, 2L),
                                 time_step    = c(1L, 1L),
                                 fishery_flag = c(1L, 7L))  # fishery 1 = NS, fishery 2 = MSF

  # Fishery 1 (NS, flag = 1): encounter used, msf_encounter zeroed
  #   stock 2 (AD): encounter = 60
  #   stock 1 (UM): encounter = 40
  #   -> ns mark_rate = 60 / (60+40) = 0.6
  #
  # Fishery 2 (MSF, flag = 7): msf_encounter used, encounter zeroed
  #   stock 2 (AD): msf_encounter = 30
  #   stock 1 (UM): msf_encounter = 20
  #   -> msf mark_rate = 30 / (30+20) = 0.6
  mort <- rbind(
    make_mort_row(run_id=1L, fishery_id=1L, stock_id=2L, age=3L, time_step=1L,
                  encounter=60.0, msf_encounter=0.0),
    make_mort_row(run_id=1L, fishery_id=1L, stock_id=1L, age=3L, time_step=1L,
                  encounter=40.0, msf_encounter=0.0),
    make_mort_row(run_id=1L, fishery_id=2L, stock_id=2L, age=3L, time_step=1L,
                  encounter=0.0,  msf_encounter=30.0),
    make_mort_row(run_id=1L, fishery_id=2L, stock_id=1L, age=3L, time_step=1L,
                  encounter=0.0,  msf_encounter=20.0)
  )
  mort$primary_key <- 1:4L

  make_queryable_mock_db_list(
    table_list = list(RunID          = run_id_tbl,
                      Fishery        = fishery_tbl,
                      FisheryScalers = fishery_scalers,
                      Mortality      = mort),
    species = "COHO"
  )
}


# UNIT TESTS -------------------------------------------------------------------

## --- coho_mark_rates() -------------------------------------------------------

test_that("coho_mark_rates() errors on a transfer database", {
  fram_db <- make_mock_fram_db(type = "transfer", species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(coho_mark_rates(fram_db), class = "framrsquared_error")
})

test_that("coho_mark_rates() errors on a Chinook database", {
  fram_db <- make_mock_fram_db(type = "full", species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(coho_mark_rates(fram_db), class = "framrsquared_error")
})

test_that("coho_mark_rates() returns a data frame with expected columns", {
  fram_db <- make_coho_mark_rates_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- coho_mark_rates(fram_db)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("run_id", "fishery_id", "AD", "UM",
                    "time_step", "fishery_type", "mark_rate") %in% names(result)))
})

test_that("coho_mark_rates() computes mark_rate as AD / (AD + UM)", {
  fram_db <- make_coho_mark_rates_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- coho_mark_rates(fram_db)

  ns_row <- result[result$fishery_id == 1L & result$fishery_type == "ns", ]
  expect_equal(ns_row$AD, 60)
  expect_equal(ns_row$UM, 40)
  expect_equal(ns_row$mark_rate, 0.6)
})

test_that("coho_mark_rates() zeros ns_encounters for an MSF-only fishery (flag 7)", {
  fram_db <- make_coho_mark_rates_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- coho_mark_rates(fram_db)

  msf_ns_row <- result[result$fishery_id == 2L & result$fishery_type == "ns", ]
  expect_equal(msf_ns_row$AD + msf_ns_row$UM, 0)
})

test_that("coho_mark_rates() uses msf_encounter for an MSF-only fishery (flag 7)", {
  fram_db <- make_coho_mark_rates_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- coho_mark_rates(fram_db)

  msf_row <- result[result$fishery_id == 2L & result$fishery_type == "msf", ]
  expect_equal(msf_row$AD, 30)
  expect_equal(msf_row$UM, 20)
  expect_equal(msf_row$mark_rate, 0.6)
})

test_that("coho_mark_rates() assigns mark based on stock_id parity (even = AD, odd = UM)", {
  fram_db <- make_coho_mark_rates_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- coho_mark_rates(fram_db)

  # stock_id 2 (even) = AD; stock_id 1 (odd) = UM
  # For fishery 1 ns: AD = 60 (from stock 2) and UM = 40 (from stock 1)
  ns_row <- result[result$fishery_id == 1L & result$fishery_type == "ns", ]
  expect_equal(ns_row$AD, 60)
  expect_equal(ns_row$UM, 40)
})

test_that("coho_mark_rates() filters to run_id when specified", {
  fram_db <- make_coho_mark_rates_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- coho_mark_rates(fram_db, run_id = 1L)
  expect_true(all(result$run_id == 1L))
})


## --- cohort_abundance() ------------------------------------------------------

test_that("cohort_abundance() errors on a transfer database", {
  fram_db <- make_mock_fram_db(type = "transfer")
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(cohort_abundance(fram_db), class = "framrsquared_error")
})

test_that("cohort_abundance() returns a data frame with expected columns", {
  fram_db <- make_cohort_abundance_mock_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- cohort_abundance(fram_db)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("run_id", "stock_id", "age", "recruit_cohorts") %in% names(result)))
})

test_that("cohort_abundance() computes recruit_cohorts as scale_factor * base_cohort_size", {
  fram_db <- make_cohort_abundance_mock_db(scale_factor = 1.5, base_cohort = 10000.0)
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- cohort_abundance(fram_db)
  expect_equal(result$recruit_cohorts, 15000)
})

test_that("cohort_abundance() sets the species attribute on the result", {
  chin_db <- make_cohort_abundance_mock_db(species = "CHINOOK")
  coho_db <- make_cohort_abundance_mock_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(chin_db))
  withr::defer(disconnect_mock_fram_db(coho_db))

  expect_equal(attr(cohort_abundance(chin_db), "species"), "CHINOOK")
  expect_equal(attr(cohort_abundance(coho_db), "species"), "COHO")
})

test_that("cohort_abundance() filters to run_id when specified", {
  # Two-run mock: only run 1 should be returned
  run_id_tbl  <- data.frame(run_id = c(1L, 2L), base_period_id = 10L,
                              run_name = c("R1", "R2"))
  sr          <- data.frame(run_id = c(1L, 2L), stock_id = 1L, age = 3L,
                             recruit_scale_factor = 1.0)
  bc          <- data.frame(base_period_id = 10L, stock_id = 1L, age = 3L,
                             base_cohort_size = 1000.0)
  fram_db <- make_queryable_mock_db_list(
    table_list = list(RunID = run_id_tbl, StockRecruit = sr, BaseCohort = bc)
  )
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- cohort_abundance(fram_db, run_id = 1L)
  expect_true(all(result$run_id == 1L))
  expect_equal(nrow(result), 1L)
})

test_that("cohort_abundance() returns all runs when run_id is NULL", {
  run_id_tbl  <- data.frame(run_id = c(1L, 2L), base_period_id = 10L,
                              run_name = c("R1", "R2"))
  sr          <- data.frame(run_id = c(1L, 2L), stock_id = 1L, age = 3L,
                             recruit_scale_factor = 1.0)
  bc          <- data.frame(base_period_id = 10L, stock_id = 1L, age = 3L,
                             base_cohort_size = 1000.0)
  fram_db <- make_queryable_mock_db_list(
    table_list = list(RunID = run_id_tbl, StockRecruit = sr, BaseCohort = bc)
  )
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- cohort_abundance(fram_db, run_id = NULL)
  expect_equal(nrow(result), 2L)
})


## --- stock_fate() ------------------------------------------------------------

test_that("stock_fate() errors on an invalid fram_db", {
  expect_error(stock_fate(list()),     class = "framrsquared_error")
  expect_error(stock_fate("not_a_db"), class = "framrsquared_error")
})

test_that("stock_fate() errors on an invalid units argument", {
  fram_db <- make_mock_fram_db()
  withr::defer(disconnect_mock_fram_db(fram_db))

  expect_error(stock_fate(fram_db, units = "proportion"))
  expect_error(stock_fate(fram_db, units = "counts"))
})

test_that("stock_fate() returns a data frame for a Chinook database", {
  fram_db <- make_stock_fate_mock_db(species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- stock_fate(fram_db)
  expect_s3_class(result, "data.frame")
})

test_that("stock_fate() returns Chinook-specific columns", {
  fram_db <- make_stock_fate_mock_db(species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- stock_fate(fram_db)
  expect_true(all(c("natural_mortality", "fishery_mortality",
                    "age_up", "escapement_to_river") %in% names(result)))
  expect_false("escapement_spawning" %in% names(result))
})

test_that("stock_fate() returns Coho-specific columns", {
  fram_db <- make_stock_fate_mock_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- stock_fate(fram_db)
  expect_true(all(c("natural_mortality", "fishery_mortality",
                    "escapement_spawning") %in% names(result)))
  expect_false("age_up" %in% names(result))
  expect_false("escapement_to_river" %in% names(result))
})

test_that("stock_fate() sets the species attribute on the result", {
  chin_db <- make_stock_fate_mock_db(species = "CHINOOK")
  coho_db <- make_stock_fate_mock_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(chin_db))
  withr::defer(disconnect_mock_fram_db(coho_db))

  expect_equal(attr(stock_fate(chin_db), "species"), "CHINOOK")
  expect_equal(attr(stock_fate(coho_db), "species"), "COHO")
})

test_that("stock_fate() filters to run_id when specified", {
  fram_db <- make_stock_fate_mock_db(species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- stock_fate(fram_db, run_id = 1L)
  expect_true(all(result$run_id == 1L))
})

test_that("stock_fate() Chinook percentage mode: fates sum to 1 per stock-age", {
  fram_db <- make_stock_fate_mock_db(species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- stock_fate(fram_db, run_id = 1L, units = "percentage")

  row_sums <- rowSums(result[, c("natural_mortality", "fishery_mortality",
                                  "age_up", "escapement_to_river")])
  expect_equal(row_sums, rep(1, nrow(result)), tolerance = 1e-9)
})

test_that("stock_fate() Coho percentage mode: fates sum to 1 per stock-age", {
  fram_db <- make_stock_fate_mock_db(species = "COHO")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- stock_fate(fram_db, run_id = 1L, units = "percentage")

  row_sums <- rowSums(result[, c("natural_mortality", "fishery_mortality",
                                  "escapement_spawning")])
  expect_equal(row_sums, rep(1, nrow(result)), tolerance = 1e-9)
})

test_that("stock_fate() fish mode returns raw counts, not proportions", {
  fram_db <- make_stock_fate_mock_db(species = "CHINOOK")
  withr::defer(disconnect_mock_fram_db(fram_db))

  result <- stock_fate(fram_db, run_id = 1L, units = "fish")

  # natural_mortality = start_cohort - working_cohort = 1000 - 900 = 100
  expect_equal(result$natural_mortality, 100)
  # fishery_mortality = landed_catch = 100
  expect_equal(result$fishery_mortality, 100)
})
