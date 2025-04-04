test_that("standardize_species works", {
  expect_equal(standardize_species("chin"), "CHINOOK")
  expect_equal(standardize_species("coHo"), "COHO")
  expect_error(standardize_species("Cooho"))
})


test_that("validate_species works",{
  dat1 = data.frame(fishery_id = 1:5,
                    let = letters[1:5])
  dat2 = data.frame(fishery_id = 1:5,
                    let = letters[1:5])
  attr(dat2, "species") <- "CHINOOK"
  expect_equal(validate_species(dat1, "COHO"), "COHO")
  expect_equal(validate_species(dat2), "CHINOOK")
  expect_error(validate_spacies(dat1))
  expect_error(validate_species(dat2, "COHO"))
})
