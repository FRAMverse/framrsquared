test_that("fetch_table basics", {
  skip_if_no_test_db()
  db <- connection_chin_post()
  expect_no_error(fetch_table(db, "Stock"))
  expect_error(fetch_table(db, "stock"))
  disconnect_fram_db(db)
})
