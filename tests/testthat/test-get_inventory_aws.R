test_that("get_inventory_aws returns expected output", {
  skip_if_offline()

  # Use a known station ID for testing
  inventory <- get_inventory_aws("USW00094728")

  expect_s3_class(inventory, "data.table")
  expect_true(nrow(inventory) > 0)
  expect_true(all(c("station_id", "element", "first_year", "last_year") %in% names(inventory)))
  expect_true(all(inventory$station_id == "USW00094728"))
})

test_that("get_inventory_aws handles invalid inputs", {
  expect_error(get_inventory_aws("invalid_id"))
  expect_error(get_inventory_aws(123))  # Non-character input
})
