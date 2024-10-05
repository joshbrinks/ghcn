test_that("get_data_aws returns expected output", {
  skip_if_offline()

  # Use a known station ID and date range for testing
  data <- get_data_aws("USW00094728", "2022-01-01", "2022-01-31", c("TMAX", "TMIN"))

  expect_s3_class(data, "data.table")
  expect_true(nrow(data) > 0)
  expect_true(all(c("ID", "DATE", "TMAX", "TMIN") %in% names(data)))
  expect_true(all(data$ID == "USW00094728"))
  expect_true(all(data$DATE >= as.Date("2022-01-01") & data$DATE <= as.Date("2022-01-31")))
})

test_that("get_data_aws handles invalid inputs", {
  expect_error(get_data_aws("invalid_id", "2022-01-01", "2022-01-31", c("TMAX", "TMIN")))
  expect_error(get_data_aws("USW00094728", "invalid_date", "2022-01-31", c("TMAX", "TMIN")))
  expect_error(get_data_aws("USW00094728", "2022-01-01", "2022-01-31", "INVALID_VAR"))
})

test_that("get_data_aws returns long format when specified", {
  skip_if_offline()

  data_long <- get_data_aws("USW00094728", "2022-01-01", "2022-01-31", c("TMAX", "TMIN"), format = "long")

  expect_s3_class(data_long, "data.table")
  expect_true("ELEMENT" %in% names(data_long))
  expect_true(all(data_long$ELEMENT %in% c("TMAX", "TMIN")))
})
