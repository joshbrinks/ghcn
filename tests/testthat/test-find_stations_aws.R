test_that("find_stations_aws returns expected output", {
  skip_if_offline()

  stations <- find_stations_aws(lat = 40.7128, lon = -74.0060, radius = 50)

  expect_s3_class(stations, "data.table")
  expect_true(nrow(stations) > 0)
  expect_true(all(c("id", "latitude", "longitude", "elevation", "state", "name") %in% names(stations)))
  expect_true(all(stations$latitude >= 40.7128 - 1 & stations$latitude <= 40.7128 + 1))
  expect_true(all(stations$longitude >= -74.0060 - 1 & stations$longitude <= -74.0060 + 1))
})

test_that("find_stations_aws handles invalid inputs", {
  expect_error(find_stations_aws(lat = "invalid", lon = -74.0060, radius = 50), "lat, lon, and radius must be numeric values")
  expect_error(find_stations_aws(lat = 40.7128, lon = -74.0060, radius = "invalid"), "lat, lon, and radius must be numeric values")
  expect_error(find_stations_aws(lat = 100, lon = -74.0060, radius = 50), "Latitude must be between -90 and 90 degrees")
  expect_error(find_stations_aws(lat = 40.7128, lon = -200, radius = 50), "Longitude must be between -180 and 180 degrees")
  expect_error(find_stations_aws(lat = 40.7128, lon = -74.0060, radius = -10), "Radius must be a positive number")
})
