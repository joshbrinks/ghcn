library(data.table)
library(usethis)

# Function to download and read a file from a URL using fread
ghcn.download_and_read <- function(url) {
  data <- data.table::fread(url, header = FALSE, sep = "\n", colClasses = "character")
  return(data)
}

# Download and parse stations data
stations_url <- "http://noaa-ghcn-pds.s3.amazonaws.com/ghcnd-stations.txt"
stations_col_names <- c("id", "latitude", "longitude", "elevation", "state", "name", "gsn_flag", "hcn_crn_flag", "wmo_id")

ghcn.stations <- ghcn.download_and_read(stations_url)
ghcn.stations <- ghcn.stations[, .(
  id = substr(V1, 1, 11),
  latitude = as.numeric(substr(V1, 13, 20)),
  longitude = as.numeric(substr(V1, 22, 30)),
  elevation = as.numeric(substr(V1, 32, 37)),
  state = substr(V1, 39, 40),
  name = substr(V1, 42, 71),
  gsn_flag = substr(V1, 73, 75),
  hcn_crn_flag = substr(V1, 77, 79),
  wmo_id = substr(V1, 81, 85)
)]

# Trim whitespace from character columns
char_cols <- names(ghcn.stations)[sapply(ghcn.stations, is.character)]
ghcn.stations[, (char_cols) := lapply(.SD, trimws), .SDcols = char_cols]

# Download and parse inventory data
inventory_url <- "http://noaa-ghcn-pds.s3.amazonaws.com/ghcnd-inventory.txt"
inventory_col_names <- c("id", "latitude", "longitude", "element", "firstyear", "lastyear")

ghcn.inventory <- ghcn.download_and_read(inventory_url)
ghcn.inventory <- ghcn.inventory[, .(
  id = substr(V1, 1, 11),
  latitude = as.numeric(substr(V1, 13, 20)),
  longitude = as.numeric(substr(V1, 22, 30)),
  element = substr(V1, 32, 35),
  firstyear = as.integer(substr(V1, 37, 40)),
  lastyear = as.integer(substr(V1, 42, 45))
)]

# Save the data in the package
usethis::use_data(ghcn.stations, overwrite = TRUE)
usethis::use_data(ghcn.inventory, overwrite = TRUE)

# Print summary information
cat("GHCN-D Stations Summary:\n")
cat("Number of stations:", nrow(ghcn.stations), "\n")
cat("Date range:", min(ghcn.inventory$firstyear), "to", max(ghcn.inventory$lastyear), "\n")
cat("Number of unique elements:", data.table::uniqueN(ghcn.inventory$element), "\n")

# List the top 10 most common elements
top_elements <- ghcn.inventory[, .N, by = element][order(-N)][1:10]

cat("\nTop 10 most common elements:\n")
print(top_elements)
