#' Find GHCN-D Stations from AWS Data with Fuzzy Matching
#'
#' This function finds GHCN-D weather stations within a specified radius of a given location
#' and/or matching a given name pattern using fuzzy matching, using the station data stored
#' in the AWS-based GHCN package.
#'
#' @param lat Numeric. Latitude of the center point in decimal degrees.
#' @param lon Numeric. Longitude of the center point in decimal degrees.
#' @param radius Numeric. Search radius in kilometers.
#' @param name_pattern Character. Pattern to search for in station names. Default is NULL.
#' @param max_distance Numeric. Maximum string distance for fuzzy matching. Default is 0.2.
#' @param max_stations Integer. Maximum number of stations to return. Default is 10.
#'
#' @return A data.table containing information about nearby and/or name-matched stations, sorted by relevance and distance.
#'
#' @examples
#' find_stations_aws(40.7128, -74.0060, 50)  # Nearby stations to New York City
#' find_stations_aws(40.7128, -74.0060, 50, name_pattern = "Central Prk")
#' find_stations_aws(lat = NULL, lon = NULL, radius = NULL, name_pattern = "JFK Airprt")
#'
#' @export
find_stations_aws <- function(lat = NULL, lon = NULL, radius = NULL, name_pattern = NULL, max_distance = 0.5, max_stations = 10) {
  # Input validation
  if (!is.numeric(lat) || !is.numeric(lon) || !is.numeric(radius)) {
    stop("lat, lon, and radius must be numeric values")
  }

  if (abs(lat) > 90) {
    stop("Latitude must be between -90 and 90 degrees")
  }

  if (abs(lon) > 180) {
    stop("Longitude must be between -180 and 180 degrees")
  }

  if (radius <= 0) {
    stop("Radius must be a positive number")
  }
  # Load the stations data
  stations <- data.table::as.data.table(ghcn::ghcn.stations)

  # Check if at least one of lat/lon/radius or name_pattern is provided
  if (is.null(lat) && is.null(lon) && is.null(radius) && is.null(name_pattern)) {
    stop("Please provide either location information (lat, lon, radius) or a name pattern to search.")
  }

  # If name pattern is provided, perform fuzzy matching
  if (!is.null(name_pattern)) {
    # Calculate string distances
    stations[, name_distance := stringdist::stringdist(tolower(name), tolower(name_pattern), method = "lv") /
               pmax(nchar(name), nchar(name_pattern))]

    # Filter stations based on maximum allowed distance
    stations <- stations[name_distance <= max_distance]

    if (nrow(stations) == 0) {
      stop("No stations found matching the provided name pattern within the specified distance.")
    }
  }

  # If location is provided, filter by distance
  if (!is.null(lat) && !is.null(lon) && !is.null(radius)) {
    # Convert degrees to radians
    deg2rad <- function(deg) deg * (pi / 180)

    # Haversine formula
    haversine <- function(lat1, lon1, lat2, lon2) {
      R <- 6371  # Earth's radius in km

      dlat <- deg2rad(lat2 - lat1)
      dlon <- deg2rad(lon2 - lon1)

      a <- sin(dlat/2)^2 + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * sin(dlon/2)^2
      c <- 2 * atan2(sqrt(a), sqrt(1-a))

      R * c
    }

    # Calculate distances
    stations[, geo_distance := haversine(lat, lon, latitude, longitude)]

    # Filter stations within radius
    stations <- stations[geo_distance <= radius]
  }

  # Sort results
  if (!is.null(name_pattern) && (!is.null(lat) && !is.null(lon) && !is.null(radius))) {
    data.table::setorder(stations, name_distance, geo_distance)
  } else if (!is.null(name_pattern)) {
    data.table::setorder(stations, name_distance)
  } else {
    data.table::setorder(stations, geo_distance)
  }

  # Limit to max_stations
  result <- stations[1:min(max_stations, .N)]

  return(result)
}

#' Get GHCN-D Station Inventory Information for Multiple Stations
#'
#' This function retrieves inventory information for specified GHCN-D station(s),
#' including available variables, their coverage periods, and other relevant details.
#'
#' @param station_ids Character vector. One or more 11-character GHCN station identification codes.
#'
#' @return A data.table containing inventory information for the specified stations.
#'
#' @examples
#' # Get inventory for two stations
#' get_inventory_aws(c("USW00094728", "USC00305816"))
#'
#' @export
get_inventory_aws <- function(station_ids) {
  # Validate input
  if (!all(nchar(station_ids) == 11)) {
    stop("Invalid station ID(s). All must be 11 characters.")
  }

  # Load the inventory data
  inventory <- data.table::as.data.table(ghcn::ghcn.inventory)

  # Filter inventory for the specified stations
  station_inventory <- inventory[id %in% station_ids]

  if (nrow(station_inventory) == 0) {
    stop("No inventory information found for the specified station ID(s).")
  }

  # Calculate the coverage (in years) for each element
  station_inventory[, coverage_years := lastyear - firstyear + 1]

  # Calculate the completeness (assuming data for all years in the range)
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  station_inventory[, completeness := coverage_years / (current_year - firstyear + 1)]

  # Add descriptions for common elements
  element_descriptions <- ghcn::ghcn_daily_datatypes
  station_inventory <- merge(station_inventory, element_descriptions,
                             by.x = "element", by.y = "datatype", all.x = TRUE, sort = FALSE)

  # For elements without descriptions, set a default message
  station_inventory[is.na(description), description := "Description not available"]

  # Order by station ID and element
  data.table::setorder(station_inventory, id, element)

  # Select and rename columns
  result <- station_inventory[, .(
    station_id = id,
    element,
    description,
    first_year = firstyear,
    last_year = lastyear,
    coverage_years,
    completeness
  )]

  return(result)
}

#' Parse Flexible Date Format
#'
#' @param date_str Character. Date string in format YYYY, YYYY-MM, or YYYY-MM-DD.
#' @param end_of_period Logical. If TRUE, return the last day of the period for YYYY or YYYY-MM formats.
#' @return A Date object.
#'
parse_flexible_date <- function(date_str, end_of_period = FALSE) {
  if (inherits(date_str, "Date")) return(date_str)

  if (grepl("^\\d{4}$", date_str)) {
    # YYYY format
    if (end_of_period) {
      return(lubridate::ymd(paste0(date_str, "-12-31")))
    } else {
      return(lubridate::ymd(paste0(date_str, "-01-01")))
    }
  } else if (grepl("^\\d{4}-\\d{2}$", date_str)) {
    # YYYY-MM format
    if (end_of_period) {
      date <- lubridate::ymd(paste0(date_str, "-01"))
      return(date + months(1) - lubridate::days(1))
    } else {
      return(lubridate::ymd(paste0(date_str, "-01")))
    }
  } else if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date_str)) {
    # YYYY-MM-DD format
    return(lubridate::ymd(date_str))
  } else {
    stop("Invalid date format. Use YYYY, YYYY-MM, or YYYY-MM-DD.")
  }
}

#' Retrieve GHCN-D Data for Specific Stations
#'
#' This function retrieves GHCN-D data for specified station(s), time period, and variable(s) directly from the NOAA GHCN-D S3 bucket.
#'
#' @param station_id Character vector. One or more 11-character GHCN station identification code(s).
#' @param start_date Character. Start date in format YYYY, YYYY-MM, or YYYY-MM-DD.
#' @param end_date Character. End date in format YYYY, YYYY-MM, or YYYY-MM-DD.
#' @param variables Character vector. GHCN-D element code(s) to retrieve (e.g., c("TMAX", "TMIN", "PRCP")).
#' @param format Character. Output format, either "wide" (default) or "long".
#'
#' @return A data.table containing the requested GHCN-D data.
#'
#' @examples
#' # Retrieve temperature data for a station from January 2020 to January 2023
#' nyc_temp_data <- get_data_aws("USC00305816", "2020-01", "2023-01", c("TMAX", "TMIN"))
#'
#' @export
get_data_aws <- function(station_id, start_date, end_date, variables, format = "wide") {
  # Validate inputs
  if (!all(nchar(station_id) == 11)) stop("Invalid station ID(s). Must be 11 characters.")
  if (!format %in% c("wide", "long")) stop("Invalid format. Must be 'wide' or 'long'.")

  # Parse dates
  start_date <- ghcn:::parse_flexible_date(start_date)
  end_date <- ghcn:::parse_flexible_date(end_date, end_of_period = TRUE)

  if (start_date > end_date) stop("Start date must be before or equal to end date.")

  # Initialize result data.table
  result <- data.table::data.table()

  # Retrieve data for each station
  for (id in station_id) {
    url <- paste0("https://noaa-ghcn-pds.s3.amazonaws.com/csv/by_station/", id, ".csv")

    tryCatch({
      station_data <- data.table::fread(url)

      # Convert DATE to Date type
      station_data[, DATE := lubridate::ymd(DATE)]

      # Filter data
      station_data <- station_data[DATE >= start_date &
                                     DATE <= end_date &
                                     ELEMENT %in% variables]

      result <- data.table::rbindlist(list(result, station_data), use.names = TRUE, fill = TRUE)
    }, error = function(e) {
      warning(paste("Error retrieving data for station", id, ":", e$message))
    })
  }

  if (nrow(result) == 0) {
    stop("No data found for the specified parameters.")
  }

  # Reshape data if wide format is requested
  if (format == "wide") {
    result <- data.table::dcast(result, ID + DATE ~ ELEMENT, value.var = "DATA_VALUE")
  }

  # Sort the result
  data.table::setorder(result, ID, DATE)

  return(result)
}
