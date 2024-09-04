#' Get Nearest NOAA GHCN-Daily Weather Stations with Available Variables
#'
#' This function retrieves the nearest weather stations from the NOAA GHCN-Daily dataset
#' given a latitude and longitude coordinate. It returns information about each station,
#' including its location, elevation, the date range of available data, and available variables.
#'
#' @param lat Numeric. Latitude of the point of interest.
#' @param lon Numeric. Longitude of the point of interest.
#' @param token Character. NOAA API token for authentication.
#' @param radius Numeric. Search radius in kilometers. Default is 50 km.
#' @param limit Integer. Maximum number of stations to return. Default is 5.
#'
#' @return A data frame containing information about the nearest weather stations, including:
#'   \item{id}{Character. The station identifier.}
#'   \item{name}{Character. The name of the station.}
#'   \item{latitude}{Numeric. The station's latitude.}
#'   \item{longitude}{Numeric. The station's longitude.}
#'   \item{elevation}{Numeric. The station's elevation.}
#'   \item{mindate}{Date. The earliest date for which the station has data.}
#'   \item{maxdate}{Date. The latest date for which the station has data.}
#'   \item{distance}{Numeric. The calculated distance from the input coordinates in degrees.}
#'   \item{available_vars}{Character. Comma-separated list of available variables.}
#'
#'
#' @examples
#' \dontrun{
#' stations <- get_ghcn_daily_stations(lat = 40.7128, lon = -74.006, token = "your_noaa_token_here")
#' print(stations)
#' }
#'
#' @export
get_ghcn_daily_stations <- function(lat, lon, token, radius = 50, limit = 5) {
  # Base URL for the NOAA API
  base_url <- "https://www.ncdc.noaa.gov/cdo-web/api/v2"

  # Construct the query parameters for stations
  params <- list(
    datasetid = "GHCND", # GHCN-Daily dataset
    extent = paste(lat - radius/111, lon - radius/88.8, lat + radius/111, lon + radius/88.8, sep = ","),
    limit = limit
  )

  # Check if token is provided
  if (missing(token) || is.null(token) || token == "") {
    stop("Error: NOAA API token is required")
  }

  # Make the API request for stations
  tryCatch({
    response <- httr::GET(
      url = paste0(base_url, "/stations"),
      query = params,
      httr::add_headers("token" = token)
    )

    # Check if the request was successful
    if (httr::status_code(response) == 200) {
      # Parse the JSON response
      data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

      # Check if any stations were returned
      if (length(data$results) == 0) {
        warning("No stations found within the specified radius")
        return(NULL)
      }

      # Extract relevant information
      stations <- data$results[, c("id", "name", "latitude", "longitude", "elevation", "mindate", "maxdate")]

      # Calculate distance from the given coordinates
      stations$distance <- sqrt((stations$latitude - lat)^2 + (stations$longitude - lon)^2)

      # Sort by distance
      stations <- stations[order(stations$distance), ]

      # Convert mindate and maxdate to Date objects
      stations$mindate <- as.Date(stations$mindate)
      stations$maxdate <- as.Date(stations$maxdate)

      # Initialize available_vars column
      stations$available_vars <- ""

      # For each station, get available datatypes
      for (i in 1:nrow(stations)) {
        datatype_params <- list(
          datasetid = "GHCND",
          stationid = stations$id[i],
          limit = 1000
        )

        datatype_response <- httr::GET(
          url = paste0(base_url, "/datatypes"),
          query = datatype_params,
          httr::add_headers("token" = token)
        )

        if (httr::status_code(datatype_response) == 200) {
          datatype_data <- jsonlite::fromJSON(httr::content(datatype_response, "text", encoding = "UTF-8"))
          if (length(datatype_data$results) > 0) {
            stations$available_vars[i] <- paste(datatype_data$results$id, collapse = ", ")
          }
        } else {
          warning(paste("Could not retrieve datatypes for station", stations$id[i]))
        }
      }

      return(stations)
    } else {
      stop(paste("Error: API request failed with status code", httr::status_code(response),
                 "\nResponse content:", httr::content(response, "text", encoding = "UTF-8")))
    }
  }, error = function(e) {
    stop(paste("Error in API request:", e$message))
  })
}

#' Get NOAA GHCN-Daily Data for a Specific Station
#'
#' This function retrieves time series data from the NOAA GHCN-Daily dataset
#' for a specific weather station within a given date range.
#'
#' @param station_id Character. The ID of the weather station.
#' @param start_date Date or character string in "YYYY-MM-DD" format. The start date for data retrieval.
#' @param end_date Date or character string in "YYYY-MM-DD" format. The end date for data retrieval.
#' @param token Character. NOAA API token for authentication.
#' @param datatype Character vector. Types of data to retrieve (e.g., "TMAX", "TMIN", "PRCP"). Default is all available types.
#'
#' @return A data frame containing the time series data for the specified station and date range, including:
#'   \item{date}{Date. The date of the observation.}
#'   \item{datatype}{Character. The type of data (e.g., "TMAX", "TMIN", "PRCP").}
#'   \item{value}{Numeric. The recorded value for the datatype.}
#'   \item{attributes}{Character. Any attributes associated with the value.}
#'
#'
#' @examples
#' \dontrun{
#' data <- get_ghcn_daily_data(
#'   station_id = "USW00094728",  # Example station ID
#'   start_date = "2022-01-01",
#'   end_date = "2022-12-31",
#'   token = "your_noaa_token_here"
#' )
#' print(head(data))
#' }
#'
#' @export
get_ghcn_daily_data <- function(station_id, start_date, end_date, token, datatype = NULL) {
  # Base URL for the NOAA API
  base_url <- "https://www.ncdc.noaa.gov/cdo-web/api/v2/data"

  # Ensure dates are in the correct format
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Construct the query parameters
  params <- list(
    datasetid = "GHCND",
    stationid = station_id,
    startdate = format(start_date, "%Y-%m-%d"),
    enddate = format(end_date, "%Y-%m-%d"),
    limit = 1000
  )

  if (!is.null(datatype)) {
    params$datatypeid <- paste(datatype, collapse = ",")
  }

  # Check if token is provided
  if (missing(token) || is.null(token) || token == "") {
    stop("Error: NOAA API token is required")
  }

  # Initialize an empty data frame to store results
  all_data <- data.frame()

  # Make API requests and aggregate data
  tryCatch({
    repeat {
      response <- httr::GET(
        url = base_url,
        query = params,
        httr::add_headers("token" = token)
      )

      # Check if the request was successful
      if (httr::status_code(response) == 200) {
        # Parse the JSON response
        data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

        # If no data is returned, break the loop
        if (length(data$results) == 0) {
          break
        }

        # Append the new data to the existing data
        all_data <- rbind(all_data, data$results)

        # If there's no more data to fetch, break the loop
        if (is.null(data$metadata$resultset$offset) ||
            data$metadata$resultset$offset + data$metadata$resultset$limit >= data$metadata$resultset$count) {
          break
        }

        # Update the offset for the next request
        params$offset <- data$metadata$resultset$offset + data$metadata$resultset$limit
      } else {
        stop(paste("Error: API request failed with status code", httr::status_code(response),
                   "\nResponse content:", httr::content(response, "text", encoding = "UTF-8")))
      }
    }

    # If no data was retrieved, return NULL
    if (nrow(all_data) == 0) {
      warning("No data found for the specified station and date range")
      return(NULL)
    }

    # Convert date to Date object and value to numeric
    all_data$date <- as.Date(all_data$date)
    all_data$value <- as.numeric(all_data$value)

    return(all_data)
  }, error = function(e) {
    stop(paste("Error in API request:", e$message))
  })
}

#' Visualize NOAA GHCN-Daily Data with Facets
#'
#' This function creates a faceted time series plot for all variables
#' retrieved from the NOAA GHCN-Daily dataset, using comprehensive
#' datatype information from ghcn::ghcn_daily_datatypes and a custom color palette.
#'
#' @param data A data frame as returned by get_ghcn_daily_data function.
#' @param station_name Character. The name of the station (for plot title).
#'
#' @return A ggplot object with faceted plots for each variable in a single row.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' data <- get_ghcn_daily_data(
#'   station_id = "USW00094728",
#'   start_date = "2022-01-01",
#'   end_date = "2022-12-31",
#'   token = "your_noaa_token_here"
#' )
#' plot <- visualize_ghcn_daily_data(data, "Central Park, NY")
#' print(plot)
#' }
#'
#' @export
visualize_ghcn_daily_data <- function(data, station_name) {
  # Input validation
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("Input 'data' must be a non-empty data frame.")
  }
  if (!all(c("date", "datatype", "value") %in% names(data))) {
    stop("Input 'data' must contain columns: 'date', 'datatype', and 'value'.")
  }
  if (!is.character(station_name) || length(station_name) != 1) {
    stop("'station_name' must be a single character string.")
  }

  # Ensure data is in the correct format
  data$date <- as.Date(data$date)
  data$value <- as.numeric(data$value)

  # Handle infinite values
  data$value[is.infinite(data$value)] <- NA

  # Create a lookup table for datatype information
  datatype_info <- ghcn::ghcn_daily_datatypes |>
    dplyr::mutate(
      unit = dplyr::case_when(
        grepl("temperature", description, ignore.case = TRUE) ~ "°C",
        grepl("precipitation", description, ignore.case = TRUE) ~ "mm",
        grepl("snowfall", description, ignore.case = TRUE) ~ "mm",
        grepl("snow depth", description, ignore.case = TRUE) ~ "mm",
        grepl("wind", description, ignore.case = TRUE) ~ "m/s",
        TRUE ~ ""
      ),
      scale_factor = dplyr::case_when(
        grepl("tenths", description, ignore.case = TRUE) ~ 10,
        TRUE ~ 1
      ),
      # Capitalize first letter of each word in description
      description = gsub("(^|\\s)(\\S)", "\\1\\U\\2", description, perl=TRUE),
      # Remove unit information from description if it's already there
      description = gsub("\\s*\\([^\\)]+\\)$", "", description)
    )

  # Join datatype information with the data
  data <- data |>
    dplyr::left_join(datatype_info, by = "datatype") |>
    dplyr::mutate(
      value = value / scale_factor,
      label = ifelse(unit != "", paste0(description, " (", unit, ")"), description)
    )

  # Create a custom color palette
  custom_palette <- c("#3398cb", "#ed5535", "#9C27B0", "#009688", "#673AB7", "#4CAF50")

  # Ensure we have enough colors for all facets
  n_facets <- length(unique(data$label))
  if (n_facets > length(custom_palette)) {
    custom_palette <- colorRampPalette(custom_palette)(n_facets)
  }

  # Create the faceted plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = date, y = value, color = label)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_wrap(~ label, scales = "free_y", nrow = 1, labeller = ggplot2::label_wrap_gen(width = 25)) +
    ggplot2::labs(title = paste("Weather Data for", station_name)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 8),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none"  # Remove the legend
    ) +
    ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    ggplot2::scale_color_manual(values = custom_palette)

  # Adjust y-axis for each datatype
  p <- p + ggplot2::scale_y_continuous(
    limits = function(x) {
      datatype <- data$datatype[data$value %in% x][1]
      if (is.na(datatype)) return(c(0, 1))  # Default range if no matching datatype

      values <- data$value[data$datatype == datatype]
      if (all(is.na(values))) return(c(0, 1))  # Default range if all values are NA

      if (datatype %in% c("PRCP", "SNOW", "SNWD") || data$unit[data$datatype == datatype][1] %in% c("mm", "m/s")) {
        c(0, max(values, na.rm = TRUE))
      } else {
        ggplot2::expand_range(range(values, na.rm = TRUE), mult = 0.05)
      }
    }
  )

  return(p)
}
