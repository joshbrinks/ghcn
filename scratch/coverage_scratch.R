#' Find GHCN Stations with Long-term Records
#'
#' This function identifies GHCN stations where EACH required variable meets
#' the specified time period criteria and returns their full temporal coverage.
#'
#' @param min_year Numeric. Minimum starting year for records
#' @param max_year Numeric. Maximum ending year for records
#' @param required_elements Character vector. Required weather elements
#' @return data.table with filtered stations and their temporal coverage
find_longterm_stations <- function(min_year = 1900,
                                   max_year = 2023,
                                   required_elements = c("TMAX", "TMIN", "PRCP")) {
  library(data.table)

  # Load the inventory data
  inventory <- as.data.table(ghcn::ghcn.inventory)
  stations <- as.data.table(ghcn::ghcn.stations)

  # Input validation
  if(max_year < min_year) {
    stop("max_year must be greater than min_year")
  }

  # First, filter for only records with our required elements
  filtered_inv <- inventory[element %in% required_elements]

  # A station qualifies if each element:
  # 1. Starts by or before min_year
  # 2. Continues through or after max_year
  qualified_elements <- filtered_inv[
    firstyear <= min_year &
      lastyear >= max_year,
    .(
      element,
      firstyear,
      lastyear
    ),
    by = id
  ]

  # Count how many of the required elements each station has
  element_counts <- qualified_elements[, .(element_count = .N), by = id]

  # Keep only stations that have ALL required elements meeting criteria
  final_stations <- element_counts[element_count == length(required_elements)]

  # Get the complete temporal range for all variables for qualified stations
  # (including years outside our filter requirements)
  full_temporal_range <- inventory[
    id %in% final_stations$id &
      element %in% required_elements
  ]

  # Create coverage ranges for each element
  temporal_summary <- full_temporal_range[,
                                          .(coverage = paste(firstyear, lastyear, sep="-")),
                                          by = .(id, element)
  ]

  # Reshape to wide format with meaningful column names
  temporal_summary_wide <- dcast(
    temporal_summary,
    id ~ element,
    value.var = "coverage"
  )

  # Rename columns to add _COV suffix
  setnames(
    temporal_summary_wide,
    old = required_elements,
    new = paste0(required_elements, "_COV")
  )

  # Add station metadata
  result <- merge(
    stations[, .(id, name, state, latitude, longitude, elevation)],
    temporal_summary_wide,
    by = "id"
  )

  # Order by station ID
  setorder(result, id)

  return(result)
}

#' Create Interactive Map of GHCN Stations
#'
#' Creates a leaflet map showing station locations with popup information
#' about station details and temporal coverage.
#'
#' @param stations data.table. Output from find_longterm_stations()
#' @return leaflet map object
map_ghcn_stations <- function(stations) {
  library(leaflet)

  # Create popup content
  popup_content <- paste0(
    "<b>", stations$name, "</b>",
    ifelse(!is.na(stations$state), paste0(" (", stations$state, ")"), ""),
    "<br>",
    "<b>ID:</b> ", stations$id,
    "<br>",
    "<b>Location:</b> ", round(stations$latitude, 2), "°, ", round(stations$longitude, 2), "°",
    "<br>",
    "<b>Elevation:</b> ", round(stations$elevation, 1), " m",
    "<br><br>",
    "<b>Coverage Periods:</b><br>",
    "PRCP: ", stations$PRCP_COV, "<br>",
    "TMAX: ", stations$TMAX_COV, "<br>",
    "TMIN: ", stations$TMIN_COV
  )

  # Create the map
  map <- leaflet(stations) %>%
    # Add base tiles
    addProviderTiles(providers$CartoDB.Positron) %>%

    # Add points
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = 6,
      color = "steelblue",
      stroke = FALSE,
      fillOpacity = 0.7,
      popup = popup_content,

      # Add hover labels
      label = ~name,
      labelOptions = labelOptions(
        noHide = FALSE,
        direction = "auto",
        textOnly = TRUE,
        style = list(
          "font-weight" = "bold",
          padding = "3px 8px"
        )
      )
    ) %>%

    # Set default view to show all points
    fitBounds(
      lng1 = min(stations$longitude),
      lat1 = min(stations$latitude),
      lng2 = max(stations$longitude),
      lat2 = max(stations$latitude)
    )

  return(map)
}

#' Analyze Missingness Rates for GHCN Stations
#'
#' Calculates missingness rates for specified time periods and variables.
#'
#' @param stations data.table Output from find_longterm_stations()
#' @param periods List of lists containing time periods to analyze
#' @param variables Character vector. Variables to analyze
#' @param parallel Logical. Whether to use parallel processing
#' @param n_cores Integer. Number of cores to use if parallel=TRUE
#' @return data.table with missingness rates by station, variable, and period
analyze_station_missingness <- function(stations,
                                        periods = list(
                                          p1900 = list(start = "1900-01-01", end = "2023-12-31", label = "1900-2023"),
                                          p1925 = list(start = "1925-01-01", end = "2023-12-31", label = "1925-2023"),
                                          p1950 = list(start = "1950-01-01", end = "2023-12-31", label = "1950-2023")
                                        ),
                                        variables = c("PRCP", "TMAX", "TMIN"),
                                        parallel = TRUE,
                                        n_cores = min(parallel::detectCores() - 1, 8)) {
  library(data.table)
  library(lubridate)

  # Function to process a single station for all periods
  process_station <- function(station_id) {
    tryCatch({
      # Get the full date range of data
      station_data <- ghcn::get_data_aws(
        station_id = station_id,
        start_date = periods[[1]]$start,  # Use earliest start date
        end_date = periods[[length(periods)]]$end,  # Use latest end date
        variables = variables
      )

      if(nrow(station_data) == 0) {
        # Return all NA if no data
        return(NULL)
      }

      # Convert to data.table if not already
      setDT(station_data)

      # Calculate coverage for each period
      period_results <- lapply(periods, function(period) {
        # Calculate exact number of days in period
        start_dt <- ymd(period$start)
        end_dt <- ymd(period$end)
        expected_days <- as.numeric(interval(start_dt, end_dt) / days(1)) + 1

        # Filter data for this period
        period_data <- station_data[DATE >= start_dt & DATE <= end_dt]

        # Calculate coverage for each variable
        var_stats <- lapply(variables, function(var) {
          if(var %in% names(period_data)) {
            # Count valid numeric values
            valid_count <- sum(!is.na(period_data[[var]]) &
                                 period_data[[var]] != "" &
                                 !is.null(period_data[[var]]) &
                                 is.finite(period_data[[var]]))

            data.table(
              id = station_id,
              variable = var,
              period = period$label,
              total_days = expected_days,
              missing_days = expected_days - valid_count,
              available_days = valid_count,
              coverage_rate = valid_count / expected_days
            )
          } else {
            data.table(
              id = station_id,
              variable = var,
              period = period$label,
              total_days = expected_days,
              missing_days = expected_days,
              available_days = 0,
              coverage_rate = 0
            )
          }
        })

        rbindlist(var_stats)
      })

      rbindlist(period_results)

    }, error = function(e) {
      warning(sprintf("Error processing station %s: %s", station_id, e$message))
      return(NULL)
    })
  }

  # Process stations either in parallel or sequentially
  if(parallel) {
    library(parallel)
    library(pbapply)

    cl <- makeCluster(n_cores)
    on.exit(stopCluster(cl))

    # Export necessary objects to cluster
    clusterExport(cl, c("periods", "variables"), environment())

    # Load required packages on each cluster
    clusterEvalQ(cl, {
      library(data.table)
      library(ghcn)
      library(lubridate)
    })

    # Process stations with progress bar
    results_list <- pblapply(stations$id, process_station, cl = cl)

  } else {
    # Sequential processing with progress bar
    results_list <- pbapply::pblapply(stations$id, process_station)
  }

  # Combine results
  results <- rbindlist(results_list[!sapply(results_list, is.null)])

  # Add station metadata
  final_results <- merge(
    results,
    stations[, .(id, name, state, latitude, longitude, elevation)],
    by = "id"
  )

  # Order by station ID, period, and variable
  setorder(final_results, id, period, variable)

  return(final_results)
}

#' Filter Stations by Coverage Rate Threshold Across Time Periods
#'
#' @param missingness data.table Output from analyze_station_missingness
#' @param threshold numeric Minimum coverage rate (0-1)
#' @param us_only logical Whether to return only US stations
#' @return data.table with qualified stations and their temporal coverage status
filter_by_coverage <- function(missingness, threshold = 0.98, us_only = FALSE) {
  library(data.table)

  # Get minimum coverage rate for each station-period combination across variables
  period_coverage <- missingness[, .(
    min_coverage = min(coverage_rate),
    n_vars = uniqueN(variable)
  ), by = .(id, name, state, latitude, longitude, elevation, period)]

  # Identify periods where station meets threshold for all variables
  qualified_periods <- period_coverage[
    min_coverage >= threshold & n_vars == 3  # Assuming we want all three variables
  ]

  # Create indicators for which periods each station qualifies for
  coverage_indicators <- dcast(
    qualified_periods,
    id + name + state + latitude + longitude + elevation ~ period,
    value.var = "min_coverage",
    fill = 0
  )

  # Create column names for periods
  period_cols <- grep("^[0-9]", names(coverage_indicators), value = TRUE)

  # Add a column indicating earliest period of qualification
  coverage_indicators[, earliest_period := fcase(
    get(period_cols[1]) >= threshold, period_cols[1],
    get(period_cols[2]) >= threshold, period_cols[2],
    get(period_cols[3]) >= threshold, period_cols[3],
    default = NA_character_
  )]

  # Filter for US stations if requested
  if(us_only) {
    coverage_indicators <- coverage_indicators[
      !is.na(state) | substr(id, 1, 2) == "US"
    ]
  }

  # Order by earliest period and then state
  coverage_indicators[, period_order := fcase(
    earliest_period == period_cols[1], 1,
    earliest_period == period_cols[2], 2,
    earliest_period == period_cols[3], 3,
    default = 4
  )]

  setorder(coverage_indicators, period_order, state)
  coverage_indicators[, period_order := NULL]

  # Format coverage percentages
  for(col in period_cols) {
    coverage_indicators[, (col) := round(get(col) * 100, 2)]
  }

  return(coverage_indicators)
}

# Step 1: Find stations with long records
long_coverage_stations <- find_longterm_stations(
  min_year = 1950,
  max_year = 2023,
  required_elements = c("PRCP", "TMAX", "TMIN")
)

# get US stations
long_coverage_stations<-long_coverage_stations[substr(id, 1, 2)=="US"]

# Define time periods
periods <- list(
  p1900 = list(start = "1900-01-01", end = "2023-12-31", label = "1900-2023"),
  p1925 = list(start = "1925-01-01", end = "2023-12-31", label = "1925-2023"),
  p1950 = list(start = "1950-01-01", end = "2023-12-31", label = "1950-2023")
)

# Analyze missingness for all periods
missingness <- analyze_station_missingness(
  stations = long_coverage_stations,
  periods = periods,
  variables = c("PRCP", "TMAX", "TMIN"),
  parallel = TRUE
)

# Step 3: Now we can use filter_by_coverage
qualified_stations <- filter_by_coverage(
  missingness = missingness,
  threshold = 0.98,
  us_only = FALSE
)
# Step 4: Create map of qualifying stations
map_ghcn_stations(good_stations)

# Step 6: Analyze Q_FLAGS for a single station as an example
# Choose the first station from our good stations
example_station <- good_stations[1, id]

station_data <- ghcn::get_data_aws(
  station_id = example_station,
  start_date = "1900-01-01",
  end_date = "2023-12-31",
  variables = c("PRCP", "TMAX", "TMIN")
)

qflag_summary <- analyze_qflags(station_data)

# Print summaries at each step
cat("\nInitial station identification:")
print(head(long_coverage_stations))

cat("\nUS stations summary:")
print(head(us_stations))

cat("\nMissingness analysis summary:")
print(head(missingness))

cat("\nStations meeting coverage criteria:")
print(head(good_stations))

cat("\nQuality flag analysis for example station:", example_station)
print(qflag_summary)
