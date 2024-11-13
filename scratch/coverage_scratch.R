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
#' Optimized function to calculate missingness rates for specified variables
#' using data.table operations and parallel processing.
#'
#' @param stations data.table Output from find_longterm_stations()
#' @param start_date Character. Start date in "YYYY-MM-DD" format
#' @param end_date Character. End date in "YYYY-MM-DD" format
#' @param variables Character vector. Variables to analyze
#' @param parallel Logical. Whether to use parallel processing
#' @param n_cores Integer. Number of cores to use if parallel=TRUE
#' @return data.table with missingness rates by station and variable
analyze_station_missingness <- function(stations,
                                        start_date = "1900-01-01",
                                        end_date = "2023-12-31",
                                        variables = c("PRCP", "TMAX", "TMIN"),
                                        parallel = TRUE,
                                        n_cores = min(parallel::detectCores() - 1, 8)) {
  library(data.table)

  # Calculate expected number of days once
  expected_days <- as.numeric(as.Date(end_date) - as.Date(start_date) + 1)

  # Function to process a single station
  process_station <- function(station_id) {
    tryCatch({
      # Get station data
      station_data <- ghcn::get_data_aws(
        station_id = station_id,
        start_date = start_date,
        end_date = end_date,
        variables = variables
      )

      if(nrow(station_data) == 0) {
        # Return all NA if no data
        return(data.table(
          id = station_id,
          variable = variables,
          total_days = expected_days,
          missing_days = expected_days,
          available_days = 0,
          missing_rate = 1,
          coverage_rate = 0
        ))
      }

      # Convert to data.table if not already
      setDT(station_data)

      # Calculate missingness for all variables at once
      stats <- lapply(variables, function(var) {
        if(var %in% names(station_data)) {
          missing_count <- sum(is.na(station_data[[var]]))
          data.table(
            id = station_id,
            variable = var,
            total_days = expected_days,
            missing_days = missing_count,
            available_days = expected_days - missing_count,
            missing_rate = missing_count / expected_days,
            coverage_rate = 1 - (missing_count / expected_days)
          )
        } else {
          data.table(
            id = station_id,
            variable = var,
            total_days = expected_days,
            missing_days = expected_days,
            available_days = 0,
            missing_rate = 1,
            coverage_rate = 0
          )
        }
      })

      rbindlist(stats)

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
    clusterExport(cl, c("expected_days", "start_date", "end_date", "variables"), environment())

    # Load required packages on each cluster
    clusterEvalQ(cl, {
      library(data.table)
      library(ghcn)
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

  # Order by station ID and variable
  setorder(final_results, id, variable)

  return(final_results)
}

#' Filter Stations by Coverage Rate Threshold
#'
#' Identifies stations that meet minimum coverage requirements for all specified variables
#'
#' @param missingness data.table Output from analyze_station_missingness()
#' @param min_coverage Numeric. Minimum coverage rate (0-1)
#' @param us_only Logical. Whether to return only US stations
#' @return data.table of stations meeting criteria
filter_by_coverage <- function(missingness, min_coverage = 0.9, us_only = FALSE) {
  # Get minimum coverage rate for each station across all variables
  station_min_coverage <- missingness[, .(
    min_coverage_rate = min(coverage_rate),
    mean_coverage_rate = mean(coverage_rate),
    worst_var = variable[which.min(coverage_rate)]
  ), by = .(id, name, state, latitude, longitude, elevation)]

  # Apply US filter if requested
  if(us_only) {
    station_min_coverage <- station_min_coverage[substr(id, 1, 2) == "US"]
  }

  # Filter stations meeting threshold
  qualifying_stations <- station_min_coverage[min_coverage_rate >= min_coverage]

  # Merge back with full variable information for qualifying stations
  result <- missingness[id %in% qualifying_stations$id]

  # Add the overall statistics
  result <- merge(result,
                  qualifying_stations[, .(id, min_coverage_rate, mean_coverage_rate, worst_var)],
                  by = "id")

  # Order by coverage rate and ID
  setorder(result, -min_coverage_rate, id)

  # Print summary
  cat(sprintf("Found %d stations meeting %.1f%% coverage threshold",
              uniqueN(result$id), min_coverage * 100))
  if(us_only) cat(" in the United States")
  cat("\n")

  # Print state distribution if US only
  if(us_only && nrow(result) > 0) {
    state_counts <- unique(result[, .(id, state)])[, .N, by = state]
    setorder(state_counts, -N)
    cat("\nStations per state:\n")
    print(state_counts)
  }

  return(result)
}

# Step 1: Find stations with long records
long_coverage_stations <- find_longterm_stations(
  min_year = 1950,
  max_year = 2023,
  required_elements = c("PRCP", "TMAX", "TMIN")
)

long_coverage_stations<-long_coverage_stations[state!=""]

# Step 2: Get actual coverage rates through analyze_station_missingness
missingness <- analyze_station_missingness(
  stations = long_coverage_stations,
  start_date = "1900-01-01",
  end_date = "2023-12-31",
  variables = c("PRCP", "TMAX", "TMIN"),
  parallel = TRUE,
  n_cores = 7
)

# Step 3: Now we can use filter_by_coverage
good_stations <- filter_by_coverage(
  missingness = missingness,
  min_coverage = 0.995,
  us_only = TRUE
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
