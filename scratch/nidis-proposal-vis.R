#' Create comprehensive station summary with DEWS regions
#'
#' @param missingness data.table Output from analyze_station_missingness
#' @param dews sf object NIDIS DEWS boundaries
#' @param threshold numeric Coverage threshold for all variables
#' @return data.table with station details and DEWS assignment
create_station_dews_summary <- function(missingness, dews, threshold = 0.995) {
  library(data.table)
  library(sf)

  # Get unique stations with coordinates
  stations_unique <- unique(missingness[, .(
    id, name, state, latitude, longitude, elevation
  )])

  # Convert to sf object for spatial join
  stations_sf <- st_as_sf(
    stations_unique,
    coords = c("longitude", "latitude"),
    crs = 4326
  )

  # Spatial join with DEWS regions and maintain coordinates
  stations_dews <- st_join(stations_sf, dews, left = TRUE)
  coords <- st_coordinates(stations_dews)
  stations_dt <- as.data.table(cbind(
    st_drop_geometry(stations_dews),
    longitude = coords[,1],
    latitude = coords[,2]
  ))

  # Replace NA in DEWS with "Not Covered by DEW"
  stations_dt[is.na(DEWS), DEWS := "Not Covered by DEW"]

  # Get coverage rates for each variable
  coverage_summary <- missingness[, .(
    coverage_rate = min(coverage_rate)
  ), by = .(id, variable)]

  # Reshape to wide format
  coverage_wide <- dcast(coverage_summary, id ~ variable,
                         value.var = "coverage_rate")

  # Combine with station info and filter for threshold
  final_summary <- merge(stations_dt, coverage_wide, by = "id")
  final_summary <- final_summary[
    PRCP >= threshold & TMAX >= threshold & TMIN >= threshold
  ]

  # Order by DEWS (with "Not Covered by DEW" last) and state
  final_summary[, dew_order := ifelse(DEWS == "Not Covered by DEW", 2, 1)]
  setorder(final_summary, dew_order, DEWS, state)

  # Select and rename columns
  result <- final_summary[, .(
    DEW = DEWS,
    State = state,
    Station = name,
    ID = id,
    Elevation_m = elevation,
    Longitude = longitude,
    Latitude = latitude,
    PRCP_coverage = round(PRCP * 100, 3),
    TMAX_coverage = round(TMAX * 100, 3),
    TMIN_coverage = round(TMIN * 100, 3)
  )]

  return(result)
}

#' Create DEWS region summary table for different thresholds
#'
#' @param missingness data.table Output from analyze_station_missingness
#' @param dews sf object NIDIS DEWS boundaries
#' @param thresholds numeric vector Coverage thresholds to analyze
#' @return data.table with station counts by DEWS and threshold
create_dews_threshold_table <- function(missingness, dews,
                                        thresholds = c(0.99, 0.995, 0.999)) {
  library(data.table)

  # Create list to store results for each threshold
  threshold_results <- lapply(thresholds, function(thresh) {
    summary <- create_station_dews_summary(missingness, dews, threshold = thresh)
    counts <- summary[, .(count = .N), by = DEW]
    setnames(counts, "count", sprintf("thresh_%0.3f", thresh))
    return(counts)
  })

  # Combine results
  result <- Reduce(function(x, y) merge(x, y, by = "DEW", all = TRUE),
                   threshold_results)

  # Replace NA with 0
  for(col in names(result)) set(result, which(is.na(result[[col]])), col, 0)

  # Order rows (keeping "Not Covered by DEW" at bottom)
  result[, dew_order := ifelse(DEW == "Not Covered by DEW", 2, 1)]
  setorder(result, dew_order, -thresh_0.995)
  result[, dew_order := NULL]

  # Rename columns for clarity using standard setnames
  old_names <- grep("^thresh_", names(result), value = TRUE)
  new_names <- paste0("Stations_", thresholds * 100, "%")
  setnames(result, old_names, new_names)

  return(result)
}

#' Create faceted map of stations by threshold
#'
#' @param missingness data.table Output from analyze_station_missingness
#' @param dews sf object NIDIS DEWS boundaries
#' @param thresholds numeric vector Coverage thresholds to analyze
#' @return ggplot object
create_threshold_maps <- function(missingness, dews,
                                  thresholds = c(0.99, 0.995, 0.999)) {
  library(ggplot2)
  library(sf)

  # Create base map data
  usa <- st_transform(maps::map("state", plot = FALSE, fill = TRUE) %>%
                        st_as_sf(), st_crs(dews))

  # Create list of station summaries for each threshold
  station_maps <- lapply(thresholds, function(thresh) {
    summary <- create_station_dews_summary(missingness, dews, threshold = thresh)
    summary$threshold <- sprintf("%.1f%% Coverage", thresh * 100)
    return(summary)
  })

  # Combine all thresholds
  all_stations <- rbindlist(station_maps)

  # Convert to sf for plotting
  stations_sf <- st_as_sf(all_stations,
                          coords = c("Longitude", "Latitude"),
                          crs = 4326)

  # Create color palette for DEWS regions
  dews_colors <- scales::hue_pal()(length(unique(dews$DEWS)))
  names(dews_colors) <- unique(dews$DEWS)

  # Create map
  ggplot() +
    # Base map layers
    geom_sf(data = usa, fill = "white", color = "gray70") +
    geom_sf(data = dews,
            aes(fill = DEWS),
            color = "gray20",
            size = 1,
            alpha = 0.5) +
    # Station points
    geom_sf(data = stations_sf,
            size = 1.75,
            shape = 16,  # Solid circle
            alpha = 0.75,
            color = "black") +
    # Aesthetics
    scale_fill_manual(values = dews_colors,
                      name = "DEWS Region") +
    facet_wrap(~threshold, ncol = 1, strip.position = "left") +
    coord_sf(xlim = c(-130, -65), ylim = c(25, 50)) +  # Focus on CONUS
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "gray50"),
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "right",
      strip.placement = "outside"
    ) +
    labs(title = "GHCN Station Distribution by Coverage Percentage",
         subtitle = "For Stations w/ Coverage Between 1900-2023")
}

# Load required packages
library(nclimgrid)
library(data.table)
library(sf)
library(ggplot2)

# Get DEWS boundaries
dews <- sf::st_make_valid(nclimgrid::nidis_dews)

# 1. Create detailed station summary (99.5% threshold)
station_summary <- create_station_dews_summary(
  missingness = missingness,
  dews = dews,
  threshold = 0.995
)

# Save to CSV for sharing
fwrite(station_summary, "high_quality_stations.csv")

# 2. Create threshold comparison table
threshold_summary <- create_dews_threshold_table(
  missingness = missingness,
  dews = dews,
  thresholds = c(0.99, 0.995, 0.999)
)

# 3. Create maps
threshold_maps <- create_threshold_maps(
  missingness = missingness,
  dews = dews,
  thresholds = c(0.99, 0.995, 0.999)
)

# Save map
ggsave("threshold_maps.png", threshold_maps,
       width = 8, height = 12, dpi = 300)
