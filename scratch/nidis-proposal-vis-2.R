#' Create DEWS region summary table for different time periods
#'
#' @param filtered_stations data.table Output from filter_by_coverage
#' @param dews sf object NIDIS DEWS boundaries
#' @return data.table with cumulative station counts by DEWS and time period
create_temporal_summary_table <- function(filtered_stations, dews) {
  library(data.table)
  library(sf)

  # Convert stations to sf for spatial join with DEWS
  stations_sf <- st_as_sf(
    filtered_stations,
    coords = c("longitude", "latitude"),
    crs = 4326
  )

  # Spatial join with DEWS
  stations_dews <- st_join(stations_sf, dews, left = TRUE)
  stations_dt <- as.data.table(st_drop_geometry(stations_dews))

  # Replace NA in DEWS with "Not Covered by DEW"
  stations_dt[is.na(DEWS), DEWS := "Not Covered by DEW"]

  # Get period columns (excluding earliest_period)
  period_cols <- grep("^[0-9]", names(stations_dt), value = TRUE)

  # Create count for each period (cumulative)
  summary_table <- stations_dt[, lapply(.SD, function(x) sum(x > 0)),
                               .SDcols = period_cols,
                               by = DEWS]

  # Order periods chronologically if needed
  setcolorder(summary_table, c("DEWS", sort(period_cols)))

  # Order rows (keeping "Not Covered by DEW" at bottom)
  summary_table[, dew_order := ifelse(DEWS == "Not Covered by DEW", 2, 1)]
  summary_table[, Total := get(period_cols[length(period_cols)])]  # Total is same as most recent period
  setorder(summary_table, dew_order, -Total)
  summary_table[, dew_order := NULL]

  return(summary_table)
}

#' Create faceted map by time period
#'
#' @param filtered_stations data.table Output from filter_by_coverage
#' @param dews sf object NIDIS DEWS boundaries
#' @return ggplot object
create_period_maps <- function(filtered_stations, dews) {
  library(ggplot2)
  library(sf)
  library(data.table)

  # Create base map data
  usa <- st_transform(maps::map("state", plot = FALSE, fill = TRUE) %>%
                        st_as_sf(), st_crs(dews))

  # Get period columns
  period_cols <- grep("^[0-9]", names(filtered_stations), value = TRUE)

  # Reshape to long format using data.table
  stations_long <- melt(filtered_stations,
                        id.vars = c("id", "name", "state", "latitude", "longitude",
                                    "elevation", "earliest_period"),
                        measure.vars = period_cols,
                        variable.name = "period",
                        value.name = "coverage")

  # Filter for qualified periods
  stations_long <- stations_long[coverage > 0]

  # Convert to sf
  stations_sf <- st_as_sf(
    stations_long,
    coords = c("longitude", "latitude"),
    crs = 4326
  )

  # Modify DEWS names to add asterisk for unofficial regions
  dews$DEWS_label <- dews$DEWS
  dews$DEWS_label[dews$DEWS %in% c("Mid Atlantic", "Lower Mississippi")] <-
    paste0(dews$DEWS[dews$DEWS %in% c("Mid Atlantic", "Lower Mississippi")], "*")

  # Create color palette for DEWS regions
  unique_dews <- unique(dews$DEWS)
  dews_colors <- scales::hue_pal()(length(unique_dews))
  names(dews_colors) <- unique_dews

  # Create faceted map
  ggplot() +
    geom_sf(data = usa, fill = "white", color = "gray70") +
    geom_sf(data = dews,
            aes(fill = DEWS),
            color = "gray40",
            size = 1,
            alpha = 0.5) +
    geom_sf(data = stations_sf,
            size = 1.75,
            alpha = 0.75,
            shape = 16,
            color = "black") +
    scale_fill_manual(values = dews_colors,
                      name = "DEWS Region",
                      labels = unique(dews$DEWS_label)) +  # Use modified labels
    facet_wrap(~period, ncol = 1) +
    coord_sf(xlim = c(-130, -65), ylim = c(25, 50)) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "gray90"),
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "right",
      plot.caption = element_text(hjust = 0, size = 9)
    ) +
    labs(title = "GHCN-D Stations Meeting 99% Coverage Threshold",
         subtitle = "For PRCP, TMAX, and TMIN in the Specified Periods",
         caption = "* Mid Atlantic and Lower Mississippi are unofficial DEWS regions included to complete CONUS coverage")
}

create_period_maps <- function(filtered_stations, dews) {
  library(ggplot2)
  library(sf)
  library(data.table)

  # Create base map data
  usa <- st_transform(maps::map("state", plot = FALSE, fill = TRUE) %>%
                        st_as_sf(), st_crs(dews))

  # Get period columns
  period_cols <- grep("^[0-9]", names(filtered_stations), value = TRUE)

  # Reshape to long format using data.table
  stations_long <- melt(filtered_stations,
                        id.vars = c("id", "name", "state", "latitude", "longitude",
                                    "elevation", "earliest_period"),
                        measure.vars = period_cols,
                        variable.name = "period",
                        value.name = "coverage")

  # Filter for qualified periods
  stations_long <- stations_long[coverage > 0]

  # Convert to sf
  stations_sf <- st_as_sf(
    stations_long,
    coords = c("longitude", "latitude"),
    crs = 4326
  )

  # Modify DEWS names to add asterisk for unofficial regions
  dews$DEWS_label <- dews$DEWS
  dews$DEWS_label[dews$DEWS %in% c("Mid Atlantic", "Lower Mississippi")] <-
    paste0(dews$DEWS[dews$DEWS %in% c("Mid Atlantic", "Lower Mississippi")], "*")

  # Create color palette for DEWS regions - ensure consistent order
  dews_colors <- viridis::viridis_pal()(length(unique(dews$DEWS)))
  names(dews_colors) <- sort(unique(dews$DEWS))  # Sort to ensure consistency

  # Get extent from DEWS shapefile to ensure all regions are shown
  dews_bbox <- st_bbox(dews)

  ggplot() +
    geom_sf(data = usa, fill = "white", color = "gray70", linewidth = 0.5) +
    geom_sf(data = dews,
            aes(fill = DEWS),
            color = "gray30",
            linewidth = 1.25,
            alpha = 0.5) +
    geom_sf(data = stations_sf,
            size = 2.25,
            alpha = 0.85,
            shape = 16,
            color = "gray20") +
    scale_fill_manual(values = dews_colors,
                      name = "DEWS Region",
                      breaks = sort(unique(dews$DEWS)),
                      labels = dews$DEWS_label[match(sort(unique(dews$DEWS)), dews$DEWS)]) +
    facet_wrap(~period, ncol = 1) +
    coord_sf(xlim = c(dews_bbox["xmin"], dews_bbox["xmax"]),
             ylim = c(dews_bbox["ymin"], dews_bbox["ymax"])) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "gray90"),
      strip.text = element_text(size = 16, face = "bold"),
      legend.position = "right",
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 16),
      plot.caption = element_text(hjust = 0, size = 13),
      axis.text = element_blank(),
      axis.title = element_blank(),
      legend.text = element_text(size = 14),  # Increase legend text size
      legend.title = element_text(size = 18), # Increase legend title size
      legend.key.size = unit(1.25, "cm")) +      # Increase symbol size) +
  labs(title = "GHCN-D Stations Meeting 98% Coverage Threshold",
       subtitle = "For PRCP, TMAX, and TMIN Variables in Specified Time Period",
       caption = "* Mid Atlantic and Lower Mississippi are unofficial DEWS regions included to complete CONUS coverage")
}

dews <- nclimgrid::nidis_dews_ext

# Create all summaries
dews_summary <- create_temporal_summary_table(qualified_stations, dews)
period_maps <- create_period_maps(qualified_stations, dews)
combined_map <- create_combined_period_map(qualified_stations, dews)

# Save outputs
fwrite(dews_summary, "dews_temporal_summary.csv")
ggsave("period_maps.png", period_maps, width = 10, height = 15, dpi = 300)
ggsave("combined_map.png", combined_map, width = 12, height = 8, dpi = 300)
