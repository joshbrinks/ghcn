# install.packages("remotes")
# remotes::install_github("joshbrinks/ghcn")

btv_stations <- ghcn::find_stations_aws(lat = 44.4193, lon = -72.0151, radius = 10)
head(btv_stations)

station_ids <- btv_stations$id
inventory <- ghcn::get_inventory_aws("USC00437054")
head(inventory)

station_data <- ghcn::get_data_aws(
  station_id = "ASN00005094",
  start_date = "1700-01-01",
  end_date = "2024-10-01",
  variables = c("PRCP", "TAVG", "TMAX", "TMIN", "SNOW", "SNWD")
)
head(station_data)

data.table::fwrite(station_data, "btv_p_sn_t.csv")
data.table::fwrite(inventory, "st_johns_inventory.csv")
