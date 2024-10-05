#' GHCN Daily Datatypes
#'
#' A dataset containing the Global Historical Climatology Network (GHCN) daily
#' datatypes and their descriptions.
#'
#' @format A data frame with 56 rows and 2 variables:
#' \describe{
#'   \item{datatype}{GHCN daily datatype code}
#'   \item{description}{Description of what the datatype represents}
#' }
#' @source \url{https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt}
"ghcn_daily_datatypes"

#' GHCN-D Stations Data
#'
#' A dataset containing information about weather stations in the Global Historical Climatology Network - Daily (GHCN-D).
#'
#' @format A data table with 9 variables:
#' \describe{
#'   \item{id}{character. 11-character station identification code.}
#'   \item{latitude}{numeric. Latitude of the station in decimal degrees.}
#'   \item{longitude}{numeric. Longitude of the station in decimal degrees.}
#'   \item{elevation}{numeric. Elevation of the station in meters.}
#'   \item{state}{character. U.S. postal code for the state (for U.S. and Canadian stations only).}
#'   \item{name}{character. Name of the station.}
#'   \item{gsn_flag}{character. Flag indicating if the station is part of the GCOS Surface Network (GSN).}
#'   \item{hcn_crn_flag}{character. Flag indicating if the station is part of the U.S. Historical Climatology Network (HCN) or U.S. Climate Reference Network (CRN).}
#'   \item{wmo_id}{character. World Meteorological Organization (WMO) number for the station.}
#' }
#' @source \url{http://noaa-ghcn-pds.s3.amazonaws.com/ghcnd-stations.txt}
"ghcn.stations"

#' GHCN-D Inventory Data
#'
#' A dataset containing inventory information for each station and element in the Global Historical Climatology Network - Daily (GHCN-D).
#'
#' @format A data table with 6 variables:
#' \describe{
#'   \item{id}{character. 11-character station identification code.}
#'   \item{latitude}{numeric. Latitude of the station in decimal degrees.}
#'   \item{longitude}{numeric. Longitude of the station in decimal degrees.}
#'   \item{element}{character. Element type (e.g., PRCP for precipitation, TMAX for maximum temperature).}
#'   \item{firstyear}{integer. First year of record for this station and element.}
#'   \item{lastyear}{integer. Last year of record for this station and element.}
#' }
#' @source \url{http://noaa-ghcn-pds.s3.amazonaws.com/ghcnd-inventory.txt}
"ghcn.inventory"
