% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ghcn.inventory}
\alias{ghcn.inventory}
\title{GHCN-D Inventory Data}
\format{
A data table with 6 variables:
\describe{
  \item{id}{character. 11-character station identification code.}
  \item{latitude}{numeric. Latitude of the station in decimal degrees.}
  \item{longitude}{numeric. Longitude of the station in decimal degrees.}
  \item{element}{character. Element type (e.g., PRCP for precipitation, TMAX for maximum temperature).}
  \item{firstyear}{integer. First year of record for this station and element.}
  \item{lastyear}{integer. Last year of record for this station and element.}
}
}
\source{
\url{http://noaa-ghcn-pds.s3.amazonaws.com/ghcnd-inventory.txt}
}
\usage{
ghcn.inventory
}
\description{
A dataset containing inventory information for each station and element in the Global Historical Climatology Network - Daily (GHCN-D).
}
\keyword{datasets}
