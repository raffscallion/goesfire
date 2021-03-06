% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cube.R
\name{create_cube}
\alias{create_cube}
\title{create_cube}
\usage{
create_cube(bbox, date_range, variables = c("Mask", "Area", "Power"),
  input_path, coords = NULL)
}
\arguments{
\item{bbox}{c(min_lon, min_lat, max_lon, max_lat) in decimal degrees}

\item{date_range}{c(start_date, end_date)}

\item{variables}{The variables to attach. Must all be numeric. Default is c("Mask",
"Area", "Power")}

\item{input_path}{character Path to the input netcdf files}

\item{coords}{Data frame of coordinates as returned from \code{\link{get_coor_grid}}.
If not provided, it will be calculated from the first input file}
}
\value{
A tibble of with x,y, lat,lon, start time, and the variables requested
}
\description{
Takes a bounding box and date_range and returns a tibble for analysis. For example,
this can be used to compare fire perimeters from another source
}
