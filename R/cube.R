
# Tools for slicing GEOS netcdf into cubes corresponding to geomac perimeters (or other bounding boxes)

#' create_cube
#'
#' Takes a bounding box and date_range and returns a tibble for analysis. For example,
#' this can be used to compare fire perimeters from another source
#'
#' @param bbox c(min_lon, min_lat, max_lon, max_lat) in decimal degrees
#' @param date_range c(start_date, end_date)
#' @param variables  The variables to attach. Must all be numeric. Default is c("Mask",
#'   "Area", "Power")
#' @param input_path character Path to the input netcdf files
#' @param coords Data frame of coordinates as returned from \code{\link{get_coor_grid}}.
#'   If not provided, it will be calculated from the first input file
#'
#' @return A tibble of with x,y, lat,lon, start time, and the variables requested
#' @export
#'
#' @import ncdf4
#' @importFrom magrittr %>%
#'
#' @examples
create_cube <- function(bbox, date_range, variables = c("Mask", "Area", "Power"),
                        input_path, coords = NULL) {

  # Interrogate files to determine which contain data within the time period. This is way
  # slow opening the netcdf files, so rely on the file names. See
  # https://geonetcast.wordpress.com/2017/04/27/goes-16-file-naming-convention/ for file
  # naming convention
  file_list <- fs::dir_ls(input_path)

  # Convert the observation start string from a julian day to a date
  start_dates <- stringr::str_sub(file_list, start = -49, end = -36) %>%
    as.POSIXct(tz = "UTC", format = "%Y%j%H%M%S")

  # Adding a buffer of 1 day to the date ranges to account for UTC offset or other
  # strangeness
  date_range <- as.Date(date_range)
  date_range[1] <- date_range[[1]] - 1
  date_range[2] <- date_range[[2]] + 1

  files <- file_list[(start_dates > date_range[[1]] & start_dates < date_range[[2]])]
  files <- files[!is.na(files)] # not sure why an NA is appended

  # This code assumes that the grids are the same for all time slices, which is true as
  # long as no time periods cross orbit changes
  if (is.null(coords)) {
    nc <- nc_open(files[1])
    print("Determining needed coordinates")
    coords <- get_coord_grid(nc)
    nc_close(nc)
  }

  # Determine which records are required spatially. Add about 4 km as a buffer
  lat_rads <- bbox[[2]] * (pi/180)
  degree_lon <- 4 / (cos(lat_rads) * 111.3)
  degree_lat <- 4 / 111.3
  expanded <- c(bbox[[1]] - degree_lon, bbox[[2]] - degree_lat, bbox[[3]] + degree_lon,
                bbox[[4]] + degree_lat)

  needed_recs <- dplyr::filter(coords, lon > expanded[[1]], lat > expanded[[2]],
                          lon < expanded[[3]], lat < expanded[[4]]) %>%
    dplyr::select(-x_rad, -y_rad)

  safe_nc_open <- purrr::safely(nc_open)

  # Now, for each file in the time range, select the needed data from variable of interest
  single_timeslice <- function(file, variables, needed_recs, pb) {

    pb$tick()$print()
    try_open <- safe_nc_open(file)
    if (!is.null(try_open$error)) {
      return(NULL)
    } else {
      nc <- try_open$result
    }
    variable_list <- purrr::map(variables, get_one_variable, nc)
    names(variable_list) <- variables
    variables_df <- dplyr::as_tibble(variable_list)
    needed_recs <- dplyr::bind_cols(needed_recs, variables_df)
    needed_recs$time <- as.POSIXct(ncatt_get(nc, varid = 0,
                                             attname = "time_coverage_start")$value,
                                   tz = "UTC", format = "%Y-%m-%dT%T")
    nc_close(nc)
    return(needed_recs)
  }

  get_one_variable <- function(var, nc) {
    variable <- ncvar_get(nc, var)
    purrr::map2_dbl(needed_recs$x, needed_recs$y, get_matrix_value,
                    variable)
  }

  get_matrix_value <- function(x, y, variable) {
    # Note index offset because R starts at 1
    value <- variable[x+1, y+1]
  }

  pb <- dplyr::progress_estimated(length(files))
  purrr::map_dfr(files, single_timeslice, variables, needed_recs, pb)

}

#' process_fire_cubes
#'
#' Takes a set of fire bounding boxes, expressed as a data frame with start and end dates
#' and lat/lon bounds and runs create cube on each, storing the results as RDS files - one
#' per data frame row
#'
#' @param df A data frame with the following fields: fire_name and sf_id (will be the name
#'   of the output RDS file); sf_start and sf_end (begin and end dates of fire); xmin,
#'   ymin, xmax, and ymax (coordinates of the fire's bounding box)
#' @param ... Additional parameters passed to create_cube
#'
#' @return Returns the list of fire names processed
#' @export
#'
#' @examples
process_fire_cubes <- function(df, input_path, output_path,
                               variables = c("Mask", "Area", "Power"), coords = NULL) {

  process_one_cube <- function(fire, input_path, variables, coords, output_path) {
    message("Processing ", fire$fire_name)
    bbox <- c(fire$xmin, fire$ymin, fire$xmax, fire$ymax)
    date_range <- c(fire$sf_start, fire$sf_end)
    result <- create_cube(bbox, date_range, variables, input_path, coords)
    # Need to sanitize fire name
    outname <- paste0(make.names(fire$fire_name), "_", fire$sf_id, ".RDS")
    outfile <- paste0(output_path, outname)
    saveRDS(result, outfile)
  }

  # Get a coordinate grid if we don't already have one
  if (is.null(coords)) {
    files <- fs::dir_ls(input_path)
    nc <- ncdf4::nc_open(files[1])
    print("Determining needed coordinates")
    coords <- get_coord_grid(nc)
  }

  purrr::walk(purrr::transpose(df), process_one_cube, input_path, variables, coords,
              output_path)

}


#' Refine a goesfire cube to only the points that fall within a polygon
#'
#' @param cube A goesfire cube data.frame as produced by \code{\link{create_cube}}
#' @param polygon A simple feature polygon (or multipolygon)
#' @param buffer Buffer distance in km (default = 4)
#' @param crs Coordinate reference system: integer with EPSG code, or character with
#'   proj4string (default is 5070 - Albers Equal Area)
#'
#' @return A goesfire cube data.frame, subsetted by the polygon
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
cube_subset <- function(cube, polygon, buffer = 4, crs = 5070) {

  # Buffer the polygon
  buffered <- polygon %>%
    sf::st_transform(crs) %>% # Work in Albers Equal Area projection
    sf::st_buffer(dist = buffer * 1000)

  cube_locs <- cube %>%
    dplyr::select(x, y, lon, lat) %>%
    dplyr::distinct()

  locs_sf <- sf::st_as_sf(cube_locs, coords = c("lon", "lat"),
                          crs = "+proj=longlat +datum=WGS84",
                          remove = FALSE)

  locs_subset <- suppressWarnings(
    sf::st_transform(locs_sf, crs) %>%
    sf::st_intersection(buffered) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(x, y))

  dplyr::right_join(cube, locs_subset, by = c("x", "y"))

}
