
#' extract_fires
#'
#' Pull fire information from GOES-R ABI netcdf files and return a csv with lat/lon info
#'
#' @param filename the full path name of the netcdf FDC file to process
#' @param vars The variables to include in the output among c("Temp", "Power", "Area",
#'   "DQF", "Mask"). All are included by default.
#' @param maskvals If "Mask" is selected in vars, these are the mask values to keep. If no
#'   mask values are selected, all pixels will be returned and the output will be very
#'   large.
#'
#' @return A data frame with latitude, longitude, time, and the variables requested.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @import ncdf4
#'
#' @examples df <- extract_fires(infile, maskvals = c(10, 11, 30, 31))
extract_fires <- function(filename, vars = c("Temp", "Power", "Area", "DQF", "Mask"),
                          maskvals = NULL) {

  # open the file
  nc <- nc_open(filename)

  # grab the requested items and splice them together
  get_variable <- function(var, nc) {

    # NOTE - ncvar_get applies scaling factor itself
    var_array <- ncvar_get(nc, var)
    #locs <- which(!is.na(var_array), arr.ind = TRUE)
    vals <- var_array[!is.na(var_array)]

    x <- ncvar_get(nc, "x")
    y <- ncvar_get(nc, "y")
    x_array <- replicate(length(y), x)
    y_array <- t(replicate(length(x), y))
    dplyr::tibble(x = x_array[!is.na(var_array)],
                  y = y_array[!is.na(var_array)],
                  Value = vals,
                  Parameter = var)
  }

  df <- purrr::map_dfr(vars, get_variable, nc) %>%
    tidyr::spread(.data$Parameter, .data$Value)

  if ("Mask" %in% vars & !is.null(maskvals)) {
    df <- dplyr::filter(df, .data$Mask %in% maskvals)
  }

  # If the requested data are empty, return an empty data frame with the correct shape
  if (nrow(df) == 0) {
    df <- dplyr::mutate(df, lon = numeric(0),
                 lat = numeric(0),
                 Filename = character(0),
                 StartTime = .POSIXct(double(0)),
                 EndTime = .POSIXct(double(0))) %>%
          dplyr::select(-.data$x , -.data$y)
    return(df)
  }

  # Get the parameters needed for geolocation
  x_scale <- ncatt_get(nc, "x", "scale_factor")$value
  y_scale <- ncatt_get(nc, "y", "scale_factor")$value
  x_offset <- ncatt_get(nc, "x", "add_offset")$value
  y_offset <- ncatt_get(nc, "y", "add_offset")$value
  r_eq <- ncatt_get(nc, "goes_imager_projection", "semi_major_axis")$value
  r_pol <- ncatt_get(nc, "goes_imager_projection", "semi_minor_axis")$value
  perspective_point <- ncatt_get(nc, "goes_imager_projection",
                                 "perspective_point_height")$value
  H <- perspective_point + r_eq
  lambda0 <- ncatt_get(nc, "goes_imager_projection",
                       "longitude_of_projection_origin")$value * (pi / 180)

  # geolocate
  df <- dplyr::mutate(df, x_rad = .data$x * x_scale + x_offset,
               y_rad = .data$y * y_scale + y_offset) %>%
        dplyr::bind_cols(purrr::map2_dfr(.$x_rad, .$y_rad, goes_lonlat, r_eq,
                                         r_pol, H, lambda0))

  # Add the filename and time
  fname <- fs::path_ext_remove(fs::path_file(filename))
  start_time <- as.POSIXct(ncatt_get(nc, 0, "time_coverage_start")$value,
                           tz = "UTC", format = "%Y-%m-%dT%T")
  end_time <- as.POSIXct(ncatt_get(nc, 0, "time_coverage_end")$value,
                         tz = "UTC", format = "%Y-%m-%dT%T")

  # Close the netcdf file
  nc_close(nc)

  df <- dplyr::mutate(df,
               Filename = fname,
               StartTime = start_time,
               EndTime = end_time) %>%
        dplyr::select(-.data$x_rad, -.data$y_rad, -.data$x, -.data$y)


}



