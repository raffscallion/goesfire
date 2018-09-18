
# Various helper functions

safe_nc_open <- purrr::safely(nc_open)

#' get_coord_bounds
#'
#' Determines the x and y bounds (in radians) for a given netcdf handle
#'
#' @param nc
#'
#' @return A named list with x1, x2, y1, and y2
#' @export
#'
#' @examples
get_coord_bounds <- function(nc) {
  x_bounds <- ncvar_get(nc, "x_image_bounds")
  y_bounds <- ncvar_get(nc, "y_image_bounds")
  list(x1 = x_bounds[1], x2 = x_bounds[2], y1 = y_bounds[1], y2 = y_bounds[2])
}


#' get_coord_grid
#'
#' Given a netCDF file, returns a data frame of decimal degree lon/lat pairs
#'
#' @param nc
#'
#' @return
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
get_coord_grid <- function(nc) {

  # Get the x and y variables
  x <- ncvar_get(nc, varid = "x")
  y <- ncvar_get(nc, varid = "y")

  # Convert to radians
  x_scale <- ncatt_get(nc, "x", "scale_factor")$value
  y_scale <- ncatt_get(nc, "y", "scale_factor")$value
  x_offset <- ncatt_get(nc, "x", "add_offset")$value
  y_offset <- ncatt_get(nc, "y", "add_offset")$value

  x_array <- replicate(length(y), x)
  y_array <- t(replicate(length(x), y))
  df <- dplyr::tibble(x = as.vector(x_array),
                      y = as.vector(y_array)) %>%
    dplyr::mutate(x_rad = x * x_scale + x_offset,
                  y_rad = y * y_scale + y_offset)

  # Get the parameters needed for geolocation
  r_eq <- ncatt_get(nc, "goes_imager_projection", "semi_major_axis")$value
  r_pol <- ncatt_get(nc, "goes_imager_projection", "semi_minor_axis")$value
  perspective_point <- ncatt_get(nc, "goes_imager_projection",
                                 "perspective_point_height")$value
  H <- perspective_point + r_eq
  lambda0 <- ncatt_get(nc, "goes_imager_projection",
                       "longitude_of_projection_origin")$value * (pi / 180)

  # geolocate
  dplyr::bind_cols(df, purrr::map2_dfr(df$x_rad, df$y_rad, goes_lonlat, r_eq, r_pol, H,
                                       lambda0))

}


#' goes_lonlat
#'
#' Converts GOES xy radian coordinates to latitude and longitude pairs
#'
#' @param x x value from the netcdf in radians (scale and offset already applied)
#' @param y y value from the netcdf in radians (scale and offset already applied)
#' @param r_eq semi_major_axis
#' @param r_pol semi_minor_axis
#' @param H satellite height from center of earth = perspective_point_height +
#'   semi_major_axis
#' @param lambda0 longitude_of_projection_origin (converted to radians)
#'
#' @return a named list with lon and lat values
#'
#' @examples coords <- goes_lonlat(x_rad, y_rad, r_eq, r_pol, H, lambda0)
goes_lonlat <- function(x, y, r_eq, r_pol, H, lambda0) {

  # Calculate distnace from satellite to point of interest
  a <- sin(x)^2 + cos(x)^2 * (cos(y)^2 + (r_eq^2 / r_pol^2) * sin(y)^2)
  b <- -2 * H * cos(x) * cos(y)
  c <- H^2 - r_eq^2
  r_s <- (-b - sqrt(b^2 - 4 * a * c)) / (2 * a)

  # Satellite coordinates
  sx <- r_s * cos(x) * cos(y)
  sy <- -r_s * sin(x)
  sz <- r_s * cos(x) * sin(y)

  # Coordinates in degrees
  lon <- ((lambda0 - atan(sy / (H - sx))) * 180) / pi
  lat <- (atan((r_eq^2 / r_pol^2) * (sz / sqrt((H - sx)^2 + sy^2))) * 180) / pi

  return(list("lon"=lon, "lat"=lat))

}
