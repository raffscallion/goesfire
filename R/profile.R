
#' Calculate hourly FRP and FRE for each satellite grid point, using imputed values for
#' fire mask values with no FRP reported. Also report fraction of total energy across all
#' points and time steps for each hour/point
#'
#' @param feer
#' @param maskvals
#' @param cube
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
get_pthourly_frp <- function(cube, feer,
                             maskvals = c(10, 11, 12, 13, 14, 15,
                                          30, 31, 32, 33, 34, 35)) {
  if (!is.null(maskvals)) {
    cube <- cube %>%
      dplyr::filter(Mask %in% maskvals)
  }

  # Count valid power values by location - need at least 2 to interpolate, otherwise use
  # the minimum value of 75
  valids <- cube %>%
    dplyr::group_by(lon, lat) %>%
    dplyr::summarise(ValidCount = sum(is.finite(Power)))

  invalids <- dplyr::filter(valids, ValidCount < 2)
  valids <- dplyr::filter(valids, ValidCount >= 2)

  hourly <- cube %>%
    dplyr::inner_join(valids, by = c("lon", "lat")) %>%
    dplyr::group_by(lon, lat) %>%
    dplyr::mutate(Interpolated = imputeTS::na.interpolation(Power),
                  Hour = lubridate::round_date(time, unit = "hour")) %>%
    dplyr::group_by(lat, lon, Hour) %>%
    dplyr::summarise(Power = mean(Interpolated, na.rm = TRUE),
                     Count = n()) %>%
    dplyr::filter(is.finite(Power)) %>%
    dplyr::mutate(FRE = Power * 3600) # MW * s = MJ

  hourly_invalids <- cube %>%
    dplyr::inner_join(invalids, by = c("lon", "lat")) %>%
    dplyr::group_by(lon, lat) %>%
    dplyr::mutate(Hour = lubridate::round_date(time, unit = "hour")) %>%
    dplyr::group_by(lat, lon, Hour) %>%
    dplyr::summarise(Power = 75,
                     Count = n()) %>%
    dplyr::filter(is.finite(Power)) %>%
    dplyr::mutate(FRE = Power * 3600) # MW * s = MJ

  hourly <- dplyr::bind_rows(hourly, hourly_invalids)

  feer_d <- feer %>%
    dplyr::mutate(lon1d = floor(Longitude),
                  lat1d = floor(Latitude)) %>%
    dplyr::select(lon1d, lat1d, Ce_850, QA_850)

  hourly <- hourly %>%
    dplyr::mutate(lon1d = floor(lon),
                  lat1d = floor(lat)) %>%
    dplyr::left_join(feer_d, by = c("lon1d", "lat1d")) %>%
    dplyr::mutate(Ce_850 = dplyr::if_else(is.na(Ce_850), 0.0159, Ce_850),
                  TPM = FRE * Ce_850, # MJ * kg/MJ = kg TPM
                  Heat_BTU = (TPM / 0.4) * 947.8, # Based on Xrad of 40% from Sukhinin
                  PM25 = TPM * 0.8) # Based on analysis of Sawtooth wilderness PM

  total_energy <- sum(hourly$FRE)

  hourly %>%
    dplyr::mutate(Fraction = FRE / total_energy) %>%
    dplyr::ungroup()

}


#' Take hourly activity, such as from \code{\link{get_pthourly_frp}} or
#' \code{\link{get_hourly_profile}} and return BlueSky fire_locations at daily resolution
#'
#' @param df
#' @param fire_name
#' @param final_area
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
get_bluesky_fire_locations <- function(df, fire_name, final_area) {

  # Work in UTC
  initial_tz <- Sys.timezone()
  Sys.setenv(TZ = "UTC")
  on.exit(Sys.setenv(TZ = initial_tz))

  # Convert hourly area to per pixel daily area
  daily <- df %>%
    dplyr::mutate(Day = lubridate::floor_date(Hour, "days")) %>%
    dplyr::group_by(lon, lat, Day) %>%
    dplyr::summarise_at(vars(Fraction, Energy), sum) %>%
    dplyr::mutate(Area = Fraction * final_area)

  # Create an id for each location
  locs <- daily %>%
    dplyr::ungroup() %>%
    dplyr::select(lon, lat) %>%
    dplyr::distinct() %>%
    dplyr::mutate(id = dplyr::row_number())

  # Convert to bluesky fire_locations format
  daily %>%
    dplyr::inner_join(locs, by = c("lon", "lat")) %>%
    dplyr::mutate(id = paste(fire_name, id, sep = "_"),
                  event_id = fire_name,
                  date_time = strftime(Day, format = "%Y%m%d0000%z"),
                  date_time = paste0(stringr::str_sub(date_time, 1, 15), ":00")) %>%
    dplyr::select(id, event_id, date_time, latitude = lat, longitude = lon, area = Area)

}

create_bluesky_hourly <- function(hourlydf, fire_name, final_area) {

  # Work in UTC
  initial_tz <- Sys.timezone()
  Sys.setenv(TZ = "UTC")
  on.exit(Sys.setenv(TZ = initial_tz))

  # Create an id for each location
  locs <- hourlydf %>%
    dplyr::ungroup() %>%
    dplyr::select(lon, lat, Ce_850) %>%
    dplyr::distinct() %>%
    dplyr::mutate(id = dplyr::row_number())

  # Compute hourly area, PM2.5, and heat
  df <- dplyr::inner_join(hourlydf, locs, by = c("lon", "lat", "Ce_850")) %>%
    dplyr::mutate(id = paste(fire_name, id, sep = "_"),
                  event_id = fire_name,
                  date_time = strftime(Hour, format = "%Y%m%d%H00%z"),
                  date_time = paste0(stringr::str_sub(date_time, 1, 15), ":00"),
                  area_acres = Fraction * final_area,
                  TPM = Energy * Ce_850,
                  pm25_tons = TPM * 0.8 * 0.00110231,
                  heat_btu = (TPM / 0.4) * 947.8) %>%
    dplyr::select(event_id, id, date_time, latitude = lat, longitude = lon, area_acres,
                  power_mw = Power, pm25_tons, heat_btu)


}
