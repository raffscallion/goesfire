
#' Title
#'
#' @param df
#' @param span
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
get_hourly_profile <- function(df, span = 0.1) {

  hourly <- df %>%
    dplyr::mutate(Hour = lubridate::floor_date(StartTime, unit = "hour")) %>%
    dplyr::group_by(Hour) %>%
    dplyr::summarise(Count = n())

  # Fill empty hours with 0 so loess is bounded
  times <- tibble(Hour = seq.POSIXt(from = min(hourly$Hour),
                                    to = max(hourly$Hour),
                                    by = "1 hour"))
  hourly <- dplyr::left_join(times, hourly, by = "Hour") %>%
    dplyr::mutate(Count = if_else(is.na(Count), 0L, Count))

  model <- loess(Count ~ as.numeric(Hour), data = hourly, span = 0.1)

  pred <- dplyr::mutate(hourly, Pred = predict(model, Hour),
                 Pred = dplyr::if_else(Pred < 0, 0, Pred))

  total <- sum(pred$Pred)
  pred_count <- dplyr::mutate(pred, Fraction = Pred / total) %>%
    dplyr::rename(CountSmooth = Pred)

}


#' Calculate hourly FRP and FRE for each satellite grid point, using imputed values for
#' fire mask values with no FRP reported. Also report fraction of total energy across all
#' points and time steps for each hour/point
#'
#' @param df
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
get_pthourly_frp <- function(df) {

  hourly <- df %>%
    dplyr::group_by(lon, lat) %>%
    dplyr::mutate(Interpolated = imputeTS::na.interpolation(Power),
                  Hour = lubridate::round_date(StartTime, unit = "hour")) %>%
    dplyr::group_by(lat, lon, Hour) %>%
    dplyr::summarise(Power = mean(Interpolated, na.rm = TRUE),
                     Ce_850 = mean(Ce_850, na.rm = TRUE)) %>%
    dplyr::filter(is.finite(Power)) %>%
    dplyr::mutate(Energy = Power * 3600)

  total_energy <- sum(hourly$Energy)

  hourly %>%
    dplyr::mutate(Fraction = Energy / total_energy) %>%
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
