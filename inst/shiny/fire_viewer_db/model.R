
# Take pixels from map viewport and produce bluesky input
model_inputs <- function(file, data, name, size, type, tz) {

  # Convert all times to local time as specified by user
  data <- mutate(data, StartTime = lubridate::with_tz(StartTime, tz))

  hourly <- get_hourly_data(data)
  profile <- get_diurnal_profile(hourly, name, tz)
  daily <- create_bluesky_daily(hourly, name, size, type, tz)

  zfile <- zip_files(file, daily, profile, hourly, name)

}

# Get Hourly Profile redux (The S2 version using FRE and per pixel profiles)
get_hourly_data <- function(df) {
  # Count valid power values by location - need at least 2 to interpolate, otherwise use
  # the minimum value of 75
  valids <- df %>%
    dplyr::group_by(lon, lat) %>%
    dplyr::summarise(ValidCount = sum(is.finite(PM25)))

  invalids <- dplyr::filter(valids, ValidCount < 2)
  valids <- dplyr::filter(valids, ValidCount >= 2)

  hourly <- df %>%
    dplyr::inner_join(valids, by = c("lon", "lat")) %>%
    dplyr::group_by(lon, lat) %>%
    dplyr::mutate(Interpolated = imputeTS::na_interpolation(Power),
                  InterpolatedPM = imputeTS::na_interpolation(PM25),
                  Hour = lubridate::round_date(StartTime, unit = "hour")) %>%
    dplyr::group_by(lat, lon, Hour) %>%
    dplyr::summarise(Power = mean(Interpolated, na.rm = TRUE),
                     PM25 = sum(InterpolatedPM, na.rm = TRUE),
                     Count = n()) %>%
    dplyr::filter(is.finite(Power)) %>%
    dplyr::mutate(FRE = Power * 3600) # MW * s = MJ

  hourly_invalids <- df %>%
    dplyr::inner_join(invalids, by = c("lon", "lat")) %>%
    dplyr::group_by(lon, lat) %>%
    dplyr::mutate(Hour = lubridate::round_date(StartTime, unit = "hour")) %>%
    dplyr::group_by(lat, lon, Hour) %>%
    dplyr::summarise(Power = 75,
                     PM25 = 5,
                     Count = n()) %>%
    dplyr::filter(is.finite(Power)) %>%
    dplyr::mutate(FRE = Power * 3600) # MW * s = MJ

  hourly <- dplyr::bind_rows(hourly, hourly_invalids) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Day = lubridate::floor_date(Hour, unit = "days"))

}

get_diurnal_profile <- function(hourly, name, tz) {
  # Create daily fraction of total FRE - currently the profile applies to the entire event
  profile <- hourly %>%
    dplyr::group_by(Hour) %>%
    dplyr::summarise(HourlyFRE = sum(FRE, na.rm = TRUE)) %>%
    dplyr::mutate(Day = lubridate::floor_date(Hour, unit = "days"))

  daily <- profile %>%
    dplyr::group_by(Day) %>%
    dplyr::summarise(DailyFRE = sum(HourlyFRE))

  profile <- inner_join(profile, daily, by = "Day") %>%
    dplyr::mutate(FractionOfDay = HourlyFRE / DailyFRE,
           Fire = name,
           LocalDay = strftime(Day, format = "%Y-%m-%d"),
           LocalHour = strftime(Hour, format = "%Y-%m-%d %H:00", tz = tz)) %>%
    dplyr::select(LocalDay, LocalHour, FractionOfDay, Fire)

  # Fill in missing hours with zeroes
  days <- dplyr::select(profile, LocalDay) %>%
    dplyr::distinct() %>%
    .$LocalDay
  h <- seq.int(0, 23)
  all_hours <- paste0(" ", sprintf("%02d", h), ":00")
  complete_set <- tidyr::crossing(days, all_hours) %>%
    dplyr::mutate(LocalHour = paste0(days, all_hours),
                  Fire = name) %>%
    dplyr::select(LocalDay = days, LocalHour, Fire)

  profile <- dplyr::left_join(complete_set, profile,
                              by = c("LocalDay", "LocalHour", "Fire")) %>%
    dplyr::mutate(FractionOfDay = dplyr::if_else(is.na(FractionOfDay), 0, FractionOfDay))

}

create_bluesky_daily <- function(df, fire_name, final_area, type, tz) {

  # Convert hourly FRE to per pixel daily area
  total_FRE <- sum(df$FRE, na.rm = TRUE)

  daily <- df %>%
    dplyr::mutate(Day = lubridate::floor_date(Hour, "days")) %>%
    dplyr::group_by(lon, lat, Day) %>%
    dplyr::summarise(FRE_Daily = sum(FRE, na.rm = TRUE)) %>%
    dplyr::mutate(Fraction = FRE_Daily / total_FRE,
                  area = Fraction * final_area)

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
                  fire_type = type,
                  date_time = strftime(Day, format = "%Y%m%d0000%z", tz = tz),
                  date_time = paste0(stringr::str_sub(date_time, 1, 15), ":00")) %>%
    dplyr::select(id, Day, event_id, fire_type, date_time, latitude = lat,
                  longitude = lon, area)

}

## Create three csv files for each day, and zip them up
zip_files <- function(file, points, profile, hourly, name) {

  dir.create(t_dir <- tempfile())

  days <- points %>%
    dplyr::ungroup() %>%
    dplyr::select(Day) %>%
    dplyr::distinct() %>%
    .$Day

  bluesky_files <- function(day, points, profile, hourly, name, temp_dir) {

    # filenames based on fire name and timestamp
    points_name <- paste0(temp_dir, "/", "fire_locations_",
                          strftime(day, format = "%Y%m%d_"), name, ".csv")
    profile_name <- paste0(temp_dir, "/", name, "_diurnal_profile_localtime_",
                           strftime(day, format = "%Y%m%d"), ".csv")
    hourly_name <- paste0(temp_dir, "/", name, "_hourly_localtime_",
                           strftime(day, format = "%Y%m%d"), ".csv")

    # subset data by day
    points <- dplyr::filter(points, Day == day)
    profile <- dplyr::filter(profile, LocalDay == as.character(day))
    hourly <- dplyr::filter(hourly, Day == day)

    readr::write_csv(points, points_name)
    readr::write_csv(profile, profile_name)
    readr::write_csv(hourly, hourly_name)

  }

  # Add a summary of acres and pm2.5 (in tons) per day
  areas <- points %>%
    group_by(Day) %>%
    summarise(Area_acres = sum(area))

  by_day <- hourly %>%
    group_by(Day) %>%
    summarise(PM25_tons = sum(PM25) / 907.185) %>%
    inner_join(areas, by = "Day") %>%
    mutate(TonsPerAcre = PM25_tons / Area_acres)
  by_day_name <- paste0(t_dir, "/", "daily_totals_", name, ".csv")
  readr::write_csv(by_day, by_day_name)

  purrr::walk(days, bluesky_files, points, profile, hourly, name, t_dir)

  tar(file, t_dir, compression = "gzip")
  return(t_dir)
}

