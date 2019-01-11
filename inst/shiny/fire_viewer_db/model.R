
# Take pixels from map viewport and produce bluesky input
model_inputs <- function(file, data, name, size) {

  profile <- get_hourly_profile(data)
  profile_diurnal <- get_diurnal_profile(profile, name)

  points <- get_daily_points(data)
  daily_acres_bluesky <- get_daily_pixel_acres(points, profile, name, size)
  zfile <- zip_files(file, daily_acres_bluesky, profile_diurnal, name)

}

# Use a loess smooth to produce an hourly time profile
get_hourly_profile <- function(df) {
  hourly <- df %>%
    mutate(Hour = lubridate::floor_date(StartTime, unit = "hour")) %>%
    group_by(Hour) %>%
    summarise(Count = n())

  # Fill empty hours with 0 so loess is bounded
  times <- tibble(Hour = seq.POSIXt(from = min(hourly$Hour),
                                    to = max(hourly$Hour),
                                    by = "1 hour"))
  hourly <- left_join(times, hourly, by = "Hour") %>%
    mutate(Count = if_else(is.na(Count), 0L, Count))

  model <- loess(Count ~ as.numeric(Hour), data = hourly, span = 0.1)

  pred <- mutate(hourly, Pred = predict(model, Hour),
                 Pred = if_else(Pred < 0, 0, Pred))

  total <- sum(pred$Pred)
  pred_count <- mutate(pred, Fraction = Pred / total) %>%
    rename(CountSmooth = Pred)
}

get_diurnal_profile <- function(hourly, fire_name) {
  daily_fraction <- hourly %>%
    mutate(Day = lubridate::floor_date(Hour, unit = "day")) %>%
    group_by(Day) %>%
    summarise(FractionOfTotal = sum(Fraction))

  diurnal_fraction <- hourly %>%
    mutate(Day = lubridate::floor_date(Hour, unit = "day")) %>%
    inner_join(daily_fraction, by = "Day") %>%
    mutate(FractionOfDay = Fraction / FractionOfTotal,
           Fire = fire_name,
           Day = as.character(Day), # might not need to convert to character if UTC
           Hour = as.character(Hour)) %>%
    select(Day, Hour, FractionOfDay, Fire)
}

# Get daily model input points
get_daily_points <- function(df) {
  df %>%
    mutate(Day = lubridate::floor_date(StartTime, unit = "day")) %>%
    group_by(Day, lon, lat) %>%
    summarise(Count = n())
}

get_daily_pixel_acres <- function(locs, hourly, fire_name, final_size) {
  daily_acres <- mutate(hourly, Acres = Fraction * final_size) %>%
    mutate(Day = lubridate::floor_date(Hour, "days")) %>%
    group_by(Day) %>%
    summarise(TotalDailyAcres = sum(Acres))

  # Get daily acres for each pixel
  daily_loc_count <- locs %>%
    group_by(Day) %>%
    summarise(DailyCount = n())
  locs_acres <- inner_join(locs, daily_acres, by = "Day") %>%
    inner_join(daily_loc_count, by = "Day") %>%
    ungroup() %>%
    mutate(Acres = TotalDailyAcres / DailyCount) %>%
    select(-Count, -TotalDailyAcres, -DailyCount) %>%
    mutate(Fire = fire_name)

  # Convert into BlueSky ready format
  # id,event_id,date_time,latitude,longitude,area
  # id - a unique ID
  # event_id - the fire name
  # date_time - yyyymmddhhmm-hh:mm format, where the first hh is 00 and "-hh:mm" is the timezone offset
  bluesky <- locs_acres %>%
    mutate(id = paste(Fire, row_number(), sep = "_"),
           date_time = strftime(Day, format = "%Y%m%d0000%z"),
           date_time = paste0(stringr::str_sub(date_time, 1, 15), ":00")) %>%
    select(id, event_id = Fire, date_time, latitude = lat, longitude = lon, area = Acres)
}

## Create two csv files, and zip them up
zip_files <- function(file, points, profile, name) {

  dir.create(t_dir <- tempfile())

  # filenames based on fire name and timestamp
  points_name <- paste0(t_dir, "/", name, "_daily_",
                        strftime(Sys.time(), format = "%Y%m%d_%H%M%S"), ".csv")
  profile_name <- paste0(t_dir, "/", name, "_diurnal_",
                         strftime(Sys.time(), format = "%Y%m%d_%H%M%S"), ".csv")

  readr::write_csv(points, points_name)
  readr::write_csv(profile, profile_name)

  tar(file, t_dir, compression = "gzip")
  return(t_dir)
}

