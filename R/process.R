
#' aggregate_hourly
#'
#' A simple convenience function to turn raw extracted fires into hourly aggregates. Area
#' and power are summed, temperature is averaged, and all mask values are listed.
#'
#' @param fires A data frame of fires as returned from \code{\link{extract_fires}}
#'
#' @return A data frame of hourly aggregate fires
#' @export
#'
#' @import tidyverse
#'
#' @examples hourly_fires <- aggregate_hourly(fires)
aggregate_hourly <- function(fires) {

  require(tidyverse)

  fires %>%
    mutate(StartTime = lubridate::floor_date(StartTime, unit = "hours")) %>%
    group_by(lon, lat, StartTime) %>%
    summarise(Area = sum(Area, na.rm = TRUE),
              DQF = paste(unique(DQF), collapse = ";"),
              Mask = paste(unique(Mask), collapse = ";"),
              Power = sum(Power, na.rm = TRUE),
              Temp = mean(Temp, na.rm = TRUE)) %>%
    ungroup()

}
