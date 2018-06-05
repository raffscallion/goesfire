
#' aggregate_hourly
#'
#' A simple convenience function to turn raw extracted fires into hourly aggregates. Area
#' is summed, power and temperature are averaged, and all mask values are listed. If FRE
#' and TPM emissions are present, they are summed as well.
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

  if(!"TPM" %in% names(fires)) {
    fires %>%
      mutate(StartTime = lubridate::floor_date(StartTime, unit = "hours")) %>%
      group_by(lon, lat, StartTime) %>%
      summarise(Area = sum(Area, na.rm = TRUE),
                DQF = paste(unique(DQF), collapse = ";"),
                Mask = paste(unique(Mask), collapse = ";"),
                Power = mean(Power, na.rm = TRUE),
                Temp = mean(Temp, na.rm = TRUE)) %>%
      ungroup()

  } else {
    fires %>%
      mutate(StartTime = lubridate::floor_date(StartTime, unit = "hours")) %>%
      group_by(lon, lat, StartTime) %>%
      summarise(Area = sum(Area, na.rm = TRUE),
                DQF = paste(unique(DQF), collapse = ";"),
                Mask = paste(unique(Mask), collapse = ";"),
                Power = mean(Power, na.rm = TRUE),
                Temp = mean(Temp, na.rm = TRUE),
                FRE = sum(FRE, na.rm = TRUE),
                TPM = sum(TPM, na.rm = TRUE)) %>%
      ungroup()

  }
}

#' feer_emissions
#'
#' Estimate emissions of total PM using the smoke emissions coefficient lookup developed
#' by the NASA Fire Energetics and Emissions Research group. If a Ce value is missing, we
#' use a fill value of 0.0159 kg/MJ, which is the global median.
#'
#' @param feer The FEERv1.0_Ce.csv file loaded as a data frame. This can be acquired from
#'   \url{https://feer.gsfc.nasa.gov/}
#' @param fires A data frame of fires processed by \code{extract_fires}
#'
#' @return A data frame with FEER information and TPM (in kg) attached
#' @export
#'
#' @examples emissions <- feer_emissions(fires, feer)
feer_emissions <- function(fires, feer) {

  require(tidyverse)

  # FEER is on a 1x1 degree grid, centered on half degrees. Can join on whole degrees.
  feer <- mutate(feer, lon1d = floor(Longitude),
                 lat1d = floor(Latitude)) %>%
    select(lon1d, lat1d, Ce_850, QA_850)

  fires <- mutate(fires, lon1d = floor(lon),
                  lat1d = floor(lat)) %>%
    left_join(feer, by = c("lon1d", "lat1d")) %>%
    mutate(Seconds = as.double(EndTime - StartTime, units = "secs"),
           FRE = Power * Seconds, # MW * s = MJ
           Ce_850 = if_else(is.na(Ce_850), 0.0159, Ce_850),
           TPM = FRE * Ce_850) # MJ * kg/MJ = kg TPM


}
