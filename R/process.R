
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
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples hourly_fires <- aggregate_hourly(fires)
aggregate_hourly <- function(fires) {

  if(!"TPM" %in% names(fires)) {
    fires %>%
      dplyr::mutate(StartTime = lubridate::floor_date(.data$StartTime, unit = "hours")) %>%
      dplyr::group_by(.data$lon, .data$lat, .data$StartTime) %>%
      dplyr::summarise(Area = sum(.data$Area, na.rm = TRUE),
                DQF = paste(unique(.data$DQF), collapse = ";"),
                Mask = paste(unique(.data$Mask), collapse = ";"),
                Power = mean(.data$Power, na.rm = TRUE),
                Temp = mean(.data$Temp, na.rm = TRUE)) %>%
      dplyr::ungroup()

  } else {
    fires %>%
      dplyr::mutate(StartTime = lubridate::floor_date(.data$StartTime, unit = "hours")) %>%
      dplyr::group_by(.data$lon, .data$lat, .data$StartTime) %>%
      dplyr::summarise(Area = sum(.data$Area, na.rm = TRUE),
                DQF = paste(unique(.data$DQF), collapse = ";"),
                Mask = paste(unique(.data$Mask), collapse = ";"),
                Power = mean(.data$Power, na.rm = TRUE),
                Temp = mean(.data$Temp, na.rm = TRUE),
                FRE = sum(.data$FRE, na.rm = TRUE),
                TPM = sum(.data$TPM, na.rm = TRUE)) %>%
      dplyr::ungroup()

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
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples emissions <- feer_emissions(fires, feer)
feer_emissions <- function(fires, feer) {

  # FEER is on a 1x1 degree grid, centered on half degrees. Can join on whole degrees.
  feer <- dplyr::mutate(feer, lon1d = floor(.data$Longitude),
                 lat1d = floor(.data$Latitude)) %>%
    dplyr::select(.data$lon1d, .data$lat1d, .data$Ce_850, .data$QA_850)

  fires <- dplyr::mutate(fires, lon1d = floor(.data$lon),
                  lat1d = floor(.data$lat)) %>%
    dplyr::left_join(feer, by = c("lon1d", "lat1d")) %>%
    dplyr::mutate(Seconds = as.double(.data$EndTime - .data$StartTime, units = "secs"),
           FRE = .data$Power * .data$Seconds, # MW * s = MJ
           Ce_850 = dplyr::if_else(is.na(.data$Ce_850), 0.0159, .data$Ce_850),
           TPM = .data$FRE * .data$Ce_850) # MJ * kg/MJ = kg TPM


}
