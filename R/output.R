
#' Create SMOKE-ready files (ems95 and ida) for use in SMOKE-CMAQ
#'
#' This takes fully processed hourly files, including PM2.5 emissions and plume top
#' estimations and produces the files needed for the SMOKE preprocessor for CMAQ. This
#' includes estimating emissions of other species (PM10, CO, NH3, NOX, SO2, VOC) based on
#' emission ratios to PM2.5, estimated from BlueSky output. The PM2.5 emissions are
#' assumed to be in kg, which is what is produced by \code{\link{feer_emissions}}.
#'
#' See
#' \href{https://forum.cmascenter.org/t/hourly-fire-emissions-in-smoke-with-pre-computed-plume-rise/311}{this
#' thread} for some useful information on the formats, including strange required quirks.
#' PTOP and PBOT are in meters, emissions are in short tons, LAY1F cannot be zero, and
#' PTOP > PBOT.
#'
#'
#' @param fires data frame of hourly fire emissions as produced by
#'   \code{\link{feer_emissions}}
#' @param path output path for files
#'
#' @return data frame of hourly fire emissions converted to units required by SMOKE. As a
#'   side effect, writes the ida and ems95 files to the specified path.
#' @export
#'
#' @examples
output_SMOKE <- function(fires, path) {

  # Add other species - these factors should be revisited
  ratios <- list(PM10 = 1.18, CO = 11.2, NH3 = 0.185,
                 NOX = 0.194, SO2 = 0.0969, VOC = 2.66)

  fires <- fires %>%
    dplyr::mutate(PM25 = PM25 * 0.00110231, # short tons per kg
                  PM10 = PM25 * ratios$PM10,
                  CO = PM25 * ratios$CO,
                  NH3 = PM25 * ratios$NH3,
                  NOX = PM25 * ratios$NOX,
                  SO2 = PM25 * ratios$SO2,
                  VOC = PM25 * ratios$VOC)

  # Need to estimate plume bottom and layer 1 fraction
  # Set plume bottom to 25% of plume top

  # No idea what to use for layer 1 fraction at the moment - this varies a lot in BlueSky,
  # and we need a method to estimate it.

  # Here is a method to assign more to the first layer for lower FRP
  lay1 <- function(FRP) {
    l <- -0.05*FRP + 55
    l[l > 60] <- 60
    l[l < 10] <- 10
    return(l)
  }

  fires <- fires %>%
    dplyr::mutate(PBOT = PlumeTop * 0.1,
                  LAY1F = lay1(Power))

  # Fire names must be <= 11 characters and contain no whitespace
  firenames <- fires %>%
    dplyr::select(Fire) %>%
    dplyr::distinct() %>%
    dplyr::mutate(FireName = make.names(Fire, unique = TRUE),
                  FireName = ifelse(nchar(FireName > 11), substr(FireName, 1, 11),
                                          FireName))
  fires <- fires %>%
    dplyr::inner_join(firenames, by = "Fire")

  # Create a unique id for each fire location
  locs <- fires %>%
    dplyr::select(FireName, lat, lon) %>%
    dplyr::distinct() %>%
    dplyr::group_by(FireName) %>%
    dplyr::mutate(PointId = paste(FireName, dplyr::row_number(), sep = "_")) %>%
    dplyr::ungroup()

  # Need to assign a state and county FIPS code for each location
  county <- assign_fips(locs, counties_sf)
  locs <- locs %>%
    dplyr::left_join(county, by = "PointId")
  fires <- fires %>%
    dplyr::left_join(locs, by = c("lon", "lat", "FireName"))

  # Create ptinv file
  make_ptinv(fires, path)

  # Create pthour file
  make_pthour(fires, path)

  return(fires)
}

assign_fips <- function(locs, counties = counties_sf) {

  # North America Albers
  proj <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  locs_sf <- sf::st_as_sf(locs, coords = c("lon", "lat"), crs = 4326) %>%
    sf::st_transform(crs = proj)


  suppressWarnings(sf::st_intersection(locs_sf, counties)) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(PointId, STATEFP, COUNTYFP)

}


make_ptinv <- function(df, path, scc = "28100010F0") {

  # Format described at
  # https://www.cmascenter.org/smoke/documentation/3.5/html/ch08s02s10.html#tbl_input_ptinv_ida

  # Pull year from data - needed for header
  # Also date needed for filename
  date_string <- as.character(min(df$Hour), format = "%Y%m%d%H")
  year <- substr(date_string, 1, 4)

  df <- df %>%
    dplyr::mutate(STID = STATEFP,
                  CYID = COUNTYFP,
                  PLANTID = PointId,
                  POINTID = "",
                  STACKID = 1,
                  Empty1 = "",
                  SCC = scc,
                  Empty2 = "",
                  LATC = lat,
                  LONC = lon) %>%
    dplyr::select(STID:LONC)

  df <- df %>%
    dplyr::mutate(STID = stringr::str_pad(STID, 2),
                  CYID = stringr::str_pad(CYID, 3),
                  PLANTID = stringr::str_pad(PLANTID, 15, "right"),
                  POINTID = stringr::str_pad(POINTID, 15, "right"),
                  STACKID = stringr::str_pad(STACKID, 12, "right"),
                  Empty1 = stringr::str_pad(Empty1, 54),
                  SCC = stringr::str_pad(SCC, 10),
                  Empty2 = stringr::str_pad(Empty2, 119),
                  LATC = stringr::str_pad(sprintf("%.4f", LATC), 8),
                  LONC = stringr::str_pad(sprintf("%.4f", LONC), 11)) %>%
    dplyr::transmute(Line = paste0(STID, CYID, PLANTID, POINTID, STACKID, Empty1, SCC,
                                   Empty2, LATC, LONC))

  # Write the header
  year_string <- paste("#YEAR", year, sep = " ")
  header <- paste("#IDA", "#PTINV", "#COUNTRY US", year_string,
                  "#DESC POINT SOURCE goesfire GOES-16 fire emissions",
                  "#DATA PM2_5 PM10 CO NH3 NOX SO2 VOC", sep = "\n")

  filename <- fs::path(path, paste0("ptinv-", date_string, ".ida"))
  outfile <- file(filename, "wb") # need to write binary to ensure unix newlines
  write(header, file = outfile)
  write.table(df, outfile, sep = "", row.name = FALSE, col.names = FALSE,
              quote = FALSE, eol = "\n", append = TRUE)
  close(outfile)

}

make_pthour <- function(df, path, scc = "28100010F0") {

  # Format described at
  # https://www.cmascenter.org/smoke/documentation/3.5/html/ch08s02s09.html#tbl_input_pthour_ems

  # Pull year from data - needed for header
  # Also date needed for filename
  date_string <- as.character(min(df$Hour), format = "%Y%m%d%H")
  year <- substr(date_string, 1, 4)

  df <- df %>%
    dplyr::mutate(STID = STATEFP,
                  CYID = COUNTYFP,
                  FCID = PointId,
                  SKID = "",
                  DVID = 1,
                  DATE = as.Date(Hour),
                  HOUR = paste0("Hr", sprintf("%02d", lubridate::hour(Hour))),
                  SCC = scc) %>%
    dplyr::select(STID, CYID, FCID, SKID, DVID, DATE, SCC, HOUR, PTOP = PlumeTop, PBOT, LAY1F,
                  PM2_5 = PM25, PM10, CO, NH3, NOX, SO2, VOC)

  # Transpose hours and parameters (LAY1F cannot be 0, so convert to 0.0001)
  df <- df %>%
    tidyr::gather("Parameter", "Value", PTOP:VOC) %>%
    tidyr::spread(key = HOUR, value = Value, fill = 0) %>%
    dplyr::arrange(DATE, FCID) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Hr", ignore.case = FALSE)),
                     .funs = dplyr::funs(. = ifelse(.data$Parameter == "LAY1F" & . == 0,
                                                    0.0001, .))) %>%
    dplyr::select(STID, CYID, FCID, SKID, DVID, Parameter, DATE, SCC,
                  dplyr::ends_with("_."))

  # Format for print
  df <- df %>%
    dplyr::mutate(STID = stringr::str_pad(STID, 2),
                  CYID = stringr::str_pad(CYID, 3),
                  FCID = stringr::str_pad(FCID, 15, "right"),
                  SKID = stringr::str_pad(SKID, 12, "right"),
                  DVID = stringr::str_pad(DVID, 12, "right"),
                  PRID = "            ",
                  POLID = stringr::str_pad(Parameter, 6, "right"),
                  DATE = paste0(format(DATE, format = "%m/%d/%y"), "GMT"),
                  DAYTOT = "         ") %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("_.")),
                     .funs = dplyr::funs(. = formatC(., width = 7, format = "G"))) %>%
    dplyr::transmute(Line = paste0(STID, CYID, FCID, SKID, DVID, PRID, POLID, DATE,
                                   Hr00_._., Hr01_._., Hr02_._., Hr03_._., Hr04_._.,
                                   Hr05_._., Hr06_._., Hr07_._., Hr08_._., Hr09_._.,
                                   Hr10_._., Hr11_._., Hr12_._., Hr13_._., Hr14_._.,
                                   Hr15_._., Hr16_._., Hr17_._., Hr18_._., Hr19_._.,
                                   Hr20_._., Hr21_._., Hr22_._., Hr23_._., DAYTOT, SCC))


  # Header
  year_string <- paste("#YEAR", year, sep = " ")
  header <- paste("#EMS-95", "#PTHOUR", "#COUNTRY US", year_string,
                  "#DESC POINT SOURCE goesfire GOES-16 fire emissions", sep = "\n")

  filename <- fs::path(path, paste0("pthour-", date_string, ".ems95"))
  outfile <- file(filename, "wb") # need to write binary to ensure unix newlines
  write(header, file = outfile)
  write.table(df, outfile, sep = "", row.name = FALSE, col.names = FALSE,
              quote = FALSE, eol = "\n", append = TRUE)
  close(outfile)

  return(df)

}
