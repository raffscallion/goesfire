

#' Calculate hourly plume top heights using the method of Sofiev et al (2012) and met data
#' from WRF
#'
#' @param fires data frame of fire locations produced by get_pthourly_frp
#' @param met_path path to wrf met files
#'
#' @return
#' @export
#'
#' @examples
calculate_plume_top <- function(fires, met_path) {

  # Which met files do we need? Add that info to the fires df
  fires <- assign_met_files(fires, met_path)

  # Process plume rise, grouping hours by needed met file
  met_files <- dplyr::distinct(dplyr::select(fires, MetFile))

  nested <- dplyr::group_by(fires, MetFile) %>%
    tidyr::nest()

  result <- purrr::map2_dfr(nested$data, nested$MetFile, calculate_sofiev)

}


#' Assign each hour in the fire list with a specific met file and hour
#'
#' @param fires A data frame of hourly fire information to process with Hour in UTC
#' @param met_path A path to WRF netcdf files with the parameters needed for Sofiev (PBLH,
#'   PH, PHB, T, HGT, XLAT, XLONG)
#'
#' @return Returns the same data frame of fires with a filename of met data attached
#' @export
#'
#' @examples
assign_met_files <- function(fires, met_path) {

  # All of the hours in the fires we want to process
  hours <- dplyr::distinct(dplyr::select(fires, Hour))

  # Hours available in the met files
  met_files <- fs::dir_ls(met_path, glob = "*.nc")
  met_hours <- purrr::map_dfr(met_files, get_wrf_hours)

  # For each hour in fires we want to process, assign the file we need
  dplyr::left_join(fires, met_hours, by = "Hour")


}

get_wrf_hours <- function(file) {

  nc <- ncdf4::nc_open(file)
  hours <- as.POSIXct(ncvar_get(nc, "Times"), tz = "UTC", format = "%F_%T")
  dplyr::tibble(MetFile = file,
                Hour = hours)

}


calculate_sofiev <- function(fires, met_file) {

  nc <- ncdf4::nc_open(met_file)

  # Find the coordinates we need
  xlat <- ncdf4::ncvar_get(nc, "XLAT")[,,1]
  xlon <- ncdf4::ncvar_get(nc, "XLONG")[,,1]
  resolution <- ncdf4::ncatt_get(nc, 0)$DX

  # Only need the unqie lat/lons
  coords <- dplyr::distinct(dplyr::select(fires, lon, lat))
  print("Extracting grid indices")
  indices <- purrr::map2_dfr(coords$lon, coords$lat, get_grid_index, xlon, xlat, resolution)
  ind_unique <- dplyr::distinct(dplyr::select(indices, i, j)) %>%
    dplyr::arrange(i, j) %>%
    dplyr::mutate(loc_id = dplyr::row_number())
  indices <- dplyr::left_join(indices, ind_unique, by = c("i", "j"))
  coords <- dplyr::left_join(coords, indices, by = c("lat", "lon"))

  # Subset of the values we need converted from arrays to lists

  # This approach is slow, but functional

  ## Terrain height came out wrong... try reversing i and j
  extract_var <- function(i, j, var, dims) {
    if (dims == 3) {
      ncdf4::ncvar_get(nc, var)[j, i,]
    } else if (dims == 4) {
      ncdf4::ncvar_get(nc, var)[j, i,,]
    }
  }

  # ## This is kinda slow, so use an archived version for testing
  # N_FT2 <- readRDS("nft2.RDS")

  # Lists are each location - sublist is each hour
  print("Extracting planetary boundary layer height")
  pblh <- purrr::map2(ind_unique$i, ind_unique$j, extract_var, var = "PBLH", dims = 3) %>%
    purrr::map(purrr::array_branch)
  print("Extracting terrain height")
  height <- purrr::map2(ind_unique$i, ind_unique$j, extract_var, var = "HGT", dims = 3) %>%
    purrr::map(purrr::array_branch)

  print("Extracting base geopotential height")
  phb <- purrr::map2(ind_unique$i, ind_unique$j, extract_var, var = "PHB", dims = 4) %>%
    purrr::map(purrr::array_branch, margin = 2)
  print("Extracting perturbation geopotential height")
  ph <- purrr::map2(ind_unique$i, ind_unique$j, extract_var, var = "PH", dims = 4) %>%
    purrr::map(purrr::array_branch, margin = 2)
  print("Extracting perturbation potential temperature")
  tpot <- purrr::map2(ind_unique$i, ind_unique$j, extract_var, var = "T", dims = 4) %>%
    purrr::map(purrr::array_branch, margin = 2)

  # For each location, for each hour, calculate brunt-vaisala
  print("Calculating Brunt-Vaisala frequency")
  N_FT2 <- purrr::pmap(list(pblh, height, ph, phb, tpot), brunt_vaisala_hourly)

  # For each location, for each hour, return the PBLH
  hours <- ncdf4::ncvar_get(nc, "Times")

  # Reshape the data and assign fire coordinates for joining with fires

  print("Matching with fires")

  N_FT2_df <- unpack_lists(N_FT2, "N_FT2", hours) %>%
    dplyr::inner_join(coords, by = "loc_id")

  pblh_df <- unpack_lists(pblh, "PBLH", hours)

  # we'd also like the terrain height to compute plume rise agains mean sea level
  height_df <- unpack_lists(height, "TerrainHeight", hours)

  fires <- dplyr::left_join(fires, N_FT2_df, by = c("lat", "lon", "Hour")) %>%
    dplyr::left_join(pblh_df, by = c("loc_id", "Hour")) %>%
    dplyr::left_join(height_df, by = c("loc_id", "Hour"))

  # Finally have everything we need for the Sofiev plume rise calculation
  dplyr::mutate(fires, PlumeTop = purrr::pmap_dbl(list(PBLH, N_FT2, Power),
                                                  sofiev_plume_top))

}

# This wacky function turns our list of lists into the dataframe we want
unpack_lists <- function(l, varname, hours) {
  df <- l %>%
    tibble::enframe(name = "loc_id", value = "hourly") %>%
    dplyr::mutate(hourly = purrr::map(hourly, purrr::set_names, nm = hours)) %>%
    dplyr::mutate(d = purrr::invoke_map(dplyr::tibble, hourly)) %>%
    tidyr::unnest(d, .drop = TRUE) %>%
    tidyr::gather(key = "Hour", value = "value", -loc_id) %>%
    dplyr::mutate(Hour = as.POSIXct(Hour, tz = "UTC", format = "%F_%T"))
  colnames(df)[colnames(df) == "value"] <- varname
  return(df)
}

# for a coordinate pair (lon, lat), find the grid index using the XLAT and XLONG vars
get_grid_index <- function(x, y, xlon, xlat, resolution = 4000) {
  dists <- raster::pointDistance(c(x, y), cbind(as.vector(xlon), as.vector(xlat)),
                                  lonlat = TRUE, r = 6370*1e3)
  dists <- matrix(dists, ncol = ncol(xlon), nrow = nrow(xlon))
  if (min(dists) > resolution) {
    warning("Coordinate is off the grid!")
    return(NULL)
  }
  gridcell <- which(dists == min(dists), arr.ind = TRUE)
  dplyr::tibble(i = gridcell[2],
                j = gridcell[1],
                lon = x,
                lat = y)

}

# Calculate brunt-vaisala frequency for a list of conditions (hours)
brunt_vaisala_hourly <- function(pblh, height, ph, phb, tpot) {

  purrr::pmap(list(pblh, height, ph, phb, tpot), brunt_vaisala_squared)

}

# Calculate a single brunt-vaisala frequency at twice the PBL
brunt_vaisala_squared <- function(pblh, height, ph, phb, tpot) {

  brunt_height <- pblh * 2
  hgt_geo <- (ph + phb) / 9.81 - height

  dists <- abs(brunt_height - hgt_geo)
  layer <- which(dists == min(dists))

  # temp and height at extraction point (between layers)
  hgt_extract <- (hgt_geo[layer] + hgt_geo[layer + 1]) / 2

  # According to Yiqin total potential temperature in K is T + 300
  T_pot <- tpot[layer] + 300

  # Find the temperature and height for the layer below this
  hgt_below <- (hgt_geo[layer - 1] + hgt_geo[layer]) / 2
  T_below <- tpot[layer - 1] + 300

  # Writing this explicitly for clarity
  g <- 9.81 #m/s^2
  mean_Tpot <- (T_pot + T_below) / 2
  dT_dZ <- (T_pot - T_below) / (hgt_extract - hgt_below)

  # Need to use complex numbers because negative square root is valid here
  bvf <- sqrt((g / mean_Tpot) * as.complex(dT_dZ))

  # Convert back to real after squaring
  return(Re(bvf^2))

}

sofiev_plume_top <- function(H_abl, N2_FT, FRP, P_f0 = 1e6, N2_0 = 2.5e-4, alpha = 0.24,
                             beta = 170, gamma = 0.35, delta = 0.6) {

  # This equation requires FRP in W, but we store it in MW
  FRP <- FRP * 1e6
  H_p <- alpha * H_abl + beta * (FRP / P_f0)^gamma * exp(-delta * N2_FT / N2_0)

}
