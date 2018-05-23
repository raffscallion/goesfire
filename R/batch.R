
#' download_and_process
#'
#' Given a filename, URL of a file available via ftp, and download path, it will download
#' and process the file if it does not already exist in the download path.
#'
#' @param filename A netcdf filename
#' @param url The ftp location of the file, including subdirectories
#' @param download_path The local download location
#' @param vars The variables to process (passed to \code{\link{extract_fires}}). By
#'   default, all per-pixel variables are processed.
#' @param maskvals If the "Mask" variable is included in vars, this is the list of mask
#'   values to include in the output. The default is c(10, 11, 30, 31). If not included,
#'   all pixels will be processed and the output will be very large. Passed to
#'   \code{\link{extract_fires}}
#'
#' @return A data frame of processed fire data, and the original file downloaded in
#'   download_path
#' @export
#'
#' @examples
download_and_process <- function(filename, url, download_path,
                                 vars = c("Temp", "Power", "Area", "DQF", "Mask"),
                                 maskvals = c(10, 11, 30, 31)) {

  # Only process the file if it doesn't exist locally (we've already done those)
  if (fs::file_exists(fs::path(download_path, filename))) {
    print(paste0("Skipping ", filename))
    return(NULL)
  } else {
    print(paste0("Processing ", filename))
    download.file(paste0(url, filename), paste0(download_path, filename), mode = "wb",
                  quiet = TRUE)
    infile <- fs::path(download_path, filename)
    df <- extract_fires(infile, vars = vars, maskvals = maskvals)
  }

}

#' batch_process
#'
#' Used for routine acquisition of GOES FDCC files from a CLASS subscription. The function
#' (really a script) will open an ftp location, find all of the netcdf FDCC files
#' (currently skipping FDCF), and process all of those files that are not already found in
#' the download_path. The new files will be downloaded and processed, with the results
#' concatenated into a sinlge csv output file
#'
#' @param url Subscription FTP location (e.g.,
#'   "ftp://ftp.class.ncdc.noaa.gov/sub/sraffuse1/52654/")
#' @param download_path Local path where netcdf files are stored
#' @param output_path Local path where output csv will be written
#' @param outname Name of output csv. The script will add a time stamp
#'
#' @return A data frame that matches the output csv
#' @export
#'
#' @examples df <- batch_process("ftp://ftp.class.ncdc.noaa.gov/sub/sraffuse1/52654/",
#'  "../../data/netcdf/", "../../data/csv/", "GOES_Fires")
batch_process <- function(url, download_path, output_path, outname) {

  filenames <- RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames <- unlist(strsplit(filenames, "\r\n"))
  # ncdf files only
  files <- filenames[grepl(glob2rx("*.nc"), filenames)]
  # CONUS only
  files <- files[grepl("FDCC", files)]

  df <- purrr::map_dfr(files, download_and_process, url, download_path)

  outname <- paste(outname, as.character(Sys.time(), format = "%Y%m%d-%H%M%S"))
  readr::write_csv(df, fs::path(output_path, outname, ext = "csv"))

  return(df)

}


url <- "ftp://ftp.class.ncdc.noaa.gov/sub/sraffuse1/52654/"
download_path <- "../../data/netcdf/"
output_path <- "../../data/csv/"
outname <- "GOES_Fires_day2"


