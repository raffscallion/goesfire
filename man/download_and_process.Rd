% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batch.R
\name{download_and_process}
\alias{download_and_process}
\title{download_and_process}
\usage{
download_and_process(filename, url, download_path, vars = c("Temp",
  "Power", "Area", "DQF", "Mask"), maskvals = c(10, 11, 30, 31),
  calc_emissions = FALSE, feer = NULL)
}
\arguments{
\item{filename}{A netcdf filename}

\item{url}{The ftp location of the file, including subdirectories}

\item{download_path}{The local download location}

\item{vars}{The variables to process (passed to \code{\link{extract_fires}}). By
default, all per-pixel variables are processed.}

\item{maskvals}{If the "Mask" variable is included in vars, this is the list of mask
values to include in the output. The default is c(10, 11, 30, 31). If not included,
all pixels will be processed and the output will be very large. Passed to
\code{\link{extract_fires}}}

\item{calc_emissions}{logical Calculate emissions (FRE and TPM) using FEER}

\item{feer}{The FEERv1.0_Ce.csv file loaded as a data frame. This can be acquired from
\url{https://feer.gsfc.nasa.gov/}}
}
\value{
A data frame of processed fire data, and the original file downloaded in
  download_path
}
\description{
Given a filename, URL of a file available via ftp, and download path, it will download
and process the file if it does not already exist in the download path.
}
