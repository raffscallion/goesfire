# Process files already downloaded

library(goesfire)
library(fs)
library(tidyverse)

ncdf_path <- "C:/Users/sraffuse/Google Drive/Working/JVA/GOES-R Fire/data/netcdf/"
output_path <- "C:/Users/sraffuse/Google Drive/Working/JVA/GOES-R Fire/data/csv/testing/"
outname <- "GOES_FDC_"

files <- dir_ls(ncdf_path, glob = "*.nc")

# This will explode memory if too large, so break into managable chunks of 20
files <- split(files, ceiling(seq_along(files) / 20))
i <- 1
for (fl in files) {
  print(i)
  df <- map_dfr(fl, goesfire::extract_fires, maskvals = c(10, 11, 30, 31))
  # Write to csv
  write_csv(df, paste0(output_path, outname, sprintf("%03d", i), ".csv"))
  i <- i + 1
}



