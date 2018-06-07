# Process files already downloaded

library(goesfire)
library(tidyverse)

ncdf_path <- "C:/Users/sraffuse/Google Drive/Working/JVA/GOES-R Fire/data/netcdf/"
output_path <- "C:/Users/sraffuse/Google Drive/Working/JVA/GOES-R Fire/data/csv/testing/"
outname <- "GOES_FDC_Test.csv"

files <- choose.files()

df <- map_dfr(files, goesfire::extract_fires, maskvals = c(10, 11, 30, 31))
write_csv(df, paste0(output_path, outname))
