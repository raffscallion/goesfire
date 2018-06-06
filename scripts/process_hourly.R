# Convert all GOES FDC csv files in a folder to hourly averages

library(goesfire)
library(fs)
library(tidyverse)

input_path <- "C:/Users/sraffuse/Google Drive/Working/JVA/GOES-R Fire/data/csv/"
output_path <- "C:/Users/sraffuse/Google Drive/Working/JVA/GOES-R Fire/data/csv/hourly/"
outname <- "GOES_Hourly.csv"

files <- dir_ls(input_path, glob = "*.csv")

open_and_aggregate <- function(file) {
  read_csv(file, col_types = cols())
}

fires <- map_dfr(files, open_and_aggregate)
df <- goesfire::aggregate_hourly(fires)

# Write to csv
write_csv(df, paste0(output_path, outname))
