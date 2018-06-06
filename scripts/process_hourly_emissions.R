# Calculate FEER emissions for all GOES FDC csv files in a folder and convert hourly totals

library(goesfire)
library(fs)
library(tidyverse)

input_path <- "C:/Users/sraffuse/Google Drive/Working/JVA/GOES-R Fire/data/csv/testing/"
output_path <- "C:/Users/sraffuse/Google Drive/Working/JVA/GOES-R Fire/data/csv/emissions/"
outname <- "GOES_Emissions.csv"

feer <- read_csv("../../data/feer/FEERv1.0_Ce.csv", skip = 6, na = "-9999")

files <- dir_ls(input_path, glob = "*.csv")

emissions <- map_dfr(files, read_csv, col_types = cols()) %>%
  goesfire::feer_emissions(feer) %>%
  aggregate_hourly()

# Write to csv
write_csv(emissions, paste0(output_path, outname))
