# goesfire
Utilities for processing GOES-ABI FDC data in R

### Installation
To install the latest development builds from GitHub:
```
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("raffscallion/goesfire")
```
### Processing netCDF files 
To turn a single FDCC or FDCF netCDF file into a data frame of hot spots, run:
```
fires <- extract_fires(input_file, maskvals = c(10, 11, 30, 31))
```
Note that the mask values selected here are only those most likely to be fire. Running `extract_fires()` without specifying maskvals will return a record for each grid cell (much larger than when only fire mask values are specified). This can be used to get information on the mask values.  

### CLASS subscription processing

To facilitate routine processing of data from a CLASS subscription, something like this will work:
```
library(goesfire)

url <- "ftp://ftp.avl.class.noaa.gov/sub/[username]/[subscription_number]/"
download_path <- "local path for downloading netcdf files"
output_path <- "local path for processed csv files"
outname <- "GOES_FDC" # this will get prepended to the output filenames
fires <- batch_process(url, download_path, output_path, outname)
```

This will download all FDCC netCDF files available in the subscription folder that do not already exist in the download path, process them into csv format, and write a single concatenated csv file to the output path.

### Viewing results

Once some fires have been processed to csv, you can use the viewer to look at them.
```
goesfire::run_viewer()
```
To use the viewer, select a list of processed csv files and they will be displayed on the map and time series.


### Emissions processing

An estimate of total particulate matter emissions can be calculated by using the emission coefficients available from [NASA FEER](https://feer.gsfc.nasa.gov/). The code has been tested with the FEERv1.0_Ce.csv file acquired from the FEER website. In this example, a folder of FDC csv files produced with `extract_fires()` is concatenated into a single output csv with calculated emissions. The emissions files can also be explored with the viewer.

```
library(goesfire)
library(fs)
library(tidyverse)

input_path <- "C:/my_csv_path/"
output_path <- "C:/my_emissions_path/"
outname <- "GOES_Emissions.csv"

# The first 6 lines are descriptive header and -9999 is the fill value
feer <- read_csv("/path_to_feer/FEERv1.0_Ce.csv", skip = 6, na = "-9999")

files <- dir_ls(input_path, glob = "*.csv")

emissions <- map_dfr(files, read_csv, col_types = cols()) %>%
  goesfire::feer_emissions(feer) %>%
  aggregate_hourly()

# Write to csv
write_csv(emissions, paste0(output_path, outname))
```

### Hourly aggregation

To aggregate the sub-hourly data, use the `aggregate_hourly()` function.
```
hourly <- extract_fires(infile, vars = vars, maskvals = maskvals) %>%
  aggregate_hourly()
```
