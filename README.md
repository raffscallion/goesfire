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
