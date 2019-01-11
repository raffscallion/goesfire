
library(shiny)
library(DBI)
library(pool)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(plotly)

# Get the current TZ (for resetting when done)
initial_tz <- Sys.timezone()
Sys.setenv(TZ = "UTC")

# Connect to the db
if (.Platform$OS.type == "windows") {
  driver <- "SQL Server"
} else {
  driver <- "SQLServer"
}
pool <- dbPool(odbc::odbc(), Driver = driver, Database = "goes", UID = "[user_id]",
               PWD = "[password]", Server = "ucd-airfire.database.windows.net")

# Get min and max date
fires <- dplyr::tbl(pool, "fires")

date_range <- fires %>%
  summarise(date_min = min(StartTime, na.rm = TRUE),
            date_max = max(StartTime, na.rm = TRUE)) %>%
  collect()
date_end <- as.Date(date_range[[2]]) + 1
date_start <- date_end - 3
date_min <- as.Date(date_range[[1]])


palette <- leaflet::colorFactor("viridis", domain = c(10, 11, 12, 13, 14, 15, 30, 31,
                                                      32, 33, 34, 35))

source("model.R")

# Cleanup after close
onStop(function() {
  poolClose(pool)
  Sys.setenv(TZ = initial_tz)
})
