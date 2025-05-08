
library(shiny)
library(DBI)
library(pool)
library(odbc)
library(dplyr)
library(dbplyr)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(shinycssloaders)
library(lubridate)
library(imputeTS)
library(htmltools)
library(htmlwidgets)
library(sf)

# # For testing only
# library(reactlog)
# options(shiny.reactlog = TRUE)

# Get the current TZ (for resetting when done)
initial_tz <- Sys.timezone()
Sys.setenv(TZ = "UTC")

# Connect to the db
if (.Platform$OS.type == "windows") {
  driver <- "PostgreSQL Unicode(x64)"
} else if (.Platform$OS.type == "unix") {
  # When using ODBC, ensure you have drivers installed locally
  # For linux install the unixODBC library || apt-get install unixodbc unixodbc-dev odbc-postgresql
  # For mac use brew or tarball of choice
  driver <- "PostgreSQL Unicode"
} else {
  driver <- "WHATEVER DRIVER YOU'RE USING FOR POSTGRES"
}

pool <- dbPool(odbc::odbc(), Driver = driver, Database = "airfire", UID = "user",
               PWD = "pwd",
               Server = "postgresql.cx7b6gvf3kxs.us-west-2.rds.amazonaws.com",
               port = 5432)

# Load table references
perimeters <- tbl(pool, in_schema("fire_info", "gofast_fire_perimeters_vw"))
# New views for G16 and G18
fires <-tbl(pool, in_schema("fire_info", "gofast_vw"))
fires_blended <-tbl(pool, in_schema("fire_info", "gofast_blended_vw"))

# Get the minimum fire date
min_fire_date <- fires %>%
  summarise(date_min = min(StartTime, na.rm = TRUE)) %>%
  collect() %>%
  .$date_min

palette <- leaflet::colorFactor("viridis", domain = c(10, 11, 12, 13, 14, 15,
                                                      30, 31, 32, 33, 34, 35))
source("model.R")

# Cleanup after close
onStop(function() {
  poolClose(pool)
  Sys.setenv(TZ = initial_tz)
})
