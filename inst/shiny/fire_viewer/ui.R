
library(shiny)
library(leaflet)
library(DT)

shinyUI(fluidPage(

  title = "GEOS Fire Viewer",

  fileInput("files", label = "Select processed files", accept = ".csv", multiple = TRUE),
  fluidRow(
    column(width = 6,
           sliderInput("timestep", label = "Time step", min = 1, max = 1, value = 1,
                       width = "100%",
                       animate = animationOptions(interval = 700, loop = TRUE))),
    column(width = 2,
           textOutput("time"),
           htmlOutput("totals"))
  ),
  fluidRow(
    column(width = 6,
           leafletOutput("map"),
           plotOutput("ts")
           ),
    column(width = 6,
           plotOutput("emissions"),
           dataTableOutput("table"))
  )
))
