
shinyUI(fluidPage(

  title = "GEOS Fire Viewer",

  fluidRow(
    column(width = 2,
           dateRangeInput("date_range", label = "Date Range", start = date_start,
                          end = date_end, min = date_min, max = date_end)),
    column(width = 8, offset = 1,
           sliderInput("datetimes", label = "Times (UTC)",
                       min = date_range[[2]] - 60 * 60 * 24 * 3, # 3 days
                       max = date_range[[2]], value = c(date_range[[1]], date_range[[2]]),
                       width = "100%", step = 60 * 5, animate = TRUE,
                       timezone = "+0000"))
  ),
  fluidRow(
    column(width = 9,
           leafletOutput("map", height = "500px")),
    column(width = 3,
           checkboxGroupInput("masks", label = "Mask Values",
                              choiceValues = c(10, 11, 12, 13, 14, 15, 30, 31, 32,
                                               33, 34, 35),
                              selected = c(10, 11, 30, 31),
                              choiceNames = c("10 - Good",
                                              "11 - Saturated",
                                              "12 - Cloud Contaminated",
                                              "13 - High Probability",
                                              "14 - Medium Probability",
                                              "15 - Low Probability",
                                              "30 - TF Good",
                                              "31 - TF Saturated",
                                              "32 - TF Cloud Contaminated",
                                              "33 - TF High Prob.",
                                              "34 - TF Medium Prob.",
                                              "35 - TF Low Prob.")),
           actionButton("model", "Model Inputs"))
  ),
  fluidRow(
    column(width = 3,
           plotlyOutput("fire_count", height = "300px")),
    column(width = 3,
           plotlyOutput("fire_area", height = "300px")),
    column(width = 3,
           plotlyOutput("total_fre", height = "300px")),
    column(width = 3,
           plotlyOutput("total_pm", height = "300px"))
  )
))
