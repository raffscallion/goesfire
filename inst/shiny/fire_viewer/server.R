
library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(gridExtra)
library(DT)
library(cowplot)

# Increase max file upload size to 30 mb
options(shiny.maxRequestSize = 30 * 1024^2)

shinyServer(function(input, output, session) {

  theme_set(theme_minimal())

  fires <- reactiveVal()
  fire_times <- reactiveVal()

  # Get start and end date
  observe({
    f <- input$files
    if (is.null(f)) {
      return(NULL)
    }

    df <- purrr::map_dfr(f$datapath, read_csv, col_types = cols()) %>%
      mutate(Area = if_else(is.na(Area), 0.001,
                            if_else(Area < 0.001, 0.001, Area)))
    fires(df)

    min_date <- min(df$StartTime)
    max_date <- max(df$StartTime)
    times <- group_by(df, StartTime) %>%
      summarise(Count = n()) %>%
      arrange(StartTime)

    steps <- nrow(times)
    fire_times(times)

    updateSliderInput(session, "timestep", min = 1, max = steps, step = 1)

  })

  output$time <- renderText({
    df <- filtered_fires()
    validate(need(nrow(df) > 0, ""))
    time <- slice(df, 1) %>%
      .$StartTime
    format(time, format = "%Y-%m-%d %H:%M %Z", tz = "UTC")
  })

  filtered_fires <- reactive({
    if (is.null(fire_times())) {
      return(NULL)
    }
    time <- slice(fire_times(), input$timestep)
    semi_join(fires(), time, by = "StartTime")
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Gray") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Physical") %>%
      fitBounds(-125, 20, -60, 55) %>%
      addFullscreenControl(pseudoFullscreen = TRUE) %>%
      addLayersControl(baseGroups = c("Gray", "NatGeo", "Imagery", "Physical"))
  })

  observe({
    df <- filtered_fires()
    time <- df$StartTime[1]
    validate(need(nrow(df) > 0, "No data"))
    leafletProxy("map", data = df) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addTerminator(time = time, options = pathOptions(fillOpacity = 0.2)) %>%
      addCircleMarkers(radius = ~sqrt(Area * 100), lng = ~lon,
                       lat = ~lat, popup = ~paste(Area), color = "#e20909")
  })

  output$bounds <- renderText({
    paste(input$map_bounds)
  })

  output$table <- renderDataTable({
    df <- filtered_fires()
    validate(need(nrow(df) > 0, "Select some files"))
    # Limit fires by bounds displayed on map
    bounds <- input$map_bounds
    df <- filter(df, lat <= bounds[1], lat >= bounds[3],
                 lon <= bounds[2], lon >= bounds[4])
    if ("Filename" %in% names(df)) {
      df <- select(df, -Filename)
    }
    d <- datatable(df, class = "compact",
              options = list(order = list(4, "desc"))) %>%
      formatRound(columns = ~ Power + Temp, digits = 1) %>%
      formatRound(columns = ~ lon + lat, digits = 2) %>%
      formatRound(columns = ~ Area, digits = 4)
    if ("TPM" %in% names(df)) {
      d <- d %>%
        formatRound(columns = ~ FRE + TPM, digits = 0)
    }
    d
  })

  output$ts <- renderPlot({
    df <- fires()
    validate(need(nrow(df > 0), "Select some files"))

    # Limit fires by bounds displayed on map
    bounds <- input$map_bounds
    df <- filter(df, lat <= bounds[1], lat >= bounds[3],
                 lon <= bounds[2], lon >= bounds[4])

    df <- group_by(df, StartTime) %>%
      summarise(TotalArea = sum(Area, na.rm = TRUE),
                TotalPower = sum(Power, na.rm = TRUE),
                FireCount = n())

    current <- filtered_fires() %>%
      slice(1) %>%
      .$StartTime

    g1 <- ggplot(df, aes(x = StartTime, y = FireCount)) + geom_line(colour = "red") +
      geom_vline(xintercept = current)
    g2 <- ggplot(df, aes(x = StartTime, y = TotalArea)) + geom_line(colour = "blue") +
      geom_vline(xintercept = current)
    g3 <- ggplot(df, aes(x = StartTime, y = TotalPower)) + geom_line(colour = "green") +
      geom_vline(xintercept = current)
    plot_grid(g1, g2, g3, align = "v", nrow = 3)

  })

  output$totals <- renderUI({
    df <- fires()
    if (is.null(df)) {
      return(NULL)
    }
    if (!"TPM" %in% names(df)) {
      return(NULL)
    }

    # Limit fires by bounds displayed on map
    bounds <- input$map_bounds
    df <- filter(df, lat <= bounds[1], lat >= bounds[3],
                 lon <= bounds[2], lon >= bounds[4])

    FRE <- paste0("<b>Total FRE: </b>",
                  formatC(sum(df$FRE, na.rm = TRUE), digits = 0, big.mark = ",",
                          format = "f"),
                  " MJ")
    tpm <- sum(df$TPM, na.rm = TRUE)
    TPM <- paste0("<b>Total PM: </b>",
                  formatC(tpm, digits = 0, big.mark = ",", format = "f"),
                  " kg (",
                  formatC(tpm * 0.0011023, digits = 0, big.mark = ",", format = "f"),
                  " tons)")

    HTML(paste(FRE, TPM, sep = "<br>"))

  })

  output$emissions <- renderPlot({
    df <- fires()
    validate(need(nrow(df > 0), "Select some files"))
    validate(need("TPM" %in% names(df), "No emissions in file"))

    # Limit fires by bounds displayed on map
    bounds <- input$map_bounds
    df <- filter(df, lat <= bounds[1], lat >= bounds[3],
                 lon <= bounds[2], lon >= bounds[4])

    df <- group_by(df, StartTime) %>%
      summarise(TotalFRE = sum(FRE, na.rm = TRUE),
                TotalTPM = sum(TPM, na.rm = TRUE))

    current <- filtered_fires() %>%
      slice(1) %>%
      .$StartTime

    g1 <- ggplot(df, aes(x = StartTime, y = TotalFRE)) + geom_line(colour = "red") +
      geom_vline(xintercept = current)
    g2 <- ggplot(df, aes(x = StartTime, y = TotalTPM)) + geom_line(colour = "blue") +
      geom_vline(xintercept = current)
    plot_grid(g1, g2, align = "v", nrow = 2)
  })

})
