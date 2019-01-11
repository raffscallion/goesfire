
shinyServer(function(input, output, session) {

  # Update the time slider when the date range is changed
  observe({
    start <- as.POSIXct(input$date_range[1], tz = Sys.timezone())
    end <- as.POSIXct(input$date_range[2], tz = Sys.timezone()) + 24 * 60 * 60
    updateSliderInput(session, "datetimes", min = start, max = end, value = c(start, end))
  })

  # Split reactives into three
  # 1. Filter by mask value and broad date range
  # 2a. Map filter by datetime
  # 2b. Plot filter by map bounds

  filtered_fires <- reactive({
    if (is.null(input$masks)) return(NULL)

    fires %>%
      filter(Mask %in% input$masks,
             StartTime >= input$date_range[1],
             StartTime <= input$date_range[2]) %>%
      collect() %>%
      mutate(Label = glue::glue("({formatC(lon, digits = 6)}, {formatC(lat, digits = 5)})<br/>",
                                "{StartTime}<br/>",
                                "Mask: {Mask}<br/>",
                                "Area: {formatC(Area, digits = 4)} km<sup>2</sup><br/>",
                                "Power: {formatC(Power, digits = 4)} MW<br/>",
                                "Temp: {formatC(Temp, digits = 4)} K<br/>",
                                "PM2.5: {formatC(PM25, digits = 4)} kg<br/>"))
  }) %>%
    debounce(millis = 1000)

  map_data <- reactive({

    start <- strftime(input$datetimes[1], format = "%Y-%m-%d %T")
    end <- strftime(input$datetimes[2], format = "%Y-%m-%d %T")

    filtered_fires() %>%
      filter(StartTime > start,
             StartTime < end)

  })

  plot_data <- reactive({
    df <- filtered_fires()
    validate(need(nrow(df > 0), "No fires"))

    # Limit fires by map bounds
    bounds <- input$map_bounds
    if (!is.null(bounds)) {
      df <- filter(df, lat <= bounds[1], lat >= bounds[3],
                   lon <= bounds[2], lon >= bounds[4])
    }

    df <- group_by(df, StartTime) %>%
      summarise(FireCount = n(),
                TotalArea = sum(Area, na.rm = TRUE),
                TotalPM25 = sum(PM25, na.rm = TRUE))


  })

  model_data <- reactive({

    start <- strftime(input$datetimes[1], format = "%Y-%m-%d %T")
    end <- strftime(input$datetimes[2], format = "%Y-%m-%d %T")
    bounds <- input$map_bounds

    df <- filtered_fires() %>%
      filter(StartTime > start,
             StartTime < end)

    if (!is.null(bounds)) {
      df <- filter(df, lat <= bounds[1], lat >= bounds[3],
                   lon <= bounds[2], lon >= bounds[4])
    }

    return(df)
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Gray") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Physical") %>%
      fitBounds(-125, 20, -60, 55)
  })

  observe({
    df <- map_data()
    if (is.null(df)) return()
    time <- df$StartTime[1]
    lp <- leafletProxy("map", data = df) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addTerminator(time = time, options = pathOptions(fillOpacity = 0.2)) %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~Label, opacity = 1,
                       fillOpacity = 0.5, weight = 2,
                       fillColor = ~palette(Mask), color = ~palette(Mask), radius = 4) %>%
      addFullscreenControl(pseudoFullscreen = TRUE) %>%
      addLayersControl(baseGroups = c("Gray", "NatGeo", "Imagery", "Physical"))
    if (nrow(df) > 0) {
      lp <- lp %>%
        addLegend("bottomright", pal = palette, values = ~Mask)
    }
    lp

  })

  # Switched to native plotly over ggplotly because of poor support for geom_rect
  output$fire_count <- renderPlotly({
    df <- plot_data()
    validate(need(nrow(df > 0), "No fires"))

    plot_ly(df) %>%
      add_lines(x = ~StartTime, y = ~FireCount) %>%
      layout(shapes = list(list(type = "rect",
                                x0 = as.character(input$datetimes[1], tz = Sys.timezone()),
                                x1 = as.character(input$datetimes[2], tz = Sys.timezone()),
                                y0 = min(df$FireCount),
                                y1 = max(df$FireCount),
                                fillcolor = "rgb(240,248,255",
                                line = list(color = "rgb(240,248,255)"),
                                layer = "below")),
             xaxis = list(title = ""),
             yaxis = list(title = "Fire Pixel Count", zeroline = FALSE))

  })

  output$fire_area <- renderPlotly({
    df <- plot_data()
    validate(need(nrow(df > 0), "No fires"))

    plot_ly(df) %>%
      add_lines(x = ~StartTime, y = ~TotalArea) %>%
      layout(shapes = list(list(type = "rect",
                                x0 = as.character(input$datetimes[1], tz = Sys.timezone()),
                                x1 = as.character(input$datetimes[2], tz = Sys.timezone()),
                                y0 = min(df$TotalArea),
                                y1 = max(df$TotalArea),
                                fillcolor = "rgb(240,248,255",
                                line = list(color = "rgb(240,248,255)"),
                                layer = "below")),
             xaxis = list(title = ""),
             yaxis = list(title = "Fire Area (km2)", zeroline = FALSE))

  })

  output$total_pm <- renderPlotly({
    df <- plot_data()
    validate(need(nrow(df > 0), "No fires"))

    plot_ly(df) %>%
      add_lines(x = ~StartTime, y = ~TotalPM25) %>%
      layout(shapes = list(list(type = "rect",
                                x0 = as.character(input$datetimes[1], tz = Sys.timezone()),
                                x1 = as.character(input$datetimes[2], tz = Sys.timezone()),
                                y0 = min(df$TotalPM25),
                                y1 = max(df$TotalPM25),
                                fillcolor = "rgb(240,248,255",
                                line = list(color = "rgb(240,248,255)"),
                                layer = "below")),
             xaxis = list(title = ""),
             yaxis = list(title = "PM2.5 (kg)", zeroline = FALSE))

  })

  ### A button for creating a BlueSky time profile files
  model_vals <- reactiveValues(data = NULL)

  dataModal <- function(failed = FALSE) {
    modalDialog(
      textInput("fire_name", "Enter a name"),
      numericInput("fire_size", "Enter the final size in acres", value = 1000, step = 100),
      footer = tagList(
        modalButton("Close"),
        downloadButton("download", "Download")
      )
    )
  }

  observeEvent(input$model, {
    showModal(dataModal())
  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$fire_name, "_", strftime(Sys.time(), format = "%Y%m%d_%H%M%S"), ".tgz")

    },
    content = function(file) {
      model_inputs(file, model_data(), input$fire_name, input$fire_size)
    }
  )



})
