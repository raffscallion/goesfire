
shinyServer(function(input, output, session) {

  # Update the time slider when the date range is changed via button
  observeEvent(input$set_dates, {
    start <- as.POSIXct(isolate(input$date_range[1]), tz = Sys.timezone())
    end <- as.POSIXct(isolate(input$date_range[2]), tz = Sys.timezone()) + 24 * 60 * 60
    updateSliderInput(session, "datetimes", min = start, max = end, value = c(start, end))
  })

  # Split reactives into three
  # 1. Filter by mask value and broad date range
  # 2a. Map filter by datetime
  # 2b. Plot filter by map bounds

  filtered_fires <- reactive({
    if (is.null(input$masks)) return(NULL)

    # Don't fire if dates are changed unless button is pressed
    input$set_dates
    date_range <- isolate(input$date_range)

    fires %>%
      filter(Mask %in% !!input$masks,
             StartTime >= !!date_range[1],
             StartTime < !!(date_range[2] + 1)) %>%
      collect() %>%
      mutate(Label = glue::glue("({formatC(lon, digits = 6)}, {formatC(lat, digits = 5)})<br/>",
                                "{StartTime}<br/>",
                                "Mask: {Mask}<br/>",
                                "Area: {formatC(Area, digits = 4)} km<sup>2</sup><br/>",
                                "Power: {formatC(Power, digits = 4)} MW<br/>",
                                "Temp: {formatC(Temp, digits = 4)} K<br/>",
                                "PM2.5: {formatC(PM25, digits = 4)} kg<br/>"))
  })

  map_data <- reactive({

    start <- strftime(input$datetimes[1], format = "%Y-%m-%d %T")
    end <- strftime(input$datetimes[2], format = "%Y-%m-%d %T")

    df <- filtered_fires() %>%
      filter(StartTime > start,
             StartTime < end)

    # If map data are too large, reduce to only the most recent at each location - not
    # sure yet what too large is but this seems ok
    if (nrow(df) > 30000) {
      df <- group_by(df, lon, lat) %>%
        arrange(desc(StartTime)) %>%
        slice(1)
    }
    return(df)


  })

  plot_data <- reactive({
    df <- filtered_fires()
    validate(need(nrow(df > 0), "No fires"),
             need(!is.null(input$map_bounds), "Waiting for map"))

    # Limit fires by map bounds
    bounds <- input$map_bounds
    if (!is.null(bounds)) {
      df <- filter(df, lat <= bounds[1], lat >= bounds[3],
                   lon <= bounds[2], lon >= bounds[4])
    }

    df <- group_by(df, StartTime) %>%
      summarise(FireCount = n(),
                TotalArea = sum(Area, na.rm = TRUE),
                TotalFRE = sum(FRE, na.rm = TRUE),
                TotalPM25 = sum(PM25, na.rm = TRUE))


  })

  # Model data is selected by a lasso tool on the map (a custom JS plugin)
  plugin_lasso <- htmlDependency("leaflet-lasso", "2.0.4",
                                 src = "./www",
                                 script = "leaflet-lasso.umd.min.js")
  registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }

  model_data <- reactiveVal(NULL)

  observeEvent(input$lasso, {

    # Convert points to polygon and intersect with points
    pts <- input$lasso

    # lasso comes back with the latitudes before the longitudes so we need to switch
    lats <- pts[which(names(pts)=="lat")]
    lons <- pts[which(names(pts)=="lng")]
    pts_mat <- matrix(c(lons, lats), ncol = 2)

    # Need to replicate first point at the end to get a closed multilinestring
    pts_closed <- rbind(pts_mat, pts_mat[1,])
    mls <- st_multilinestring(list(pts_closed))

    # convert multilinestring to polygon
    poly <- st_polygonize(mls)

    # Now that we have a polygon, we can intersect the data to find what is inside - start
    # with a subset before converting to geometries
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

    # Now, convert this subset data to sf and find points within
    sf <- st_as_sf(df, coords = c("lon", "lat"))

    d <- dplyr::filter(df, st_within(sf, poly, sparse = FALSE))
    model_data(d)

    showModal(modalDialog(
      paste(nrow(d), "fire pixels selected"),
      textInput("fire_name", "Enter a name"),
      numericInput("fire_size", "Enter the final size in acres", value = 1000, step = 100),
      radioButtons("fire_type", "Fire Type", choices = c("WF", "RX"), selected = "WF"),
      selectInput("tz", "Time Zone",
                  choices = c("Pacific"="America/Los_Angeles",
                              "Mountain"="America/Denver",
                              "Central"="America/Chicago",
                              "Eastern"="America/New_York")),
      footer = tagList(
        modalButton("Close"),
        downloadButton("download", "Download")
      )
    ))

  })

  output$download <- downloadHandler(
    filename = function() {
      paste0(input$fire_name, "_", strftime(Sys.time(), format = "%Y%m%d_%H%M%S"), ".tgz")

    },
    content = function(file) {
      model_inputs(file, model_data(), input$fire_name, input$fire_size, input$fire_type,
                   input$tz)
    }
  )

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Gray", options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo", options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery", options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Physical", options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)) %>%
      setView(-98, 38, zoom = 5) %>%
      registerPlugin(plugin_lasso) %>%
      # Add lasso control
      onRender("function (el, x) {
                  L.control.lasso().addTo(this);
               }"
      ) %>%
      # Listen for the event when the lasso is done and get the envelope
      onRender("
               function(el, x) {
                  var myMap = this;
                  myMap.on('lasso.finished', function(el, x)
                  {
                    debugger;
                    Shiny.setInputValue('lasso', el.latLngs);
                  });
               }")
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
      addLayersControl(baseGroups = c("Gray", "Imagery", "Topo", "Terrain", "NatGeo", "Physical"))

    if (nrow(df) > 0) {
      lp <- lp %>%
        addLegend("bottomright", pal = palette, values = ~Mask)
    }
    lp

  })

  # Switched to native plotly over ggplotly because of poor support for geom_rect
  output$fire_count <- renderPlotly({
    df <- plot_data()
    validate(need(nrow(df) > 0, "No fires"))

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
    validate(need(nrow(df) > 0, "No fires"))

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

  output$total_fre <- renderPlotly({
    df <- plot_data()
    validate(need(nrow(df) > 0, "No fires"))

    plot_ly(df) %>%
      add_lines(x = ~StartTime, y = ~TotalFRE) %>%
      layout(shapes = list(list(type = "rect",
                                x0 = as.character(input$datetimes[1], tz = Sys.timezone()),
                                x1 = as.character(input$datetimes[2], tz = Sys.timezone()),
                                y0 = min(df$TotalFRE),
                                y1 = max(df$TotalFRE),
                                fillcolor = "rgb(240,248,255",
                                line = list(color = "rgb(240,248,255)"),
                                layer = "below")),
             xaxis = list(title = ""),
             yaxis = list(title = "FRE (MJ)", zeroline = FALSE))

  })

  output$total_pm <- renderPlotly({
    df <- plot_data()
    validate(need(nrow(df) > 0, "No fires"))

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

})
