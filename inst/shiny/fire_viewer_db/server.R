
shinyServer(function(input, output, session) {

  # Set the date range on session load
  output$date_range <- renderUI({

    date_end <- Sys.Date() + 1
    date_min <- min_fire_date
    date_start <- Sys.Date() - 5

    # Also update the time slider
    start <- as.POSIXct(date_start, tz = Sys.timezone())
    end <- as.POSIXct(date_end, tz = Sys.timezone())
    updateSliderInput(session, "datetimes", min = start, max = end, value = c(start, end))

    dateRangeInput("date_range", label = "Date Range", start = date_start,
                   end = date_end, min = date_min, max = date_end)


  })


  # Update the time slider when the date range is changed via button
  observeEvent(input$set_dates, {
    start <- as.POSIXct(isolate(input$date_range[1]), tz = Sys.timezone())
    end <- as.POSIXct(isolate(input$date_range[2]), tz = Sys.timezone()) + 24 * 60 * 60
    updateSliderInput(session, "datetimes", min = start, max = end, value = c(start, end))
  })

  # Some info in the about button
  observeEvent(input$about, {
    showModal(modalDialog(
      HTML(about_content),
      title = "About GoFAST",
      easyClose = TRUE
    ))
  })


  # Split reactives into three
  # 1. Filter by mask value and broad date range
  # 2a. Map filter by datetime
  # 2b. Plot filter by map bounds

  filtered_fires <- eventReactive(input$set_dates, {

    date_range <- isolate(input$date_range)

    if (input$source == "Best Available") {
      tbl_src <- fires_blended
    } else {
      tbl_src <- fires
    }

    source_filter <- switch(input$source,
                            "GOES-18" = "G18",
                            "GOES-19" = "G19",
                            NULL)

    df <- tbl_src %>%
      filter(Mask %in% !!input$masks,
             StartTime >= !!date_range[1],
             StartTime < !!(date_range[2] + 1))
    if (!is.null(source_filter)) {
      df <- df %>%
        filter(source == source_filter)
    }

    df <- df  %>%
      collect() %>%
      mutate(Label = glue::glue("({formatC(lon, digits = 6)}, {formatC(lat, digits = 5)})<br/>",
                                "{StartTime}<br/>",
                                "Source: {source}<br/>",
                                "Mask: {Mask}<br/>",
                                "Area: {formatC(Area, digits = 4)} km<sup>2</sup><br/>",
                                "Power: {formatC(Power, digits = 4)} MW<br/>",
                                "Temp: {formatC(Temp, digits = 4)} K<br/>",
                                "PM<sub>2.5</sub>: {formatC(PM25, digits = 4)} kg<br/>"))
  })

  filtered_polys <- eventReactive(input$set_dates, {

    date_range <- isolate(input$date_range)

    fires <- perimeters %>%
      filter(gofast_date_utc >= !!date_range[1],
             gofast_date_utc < !!(date_range[2] + 1)) %>%
      group_by(irwin_id) %>%
      filter(gofast_date_utc == max(gofast_date_utc, na.rm = TRUE)) %>%
      mutate(Shape = ST_AsText(shape)) %>%
      select(-shape) %>%
      collect()

    if (nrow(fires) > 0) {
      fires <- fires %>%
        sf::st_as_sf(wkt = "Shape", crs = 4326) %>%
        mutate(Label = paste(incident_name, perimeter_date_time_utc))
    } else {
      return(NULL)
    }

    return(fires)

  })

  map_data <- reactive({

    validate(need(!is.null(filtered_fires()), "Loading"))
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

    if (input$resolution != "5 minute") {
      if (input$resolution == "Hourly") {
        df <- mutate(df, StartTime = lubridate::floor_date(StartTime, "hours"))
      } else {
        df <- mutate(df, StartTime = lubridate::floor_date(StartTime, "days"))
      }
    }

    df <- group_by(df, StartTime) %>%
      summarise(FireCount = n(),
                TotalArea = sum(Area, na.rm = TRUE),
                TotalFRE = sum(FRE, na.rm = TRUE),
                TotalPM25 = sum(PM25, na.rm = TRUE),
                .groups = "drop")


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
    sf <- sf::st_as_sf(df, coords = c("lon", "lat"))

    d <- filter(df, st_within(sf, poly, sparse = FALSE))
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
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo", options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Gray", options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery", options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldTerrain, group = "Terrain", options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldPhysical, group = "Physical", options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "NatGeo", options = providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)) %>%
      setView(-98, 38, zoom = 5) %>%
      addFullscreenControl(pseudoFullscreen = TRUE) %>%
      addLayersControl(baseGroups = c("NatGeo", "Topo", "Gray", "Imagery", "Terrain", "Physical"),
                       overlayGroups = c("Perimeters", "GOES")) %>%
      hideGroup("Perimeters") %>%
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
    if ("Perimeters" %in% input$map_groups) {
      perims <- filtered_polys()
    } else {
      perims <- NULL
    }

    if (is.null(df)) return()
    time <- df$StartTime[1]
    lp <- leafletProxy("map", data = df) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      addTerminator(time = time, options = pathOptions(fillOpacity = 0.2))

    if (!is.null(perims)) {
      lp <- lp %>%
        addPolygons(data = perims, color = "#FF0000",
                    fillColor = "#555555", opacity = 1, weight = 2,
                    label = ~Label, group = "Perimeters")
    }

    lp <- lp %>%
       addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~Label, opacity = 1,
                       fillOpacity = 0.5, weight = 2, group = "GOES",
                       fillColor = ~palette(Mask), color = ~palette(Mask), radius = 4)

    if (nrow(df) > 0) {
      lp <- lp %>%
        addLegend("bottomright", pal = palette, values = ~Mask)
    }
    lp

  })

  output$fire_count <- renderPlotly({
    df <- plot_data()
    validate(need(nrow(df) > 0, "No fires"))
    ymin <- min(df$FireCount)
    ymax <- max(df$FireCount)
    title <- "Fire Pixel Count"
    time_series_plot(df, ~FireCount, ymin, ymax, title)
  })

  output$fire_area <- renderPlotly({
    df <- plot_data()
    validate(need(nrow(df) > 0, "No fires"))
    ymin <- min(df$TotalArea)
    ymax <- max(df$TotalArea)
    title <- "Fire Area (km<sup>2</sup>)"
    time_series_plot(df, ~TotalArea, ymin, ymax, title)
  })

  output$total_fre <- renderPlotly({
    df <- plot_data()
    validate(need(nrow(df) > 0, "No fires"))
    ymin <- min(df$TotalFRE)
    ymax <- max(df$TotalFRE)
    title <- "FRE (MJ)"
    time_series_plot(df, ~TotalFRE, ymin, ymax, title)
  })

  output$total_pm <- renderPlotly({
    df <- plot_data()
    validate(need(nrow(df) > 0, "No fires"))
    ymin <- min(df$TotalPM25)
    ymax <- max(df$TotalPM25)
    title <- "PM<sub>2.5</sub> (kg)"
    time_series_plot(df, ~TotalPM25, ymin, ymax, title)
  })



  time_series_plot <- function(df, y, ymin, ymax, title) {

    plot_ly(df) %>%
      add_lines(x = ~StartTime, y = y) %>%
      layout(shapes = list(list(type = "rect",
                                x0 = as.character(input$datetimes[1], tz = Sys.timezone()),
                                x1 = as.character(input$datetimes[2], tz = Sys.timezone()),
                                y0 = ymin,
                                y1 = ymax,
                                fillcolor = "rgb(240,248,255",
                                line = list(color = "rgb(240,248,255)"),
                                layer = "below")),
             xaxis = list(title = ""),
             yaxis = list(title = title, zeroline = FALSE))

  }

})

about_content <- "
The GOES Fire and Smoke Tool (GoFAST) is a viewer of the NOAA GOES-18 (GOES west) and GOES-19 (GOES east) satellite fire detections. The data are obtained from the University of Wisconsin Space Science and Engineering Center (SSEC). PM<sub>2.5</sub> is calculated using the NASA FEER algorithm (Ichoku and Ellison, 2014). Use the lasso tool to “lasso” a selection of fire detections and obtain csv files of fire PM<sub>2.5</sub> emissions estimated using FEER and allocated hourly according to the fire radiative energy (FRE) of the detections.  Also output are daily diurnal profiles of fire activity. Support was provided by the NASA Health and Air Quality Applied Sciences Team (HAQAST) program.
<br>
<br>

See also:
<br>
<a href='https://www.goes-r.gov/'>https://www.goes-r.gov</a>
<br>
<a href='https://wfabba.ssec.wisc.edu/index.html'>https://wfabba.ssec.wisc.edu</a>
<br>
<br>
Ichoku, C., & Ellison, L. (2014). Global top-down smoke-aerosol emissions estimation using satellite fire radiative power measurements. Atmospheric Chemistry and Physics, 14(13), 6643-6667.
<br>
O’Neill, S. M., Diao, M., Raffuse, S., Al-Hamdan, M., Barik, M., Jia, Y., ... & Loesche, P. (2021). A multi-analysis approach for estimating regional health impacts from the 2017 Northern California wildfires. Journal of the Air & Waste Management Association, 71(7), 791-814.
<br>
<br>
Developed by the <a href='https://airquality.ucdavis.edu/'>UC Davis Air Quality Research Center</a> and supported by the USDA Forest Service

"
