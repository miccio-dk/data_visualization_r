## Map Viewer for Mobile Sensor Data Visualization in R
## Riccardo Miccini - 2017
## R shiny app - server side

# load necessary packages
library(shiny)
library(dplyr)
library(sp)
library(googlesheets)
library(leaflet)
library(plotly)


shinyServer(function(input, output, session) {
  # Google Sheet data
  gsheet_key <- "1qZiPVyEPLzjBmFguJLslwv5xIN00_t_cisOPk9PB1X4"
  gsheet_file <- gs_key(gsheet_key)
  gsheet_sheet <- "p1"
  
  # reactive expression for the raw data
  allData <- reactive({
    # reload every 2 seconds
    if(input$realtime) {
      invalidateLater(5000, session)
    }
    
    # read data with Google Sheet API
    data <- gs_read(gsheet_file, ws = gsheet_sheet, range = cell_cols("A:F"), literal = TRUE)
    
    # add POSIXct date-time column
    data$date_time_posix <- as.POSIXct(data$date_time, format = "%d/%m/%Y %H:%M:%S")
    return(data)
  })
  
  # reactive expression for the user-filtered data
  filteredData <- reactive({
    if(is.null(input$device_ids) || input$device_ids == "wait") {
      # get all data (if select box is empty)
      data <- allData()
    } else {
      # filter data
      data <- allData() %>%
        filter(device_id %in% input$device_ids) %>%
        filter(format(date_time_posix, "%Y-%m-%d") >= min(input$date)) %>%
        filter(format(date_time_posix, "%Y-%m-%d") < max(input$date))
    }
    
    # sort by device id and time
    data_sorted <- data %>% arrange(device_id, date_time)
    
    return(data_sorted)
  })
  
  # reactive expression for a colorblind-safe (up to 4 elements) palette, based on :
  # http://colorbrewer2.org/#type=qualitative&scheme=Paired&n=5
  col_palette <- reactive({
    colorFactor(
      palette = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99'),
      domain = filteredData()$device_id,
      ordered = TRUE
    )
  })
  
  
  # generates map obj and add pretty tiles
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(
        "OpenStreetMap.Mapnik", 
        options = providerTileOptions(noWrap = TRUE))
  })
  
  # generate plot obj
  output$plot <- renderPlotly({
    validate(need(length(input$device_ids) == 1, "Select a single device ID to show extra data"))
    
    plot_ly(filteredData()) %>%
      add_trace(
        x = ~date_time_posix,
        y = ~altitude,
        yaxis = "y",
        type = "bar",
        name = "altitude",
        hoverinfo = "text",
        text = ~paste(format(altitude, digits = 3), " m"),
        marker = list(color = col_palette()(input$device_ids)),
        opacity = 0.5) %>%
      add_trace(
        x = ~date_time_posix,
        y = ~speed,
        yaxis = "y2",
        type = "scatter", mode = "lines",
        name = "speed",
        hoverinfo = "text",
        text = ~paste(format(speed, digits = 2), " m/s"),
        line = list(color = col_palette()(input$device_ids))) %>%
      layout(
        title = "Speed and altitude of selected device",
        xaxis = list(title = ""),
        yaxis = list(
          side = "left", 
          title = "Altitude (m)", 
          showgrid = FALSE, 
          zeroline = FALSE),
        yaxis2 = list(
          overlaying = "y", 
          side = "right", 
          title = "Speed (m/s)", 
          showgrid = FALSE, 
          zeroline = FALSE),
        hovermode = "closest")
  })
  
  
  # observer for loading devices ids int select box
  observe({
    if(nrow(allData()) > 1) {
      # get unique list of ids
      lst_device_ids <- allData() %>% select(device_id) %>% unique()
      
      if(!is.null(input$device_ids) && input$device_ids == "wait") {
        # update widget content
        updateSelectInput(
          session, "device_ids", 
          choices = as.list(lst_device_ids), 
          selected = as.array(lst_device_ids$device_id))
        updateDateRangeInput(
          session, "date",
          start = min(format(as.POSIXct(allData()$date_time, format = "%d/%m/%Y"), "%Y-%m-%d")),
          end = max(format(as.POSIXct(allData()$date_time, format = "%d/%m/%Y"), "%Y-%m-%d")))
      }
    }
  })
    
  # observer for updating the map elements
  observe({
    # load user-filtered data
    data_filtered <- filteredData()
    
    if(nrow(data_filtered) > 0) {
      # generate unique list of ids
      data_unique_id <- unique(data_sorted$device_id)
      
      # generate list of individual paths based on unique ids
      lst_paths <- lapply(data_unique_id, function(id){
        # filter by device id
        data_sorted_id <- data_sorted %>% filter(device_id == id)
        # return Lines obj generated from a data matrix of [lng lan] 
        Lines(Line(data.matrix(data_sorted_id[c("lng", "lat")])), ID = id)
      })
      
      # spatial lines df object
      sldf_device_id <- data_unique_id
      sl_paths <- SpatialLinesDataFrame(
        sl = SpatialLines(lst_paths),
        data = as.data.frame(
          x = sldf_device_id, 
          row.names = sldf_device_id))
      
      # generate markers on users current positions
      markers <- data_filtered %>% 
        group_by(device_id) %>% 
        filter(date_time == max(date_time)) %>% 
        arrange(device_id)
      
      # update map view obj with graphic elements
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls() %>%
        addPolylines(
          data = sl_paths,
          stroke = TRUE,
          weight = 3,
          color = ~col_palette()(sl_paths$sldf_device_id),
          opacity = 0.7) %>%
        addCircleMarkers(
          data = markers, lat = ~lat, lng = ~lng, group = ~device_id,
          radius = 6,
          stroke = TRUE,
          weight = 1,
          color = "#fff",
          opacity = 0.9,
          fillColor = ~col_palette()(device_id),
          fillOpacity = 0.8) %>%
        addLegend(
          values = data_unique_id,
          pal = col_palette(),
          title = "Device IDs",
          position = "topright", 
          opacity = 1) %>%
        fitBounds(
          lat1 = min(data_sorted$lat), 
          lng1 = min(data_sorted$lng), 
          lat2 = max(data_sorted$lat), 
          lng2 = max(data_sorted$lng))
    }
  })
  
  # observer event for filter reset button
  observeEvent(input$resetFilter, {
    # update widget content
    updateSelectInput(
      session, "device_ids", 
      choices = list("Wait..." = "wait"), 
      selected = "wait")
    updateDateRangeInput(
      session, "date",
      start = min(allData()$date_time_posix),
      end = max(format(as.POSIXct(allData()$date_time, format = "%d/%m/%Y"), "%Y-%m-%d")))
  })

  # observer event for clear remote data button
  observeEvent(input$clearRemote, {
    cat(file = stdout(), "aaa")
    gs_edit_cells(gsheet_file, gsheet_sheet, anchor = "A2",
                  input = matrix("", nrow = nrow(allData()), ncol = 4))
  })
  
  
  # download handler for export shown data button
  output$exportDataCurr <- downloadHandler(
    filename = function() {
      paste0("r_mobile_tracker_", format(input$date[1], "%Y%m%d"), ".csv")
    },
    content = function(file) {
      # query and write data
      data <- filteredData()
      write.table(data, file, sep = ",", row.names = FALSE)
    }
  )
  
  # download handler for export all data button
  output$exportDataAll <- downloadHandler(
    filename = function() {
      paste0("r_mobile_tracker_", format(input$date[2], "%Y%m%d"), "_all.csv")
    },
    content = function(file) {
      # query and write data
      data <- allData()
      write.table(data, file, sep = ",", row.names = FALSE)
    }
  )
})
  
