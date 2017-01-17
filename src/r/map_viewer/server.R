## Map Viewer for Mobile Sensor Data Visulization in R
## Riccardo Miccini - 2017
## R shiny app - server side

# load necessary packages
library(shiny)
library(leaflet)
library(googlesheets)
library(sp)
library(dplyr)


shinyServer(function(input, output, session) {
  # Google Sheet data
  key <- "1qZiPVyEPLzjBmFguJLslwv5xIN00_t_cisOPk9PB1X4"
  file <- gs_key(key)
  
  # reactive expression for the raw data
  allData <- reactive({
    # reload every 2 seconds
    invalidateLater(5000, session)
    
    # read data with Google Sheet API
    data <- gs_read(file, ws = "p1", range = cell_cols("A:D"), literal = TRUE)
    
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
        filter(format(as.POSIXct(date_time, format = "%d/%m/%Y"), "%Y-%m-%d") >= min(input$date)) %>%
        filter(format(as.POSIXct(date_time, format = "%d/%m/%Y"), "%Y-%m-%d") < max(input$date))
    }
    
    return(data)
  })
  
  
  # generates map obj and add pretty tiles
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(
        "OpenStreetMap.Mapnik", 
        options = providerTileOptions(noWrap = TRUE))
  })
  
  
  # observer for loading list of devices id
  observe({
    # get unique list of ids
    lst_device_ids <- allData() %>% select(device_id) %>% unique()
    
    if(!is.null(input$device_ids) && input$device_ids == "wait") {
      # update widget content
      updateSelectInput(session, "device_ids", 
                        choices = as.list(lst_device_ids), 
                        selected = as.array(lst_device_ids$device_id))
    }
  })
    
  # observer for updating the map elements
  observe({
    # load user-filtered data
    data_filtered <- filteredData()
    
    if(nrow(data_filtered) > 0) {
      # sort by device id and time
      data_sorted <- data_filtered %>% arrange(device_id, date_time)
      
      # generate list of individual paths based on unique ids
      lst_paths <- lapply(unique(data_filtered$device_id), function(id){
        # filter by device id
        data_sorted_id <- data_sorted %>% filter(device_id == id)
        # return Lines obj generated from a data matrix of [lng lan] 
        Lines(Line(data.matrix(data_sorted_id[c("lng", "lat")])), ID = id)
      })
      
      # spatial lines object
      sl_paths <- SpatialLines(lst_paths)
      
      # create a colorblind-safe (up to 4 elements) palette, based on :
      # http://colorbrewer2.org/#type=qualitative&scheme=Paired&n=5
      col_palette <- colorFactor(
        palette = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99'),
        domain = data_sorted$device_id
      )
      
      # generate markers on users current positions
      markers <- data_filtered %>% 
        group_by(device_id) %>% 
        filter(date_time == max(date_time)) %>% 
        arrange(device_id)
      
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolylines(
          data = sl_paths,
          stroke = TRUE,
          weight = 2.5,
#          color = ~col_palette("a"),
          opacity = 0.35) %>%
        addCircleMarkers(
          data = markers, lat = ~lat, lng = ~lng, group = ~device_id,
          radius = 6,
          stroke = TRUE,
          weight = 1,
          color = "#fff",
          opacity = 0.9,
          fillColor = ~col_palette(device_id),
          fillOpacity = 0.8) %>%
        fitBounds(
          lat1 = min(data_sorted$lat), 
          lng1 = min(data_sorted$lng), 
          lat2 = max(data_sorted$lat), 
          lng2 = max(data_sorted$lng))
    }
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
  
