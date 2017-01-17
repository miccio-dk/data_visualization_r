## Map Viewer for Mobile Sensor Data Visulization in R
## Riccardo Miccini - 2017
## R shiny app - user interface

# load necessary packages
library(shiny)
library(leaflet)


# TODO apply theme http://bootswatch.com/paper/bootstrap.min.css

shinyUI(fluidPage(
  # title
  titlePanel("Mobile Sensor Data Visulization in R"),

  # split UI in main and side panels
  sidebarLayout(
    sidebarPanel(
      # filter currently plotted data (user, date range)
      h3("Filter data"),
      dateRangeInput("date", label = h5("Date range"),
                     start = Sys.Date()-10,
                     end = Sys.Date()),
      selectInput("device_ids", label = h5("Device IDs"), multiple = TRUE,
                  choices = list("Wait..." = "wait"), selected = "wait"),
      actionButton("reset_filter", label = "Reset filters"),
      
      # export currently plotted data or all data
      h3("Export data"),
      downloadButton("exportDataCurr", label = "Export shown data"),
      downloadButton("exportDataAll", label = "Export all data"),
      p(),
      
      # clear remote data
      actionButton("clearRemote", label = "Clear remote data")
      
      # EXTRA show users close to the currently selected ones (selectable radius)
    ),

    mainPanel(
      # map object
      leafletOutput("map")
    )
  )
))
