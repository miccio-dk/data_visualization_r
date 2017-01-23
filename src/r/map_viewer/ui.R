## Map Viewer for Mobile Sensor Data Visualization in R
## Riccardo Miccini - 2017
## R shiny app - user interface

# load necessary packages
library(shiny)
library(leaflet)
library(shinythemes)
library(plotly)


# TODO 
# - fix zoom bug
# - mention UI reqs in report SRS
# - add heatmap of user positions
# - time-series pplots http://rstudio.github.io/dygraphs/gallery-series-options.html


shinyUI(fluidPage(
  # apply custom theme
  theme = shinytheme("paper"),
  
  # title
  titlePanel("Mobile Sensor Data Visulization in R"),

  # split UI in main and side panels
  sidebarLayout(
    sidebarPanel(
      # update in realtime or not
      checkboxInput("realtime", label = "Update in real-time", value = TRUE),
      
      # filter currently plotted data (user, date range)
      hr(),
      h5("Filter data"),
      dateRangeInput("date", label = h6("Date range"),
                     start = Sys.Date()-10,
                     end = Sys.Date()),
      selectInput("device_ids", label = h6("Device IDs"), multiple = TRUE,
                  choices = list("Wait..." = "wait"), selected = "wait"),
      actionButton("resetFilter", label = "Reset filters"),
      
      # export currently plotted data or all data
      hr(),
      h5("Export data"),
      downloadButton("exportDataCurr", label = "Current data"),
      downloadButton("exportDataAll", label = "All"),
      
      # clear remote data
      hr(),
      actionButton("clearRemote", label = "Clear remote data")
    ),

    mainPanel(
      # map object
      leafletOutput(
        "map",
        width = "100%",
        height = "325px"),
      hr(),
      # plotly chart object
      plotlyOutput(
        "plot",
        width = "100%",
        height = "250px")
    )
  )
))
