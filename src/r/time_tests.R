## Map Viewer for Mobile Sensor Data Visualization in R
## Riccardo Miccini - 2017
## Timing tests

# load necessary packages
library(dplyr)
library(sp)
library(googlesheets)

# Google Sheet data
gsheet_key <- "1qZiPVyEPLzjBmFguJLslwv5xIN00_t_cisOPk9PB1X4"
gsheet_file <- gs_key(gsheet_key)
gsheet_sheet <- "p1"

# read data with Google Sheet API
data <- gs_read(gsheet_file, ws = gsheet_sheet, range = cell_cols("A:F"), literal = TRUE)
