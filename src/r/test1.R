library(googlesheets)
suppressMessages(library(dplyr))

key <- "1qZiPVyEPLzjBmFguJLslwv5xIN00_t_cisOPk9PB1X4"
file <- gs_key(key)


while (TRUE) {
  flush.console()
  
  data = gs_read(file, ws = "p1", range = cell_cols("A,B"), literal = TRUE)
  data$Time = as.POSIXct(data$date_time)
  
  plot(data$Time, data$value, type = 'p')
  Sys.sleep(.09)
}