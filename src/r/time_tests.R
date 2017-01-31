## Map Viewer for Mobile Sensor Data Visualization in R
## Riccardo Miccini - 2017
## Timing tests

# load necessary packages
library(dplyr)
library(googlesheets)
library(emdbook)
library(ggplot2movies)
library(ggplot2)

# sheet params
gsheet_sheet <- "time_test"

# simulation params
num_rows <- lseq(100, 50000, 20)
time_smpls <- 20

# data object
data <- data.frame("Mean" = double(), "1st Qu." = double(), "3rd Qu." = double())

# for each number of rows to be simulated
for(num_rows_i in num_rows) {
  
  # generate data
  ggplot2movies::movies %>%
    head(num_rows_i) %>%
    write.csv(file = "tmp_data.csv")
  
  # create new sheet from given data
  gsheet_curr <- paste(gsheet_sheet, toString(round(num_rows_i)))
  gsheet_file <- gs_upload(file = "tmp_data.csv", sheet_title = gsheet_curr)
  file.remove("tmp_data.csv")
 
  # time samples array 
  time_smpls_data <- numeric(length = time_smpls)
  
  # for each sample to be taken
  for(time_smpls_i in 1:time_smpls) {
    # start the clock
    time_start <- proc.time()
    
    # read data from sheet
    tmp <- gs_read(gsheet_file, ws = gsheet_curr, range = cell_cols("A:H"))
    
    # stop the clock
    time_elaps <- proc.time() - time_start
    
    # store measurement
    time_smpls_data[time_smpls_i] <- time_elaps[["elapsed"]]
  }
  
  # append results
  tmp_summary <- summary(time_smpls_data)
  data_new <- data.frame(
    "Mean" = as.numeric(tmp_summary["Mean"]),
    "1st Qu." = as.numeric(tmp_summary["1st Qu."]),
    "3rd Qu." = as.numeric(tmp_summary["3rd Qu."]))
  data <- bind_rows(data, data_new)

  # plot data
  gs_delete(gsheet_file)
}

# append number of rows column
data$Rows <- round(num_rows)

# generate plot
p <- ggplot(data, aes(x = Rows, y = Mean, fill = (TPRow * 1000))) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge(), width = .085) +
  geom_errorbar(aes(ymin = X1st.Qu., ymax = X3rd.Qu.), width = .025) + 
  labs(
    x = "Number of rows", 
    y = "Total time taken (s)",
    fill = "Time per row (ms)") +
  theme(
    panel.background = element_blank(),
    panel.grid.minor = element_line(colour = "lightgray", size = 0.3),
    panel.grid.major = element_line(colour = "gray", size = 0.5),
    axis.line = element_line(colour = "darkgray", size = 0.5),
    axis.ticks = element_line(colour = "darkgray", size = 0.5)) +
  scale_x_log10(
    minor_breaks = c(seq(100 , 1000, 100), seq(1000, 10000, 1000)), 
    breaks = lseq(100, 10000, 3)) + 
  scale_y_log10(
    minor_breaks = c(seq(1, 10, 1), seq(10, 100, 10)), 
    breaks = lseq(1, 100, 3)) +
  scale_fill_continuous(guide = "colourbar")
print(p)
