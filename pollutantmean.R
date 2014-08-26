# Wing Yan Fan
# 08/15/2014
# R Programming 
# Assignment 1 Part 1

# Write a function named 'pollutantmean' that calculates the mean of a pollutant 
# (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' 
# takes three arguments: 'directory', 'pollutant', and 'id'. 

pollutantmean <- function(directory, pollutant, id = 1:332) {
  files_full <- list.files(directory, full.names=TRUE)
  #files_full
  #file1 <- read.csv(files_full[3])
  #head(file1)

  data <- data.frame()
  for (i in id) {
    data <- rbind(data, read.csv(files_full[i]))
  }
  #str(data)
  
  data_sub <- data[, pollutant]
  ans <- mean(data_sub, na.rm=TRUE)
  format(round(ans, 3), nsmall = 3)
}

