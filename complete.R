# Wing Yan Fan
# 08/15/2014
# R Programming 
# Assignment 1 Part 2

# Write a function that reads a directory full of files and reports the number 
# of completely observed cases in each data file. The function should return a 
# data frame where the first column is the name of the file and the second column 
# is the number of complete cases. 

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating the location of the CSV files
  ## 'id' is an integer vector indicating the monitor ID numbers to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the number of complete cases
  
  ## Read all files from directory with full path name into list
  files_full <- list.files(directory, full.names=TRUE)
  
  ## empty vector
  numOfNobs <- numeric(0)
  
  for (i in id) {
    
    ## Read data from each csv file
    data <- read.csv(files_full[i])
    
    ## count complete cases and append to vector
    numOfNobs <- c(numOfNobs, nrow(na.omit(data)))
  }
  
  ## Result is a data frame with id and nobs columns
  data.frame(id = id, nobs = numOfNobs)
}
