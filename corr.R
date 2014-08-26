# Wing Yan Fan
# 08/15/2014
# R Programming 
# Assignment 1 Part 3

# Write a function that takes a directory of data files and a threshold for complete 
# cases and calculates the correlation between sulfate and nitrate for monitor locations 
# where the number of completely observed cases (on all variables) is greater than the 
# threshold. The function should return a vector of correlations for the monitors that meet 
# the threshold requirement.

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the number of completely observed 
  ## observations (on all variables) required to compute the correlation between nitrate and 
  ## sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ## Read all files from directory with full path name into list
  files_full <- list.files(directory, full.names=TRUE)
  
  ## empty vector
  corrs <- numeric(0)
  
  ## data frame of complete cases
  nobsDF <- complete("specdata")
  
  ## with threshold
  nobsDF <- nobsDF[nobsDF$nobs > threshold,]
  
  for (i in nobsDF$id) {
    
    ## Read data from each csv file
    data <- read.csv(files_full[i])
    
    ## Calculate corr between nitrate and sulfate
    corrs <- c(corrs, cor(data$nitrate, data$sulfate, use = "pairwise.complete.obs"))
  }
  return (corrs)
}