# Wing Yan Fan
# 08/26/2014
# R Programming 
# Assignment 3 Part 2

# Write a function called rankhospital that takes three arguments: 
# the 2-character abbreviated name of a state (state), an outcome (outcome), 
# and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a 
# character vector with the name of the hospital that has the ranking 
# specified by the num argument.

rankhospital <- function(state, outcome, num="best") {
  # check the validity of its arguments
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  
  # get the index of the outcome string
  index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  
  # read data from data file
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  # suppress warnings and remove NA's
  data[,index] <- suppressWarnings(as.numeric(data[,index]))
  data <- na.omit(data)
  
  # invalid state input or no observations
  states <- table(data$State)
  if (!state %in% names(states)) { 
    stop("invalid state")
  }
  
  # slice data by the given state
  slice <- subset(data, State==state)
  
  # sort the sliced data by outcome and hospital name.
  slice <- slice[order(slice[,index], slice[,2], na.last=TRUE),2] #this is ordered by rate
  slice <- na.omit(slice)
  
  num <- ifelse(num == "best", 1, ifelse(num == "worst", length(slice), as.numeric(num)))
  
  # get the hospital names 
  slice[num]
}