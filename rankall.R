# Wing Yan Fan
# 08/26/2014
# R Programming 
# Assignment 3 Part 3

# Write a function called rankhospital that takes three arguments: the 2-character 
# abbreviated name of a state (state), an outcome (outcome), and the ranking of a 
# hospital in that state for that outcome (num). The function reads the 
# outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument. 

rankall <- function(outcome, num = "best") {
  
  # check the validity of its arguments
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  # get the index of the outcome string
  i <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  
  # read from data file
  data <- read.csv(file="outcome-of-care-measures.csv", colClasses = 'character') 
  
  # suppress warnings and remove NA's
  data[,i] <- suppressWarnings(as.numeric(data[,i]))
  data <- data[!is.na(data[,i]),]
  
  # remove duplicate states
  unique.states <- sort(unique(data$State))
  
  result.df <- list()
  
  for(state in unique.states) {
    data.state <- data[data$State == state, ]
    data.state[, i] <- as.numeric(x=data.state[, i])
    data.state <- data.state[complete.cases(data.state), ]
    
    # validate our num
    numrank <- ifelse(num == "best", 1, ifelse(num == "worst", length(data.sorted), as.numeric(num)))
    
    data.state <- data.state[order(data.state[,i], data.state$Hospital.Name), ]
    
    return.names <- data.state[numrank, ]$Hospital.Name
    
    result.df <- rbind(result.df, list(return.names[1], state))
  }
  
  result.df <- as.data.frame(x=result.df)
  colnames(x=result.df) <- c('hospital', 'state')
  
  result.df
}