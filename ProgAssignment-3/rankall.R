# Author : Santhosh Kumar

## rankall that takes two arguments: an outcome name (outcome) and a hospital rank-ing (num). 
## The function reads the outcome-of-care-measures.csv and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num. For example the function call
## rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
## are the best in their respective states for 30-day heart attack death rates. The function should return a value
## for every state (some may be NA). The rst column in the data frame is named hospital, which contains
## the hospital name, and the second column is named state, which contains the 2-character abbreviation for
## the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
## hospitals when deciding the rankings

setwd("C:/Users/APPU-PAPPU/Desktop/Coursera/Rworkspace")
getwd()

dataset <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
dataset <- dataset[c(2, 7, 11, 17, 23)]
dataset[, c(3, 4, 5)] <- suppressWarnings(sapply(dataset[, c(3, 4, 5)], as.numeric))

## Rank based on the outcome and hospital ranking generated
rankall <- function(outcome, num = "best") {
  ## Read state,outcome data
  
  ## Check that state and outcome are valid
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the (abbreviated) state name
  
  
  #Invalid state input / no observations found
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  #Invalid state input / no observations found
  states <- unique(dataset$State)
  states <- sort(states)
  
  if (!(outcome %in% outcomes)) {
    stop("invalid outcome")
  }
    
  ranks <- data.frame(hospital=NA, state=NA)
  
  for (i in 1:length(states)) {
    ranks[i, ] <- c(rankhospital(states[i], outcome, num), states[i])
  }
  
  ranks
}

dataset <- dataset[order(dataset[, 4]), ]
dataset <- dataset[!is.na(dataset[, 4]), ]
