## best.R

# Create a function "best" that take 2 args: 1. 2-character state name 2.outcome name. 
# The function reads the outcome-of-care-measures.csv and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specifed outcome
# in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
# be one of heart attack, heart failure, or pneumonia. Hospitals that do not have data on a particular
# outcome should be excluded from the set of hospitals when deciding the rankings.

best <- function(state, outcome) {
  #Invalid outcome input 
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop("invalid outcome")
  }
  
  #Get index for given outcome.
  index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  
  #Read and coerce the dataset with warnings supressed and remove NA's.
  dataset <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  dataset[,index] <- suppressWarnings(as.numeric(dataset[,index]))
  dataset <- na.omit(dataset)
  
  #Invalid state input / no observations found
  states <- table(dataset$State)
  if (!state %in% names(states)) { 
    stop("invalid state")
  }
  
  #seperate out the data by given state and sort it by outcome and hospital name.
  resultset <- subset(dataset, State==state)
  resultset <- resultset[order(resultset[,index], na.last=TRUE),2]
  resultset <- na.omit(resultset)
  
  #Get hospital name with the lowest 30-day mortality rate.
  resultset[1]
}

## Evaluated Results
## > source('C:/Users/APPU-PAPPU/Desktop/Coursera/Rworkspace/best.R')
## > best("TX", "heart attack")
## [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
## > best("TX", "heart failure")
## [1] "FORT DUNCAN MEDICAL CENTER"
## > best("MD", "heart attack")
## [1] "JOHNS HOPKINS HOSPITAL, THE"
## > best("MD", "pneumonia")
## [1] "GREATER BALTIMORE MEDICAL CENTER"
## > best("BB", "heart attack")
## Error in best("BB", "heart attack") : invalid state
## > best("NY", "hert attack")
## Error in best("NY", "hert attack") : invalid outcome