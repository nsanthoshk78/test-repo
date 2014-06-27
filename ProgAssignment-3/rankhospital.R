## rankhospital.R

## rankhospital that takes three arguments: the 2-character abbreviated name of a state (state), 
## an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
## The function reads the outcome-of-care-measures.csv and returns a character vector with 
## the name of the hospital that has the ranking specified by the num argument.

setwd("C:/Users/APPU-PAPPU/Desktop/Coursera/Rworkspace")
getwd()

dataset <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
dataset <- dataset[c(2, 7, 11, 17, 23)]
dataset[, c(3, 4, 5)] <- suppressWarnings(sapply(dataset[, c(3, 4, 5)], as.numeric))

state <- "TX"
outcome <- "heart failure"
num <- 10L

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  
  ## Check validity of outcome and state
  
  ## Return hospital name in that state with the given rank 30-day death rate
  
  states <- unique(dataset$State)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(state %in% states)) {
    stop("invalid state")
  }
  
  if (!(outcome %in% outcomes)) {
    stop("invalid outcome")
  }
  
  dataset1 <- dataset[dataset$State == state, ]
  
  if (outcome == "heart attack") {
    dataset1 <- dataset1[order(dataset1[, 3], dataset1[, 1]), ]
    dataset1 <- dataset1[!is.na(dataset1[, 3]), ]
  }
  else if (outcome == "heart failure") {
    dataset1 <- dataset1[order(dataset1[, 4], dataset1[, 1]), ]
    dataset1 <- dataset1[!is.na(dataset1[, 4]), ]
  }
  else {
    dataset1 <- dataset1[order(dataset1[, 5], dataset1[, 1]), ]
    dataset1 <- dataset1[!is.na(dataset1[, 5]), ]
  }
  
  if (num == "best") {
    num <- 1L
  }  
  else if (num == "worst") {
    num <- nrow(dataset1)
  }
  else {
    num <- as.numeric(num)
  }
  
  dataset1[num, 1]
}

## Evaluated Results
##> source('C:/Users/APPU-PAPPU/Desktop/Coursera/Rworkspace/rankhospital.R')
##> rankhospital("TX", "heart failure", 4)
##[1] "DETAR HOSPITAL NAVARRO"
##> rankhospital("MD", "heart attack", "worst")
##[1] "HARFORD MEMORIAL HOSPITAL"
##> rankhospital("MN", "heart attack", 5000)
##[1] NA