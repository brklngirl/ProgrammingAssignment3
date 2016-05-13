  # First, we load "Outcome of Care Measures" data table into R and inspect it:

  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  head(outcome)
  ncol(outcome)
  nrow(outcome)
  names(outcome)

  # 1. Let's build a simple plot for the 30-day death rates from heart attack (column 11):

  outcome[, 11] <- as.numeric(outcome[, 11])
  hist(outcome[, 11])

  # 2. Now we will create a function searching for the best hospital in state.

  best <- function (state, outcome) {
  my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  
  select_o <- if (outcome == "heart attack") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  # Let's check state validity
  valid_state <- which(my_data$State == state)
  
  # we create a loop, if any of the table states names match given state name, 
  # then mark state as valid; if no states match, mark invalid
  for (i in 1:nrow(my_data)) {
    if(my_data$State[i] == state) {
      valid_state
      break} 
    if (length(valid_state) == 0) {
      stop("invalid state")}
  }
  
  #now we select only observations that match the state and the outcome specified by user
    best_data <- my_data[valid_state, c("Hospital.Name", select_o)] 
    
  #make sure selected outcome column is numeric:
    best_data[, select_o] <- as.numeric(best_data[, select_o])
    
  #order our new table by outcome and Hospital.Name
    ordered <- order(best_data[, select_o], best_data[, "Hospital.Name"])
    
  #To select the Hosital with the lowest mortality rate, we just need to select 
  #the first element in our ordered table
    as.character(best_data[, "Hospital.Name"][ordered[1]])
  }
  
  # sample outputs:
  best("TX", "heart attack")
  best("TX", "heart failure")
  best("MD", "heart attack")
  best("MD", "pneumonia")
  best("BB", "heart attack")
  best("NY", "hert attack")
