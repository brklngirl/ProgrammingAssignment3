# 3. We create a fucntion that returns the hospital with specified ranking

  rankhospital <- function (state, outcome, num) {
  
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
  
  # we create a loop, if any of the states names in a column match given state name, 
  # then mark state as valid; if no states match, mark invalid
  for (i in 1:nrow(my_data)) {
    if(my_data$State[i] == state) {
      valid_state
      break} 
    if (length(valid_state) == 0) {
      stop("invalid state")}
  }
  
  # now we select only observations that match the state and the outcome specified by user
  rank_data <- my_data[valid_state, c("Hospital.Name", select_o)] 
  na.omit(rank_data)
  
  # make sure selected outcome column is numeric:
  rank_data[, select_o] <- as.numeric(rank_data[, select_o])
  
  # order our new table by outcome and Hospital.Name
  ordered <- rank_data[order(rank_data[, select_o], rank_data$Hospital.Name, na.last = NA), ]
  
  # now we will define values for Hospital rank
  rank <- as.integer() 
  rank <- if (num == "best") {
    1
  } else if (num < nrow(rank_data)) {
    num 
  } else if (num == "worst") {
    nrow(ordered)
  } else { 
    stop("NA")
  }
  # Now we select the Hospital with a given rank
  as.character(ordered$Hospital.Name[rank])
}

#sample inputs:
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
