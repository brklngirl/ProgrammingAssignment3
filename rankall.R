## Solution for part 4 of Programming Assignment 3

# 4. Create a fucntion that returns hospital in all states with the given ranking

rankall <- function (outcome, num = "best") {
  
  #read-in our data
  my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", 
                      na.strings = "Not Available")
  
  # check outcome validity
  select_o <- if (outcome == "heart attack") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  # select only needed columns into data frame to perform final manipulations
  rank_data <- my_data[, c("Hospital.Name","State", select_o)]
  rank_data[, select_o] <- as.numeric(rank_data[, select_o])
  
  rank_data <- na.omit(rank_data)
  names(rank_data) <- c("hospital", "state", "outcome_rate")
  
  dat <- NULL
  
  # create loop over each unique state value
  for(i in unique(rank_data$state)) {
  hosp_data <- rank_data[rank_data$state == i, ]
    
  # define rank values
  rank <- as.integer() 
    
  rank <- if (num == "best") {
      1
  } else if (num == "worst") {
      nrow(hosp_data)
  } else if (num < nrow(hosp_data)) {
      num
  } else {
      hosp_data$hospital <- "NA"
  } 
    
  # order outcome results and then hospital names for each state
  ordered <- hosp_data[order(hosp_data$outcome_rate, hosp_data$hospital), c(1, 2)][rank, ]
  ordered$state <- rep(i, nrow(ordered))
  
  # tie result of for each state into a data frame
  dat <- rbind(dat, ordered)
  names(dat) <- c("hospital", "state")
  final <- dat[order(dat$state), ]
  }
  
  final
}

# sample inputs

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
