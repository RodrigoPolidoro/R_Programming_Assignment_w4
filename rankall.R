# Clear environment objects before running the code
rm(list = ls())

# Defining function to find the hospital in the desired rank
rankall <- function(outcome, num = "best") {
  
  # Read the data
  data <- read.csv("outcome-of-care-measures.csv")
  
  # Check whether the outcome and rank are valid
  
  # Check if outcome value is one of the possibilities
  # If not stop the function
  if ( (outcome != "heart attack") && (outcome != "heart failure") && 
       (outcome != "pneumonia") ) {
    stop("Invalid outcome")
  }
  # Check if ranking is valid
  # If not stop the function
  if ( (class(num) == "character") && ( (num != "best") && (num != "worst") )  ) {
    stop("Invalid rank")
  }
  
  # Find the hospital with wanted rank in 30 day mortality rate in each state
  
  # Create array with states list and order alphabetically
  states <- unique(data$State)
  states <- states[order(states)]
  # Create an empty data frame to store the hospitals list
  rank_hosp <- data.frame()
  
  # Change column of mortality rates to numeric
  # Remove NAs values from column
  # Order the data frame by rank and hospital names alphabetically
  if (outcome == "heart attack") {
    data[, 11] <- as.numeric(data[ , 11])
    data <- data[complete.cases(data[ , 11]), ] 
    data <- data[ order(data[, 11], data$Hospital.Name), ]
    
  } else if(outcome == "heart failure") {
    data[, 17] <- as.numeric(data[ , 17])
    data <- data[complete.cases(data[ , 17]), ] 
    data <- data[ order(data[, 17], data$Hospital.Name), ]
    
  } else {
    data[, 23] <- as.numeric(data[ , 23])
    data <- data[complete.cases(data[ , 23]), ] 
    data <- data[ order(data[, 23], data$Hospital.Name), ]
  }
  
  # Find the hospital of desired rank per outcome
  for (i in states) {
    
    # Filter only state hospitals
    data_state <- data[data$State == i, ]
    
    # Convert rank all to numeric
    if (num == "best") {
      n <- 1
    } else if (num == "worst") {
      n <- nrow(data_state)
    } else {
      n <- num
    }
    
    # Add the hospital and state to the data frame
    rank_hosp <- rbind(rank_hosp, c( data_state$Hospital.Name[n], i ) )
  }
  
  # Rename the data frame columns
  names(rank_hosp) <- c("hospital", "state")
  
  # Return data frame as output of function
  rank_hosp
  
}

# Test
print(tail(rankall("heart failure"),10))
