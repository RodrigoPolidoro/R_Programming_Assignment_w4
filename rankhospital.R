# Clear environment objects before running the code
rm(list = ls())

# Defining function to find the hospital in the desired rank
rankhospital <- function(state, outcome, num = "best") {
  
  # Read the data
  data <- read.csv("outcome-of-care-measures.csv")
  
  # Check whether the state, outcome and rank are valid
  
  # Check if state value appears on State column of data frame
  # If not stop the function
  if (!state %in% data$State) {
    stop("Invalid state")
  }
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
  
  # Find the hospital with wanted rank in 30 day mortality rate in the state
  
  # Create a data frame with only state hospitals
  state_data <- data[data$State == state, ]

  # Change column of mortality rates to numeric
  # Remove NAs values from column
  # Order the data frame by rank and hospital names alphabetically
  if (outcome == "heart attack") {
    state_data[, 11] <- as.numeric(state_data[ , 11])
    state_data <- state_data[complete.cases(state_data[ , 11]), ] 
    state_data <- state_data[ order(state_data[, 11], state_data$Hospital.Name), ]
    
  } else if(outcome == "heart failure") {
    state_data[, 17] <- as.numeric(state_data[ , 17])
    state_data <- state_data[complete.cases(state_data[ , 17]), ] 
    state_data <- state_data[ order(state_data[, 17], state_data$Hospital.Name), ]
    
  } else {
    state_data[, 23] <- as.numeric(state_data[ , 23])
    state_data <- state_data[complete.cases(state_data[ , 23]), ] 
    state_data <- state_data[ order(state_data[, 23], state_data$Hospital.Name), ]
  }
  
  # Find the hospital (of given rank) per outcome
  
  # Convert rank all to numeric
  if (num == "best") {
    n <- 1
  } else if (num == "worst") {
    n <- nrow(state_data)
  } else {
    n <- num
  }
  
  # Find the hospital of desired rank per outcome
  rank_hosp <- state_data$Hospital.Name[n]
  
  # Return the hospital name
  rank_hosp
}

# Test
print(rankhospital("TX", "heart failure", "best"))
  