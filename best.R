# Clear environment objects before running the code
rm(list = ls())

# Defining the function to find the best hospital
best <- function(state, outcome) {
  
  # Read the data
  data <- read.csv("outcome-of-care-measures.csv")
  
  # Check whether the state and outcome are valid
  
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
  
  # Find the hospital with lowest 30 day mortality rate in the state
  
  # Create a data frame with only state hospitals
  state_data <- data[data$State == state, ]
  #print(head(state_data))
  
  # Order the data frame by hospital names alphabetically
  state_data <- state_data[ order(state_data$Hospital.Name), ]
  #print(head(state_data))
  
  # Find the hospital (which one has minimum deaths) per outcome
  if (outcome == "heart attack") {
    best <- state_data$Hospital.Name[ which.min(state_data[, 11]) ]
  } else if(outcome == "heart failure") {
    best <- state_data$Hospital.Name[ which.min(state_data[, 17]) ]
  } else {
    best <- state_data$Hospital.Name[ which.min(state_data[, 23]) ]
  }
  
  # Return the hospital name
  best
       
}

# Test
print( best("TX", "heart failure"))