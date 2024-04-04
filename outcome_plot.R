# Clear environment objects before running the code
rm(list = ls())

# Read the outcome file into a character data frame 
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# Some analysis of the data frame
head(outcome)
str(outcome)
dim(outcome)
names(outcome)

# Plot the 30 day mortality rates for heart attack
# Change the column to numeric values for plotting
outcome[, 11] <- as.numeric(outcome[, 11])

# Plot the histogram
hist(outcome[, 11])

