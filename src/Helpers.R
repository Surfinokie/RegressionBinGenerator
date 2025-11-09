# General helper functions
library(tidyverse)

# Calculates the number of decimal places in a number (from Google)
# X should be a number and the return value is an integer
GetDecimalPlaces <- function(x) {
  if ((x %% 1) != 0) { 
    # Check if the number has a fractional part
    # Convert to character, remove trailing zeros, split by the decimal point,
    # and get the length of the second part (after the decimal)
    return(nchar(strsplit(sub("0+$", "", as.character(x)), ".", fixed = TRUE)[[1]][[2]]))
  } else {
    # If it's an integer, return 0 decimal places
    return(0)
  }
}

# Get a vector of distinct measure names from a dataframe
# The measure_name should be a column name of the dataset which should be a dataframe
# Come to find out this is sub-optimal. unique() does all this from baseR
GetDistinctMeasure <- function(dataset, measure_name) {
  distinct_measure <- dataset %>%
    distinct(dataset[[measure_name]]) %>%
    pull()

  return(distinct_measure)
}

CalculateThresholdMultiplier <- function(threshold){
  # If the threshold = 0, the multiplier is 1
  # If the threshold is positive integer >= 1 < 10, the multiplier will be 1/threshold - ???
  # If it is >= 10, then the multiplier will be 1/threshold - ???
  # If 0 < threshold < 1, the multiplier will be 10^GetDecimalPlaces(threshold)
  # If GetDecimalPlaces(threshold) > 0 && 1 < threshold then it is an error
  
  # This is a linear transform on the measure allowing us to use the floor function
  # Since we aren't going to do any non-linear operations in the counting, then reversing the transform at the end
  # of the day will bring us back to the original value while allowing easy counts/bins
  # For values != to 1 or 0 but that are still integers, is this needed? Because if the decimal place version
  # works, would that be similar?
}
