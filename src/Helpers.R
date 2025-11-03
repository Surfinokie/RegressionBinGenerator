#General helper functions

#Calculates the number of decimal places in a number (from Google)
#return value is an integer
DecimalPlaces <- function(x) {
  if ((x %% 1) != 0) { # Check if the number has a fractional part
    # Convert to character, remove trailing zeros, split by the decimal point,
    # and get the length of the second part (after the decimal)
    return(nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]]))
  } else {
    # If it's an integer, return 0 decimal places
    return(0)
  }
}