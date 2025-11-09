# TODO:

1.  Need test files for edge cases
2.  Convert to package
3.  Merge ProcessDev.R and FinalResultGenerator.R logic to add new columns to original data and return that.
4.  Add column name in method call to id the column over which to sum.
5.  Make rugosity value a variable so it can be changed. This is tricky because of decimal places. Build a funciton to compute the number of decimal places in the function and then take 10 to the power of that number and it will give the variable/multiplier.
6.  Restructure this so that depth category and rugosity category can live peacefully together.
7.  Genericize min, max, & mid columns for use with depth & rugosity
8. Add error handling/bounds checking in helper functions

# Questions

-Should package prompt for result save info or just return data.frame?

# Dev Project
1. Add measure variable.

