merge_rows_by_threshold_baseR <- function(df, col1_name, col2_name, thresh) {
  
  # Ensure the input is a data.frame for base R indexing
  df <- as.data.frame(df)
  
  # Initialize the index for the current row we are checking
  i <- 1
  
  # Loop until the index 'i' is at the last row
  while (i < nrow(df)) {
    
    # Calculate the absolute difference between the current row (i) 
    # and the next row (i + 1) for the two specified columns
    diff_val <- abs(df[i + 1, col1_name] - df[i, col2_name])
    
    # Check if the difference is less than the threshold
    if (diff_val < thresh) {
      
      # *** MERGE STEP ***
      # 1. Add the numeric values of the next row (i+1) to the current row (i).
      #    We only assume numeric columns are being 'added'. 
      #    You'll need a custom rule for non-numeric columns (e.g., paste, take first/last).
      numeric_cols <- sapply(df, is.numeric)
      df[i, numeric_cols] <- df[i, numeric_cols] + df[i + 1, numeric_cols]
      
      # 2. Remove the next row (i+1)
      df <- df[-(i + 1), ]
      
      # NOTE: We do NOT increment 'i' here because the *new* row 'i + 1' 
      # (which was originally 'i + 2') now needs to be checked against the 
      # row 'i' we just modified.
      
    } else {
      
      # The difference is >= thresh, so we keep both rows and move to the next row
      i <- i + 1
    }
  }
  
  return(df)
}