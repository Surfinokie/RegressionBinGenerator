# David Graham 11/8/25
# R version 4.4.3
# Tidyverse 2.0.0 (dplyr 1.1.4)
# Go to line 194 for instructions on execution
library(tidyverse)

# Get all rows with the species code and create a new summarised df on which to operate
PrimaryGroupExtractor <- function(primary_group_value) {
  intermed_frame <- filter(rbg_env$ana_frame, !!sym(rbg_env$primary_group) == primary_group_value) %>%
    group_by(!!sym(rbg_env$primary_group), act_measure) %>%
    summarise(
      n = n(),
      obs = max(pres),
      pres_true = sum(ifelse(pres == 1, 1, 0)),
      pres_false = sum(ifelse(pres == 0, 1, 0))
    ) |>
    arrange(act_measure)
  
  operant_frame <- as.data.frame(intermed_frame) 
  operant_frame <- FillSparseValues(operant_frame)
  return(operant_frame)
  
}

FillSparseValues <- function(operant_frame) {
  # Find missing values for the series defined by measure
  # Must make sure this is an integer series from 1 to n
  # PrimaryGroupExtractor (pge) has already grouped and ordered the rows
  # Now generate a series from 0 to max(pge$act_measure), find the disjoint set, then add those rows 
  # to pge data frame
  rbg_env$max_val <- max(operant_frame$act_measure)
  rbg_env$all_vals <- seq(from = 1, to = rbg_env$max_val, by = 1)
  rbg_env$sdiff <- setdiff(rbg_env$all_vals, operant_frame$act_measure)
  
  # Add an empty row to the data frame for each missing value defined in sdiff
  for (i in 1:length(rbg_env$sdiff)){
    operant_frame <- operant_frame %>% add_row(group_name = operant_frame$group_name[1], 
                                               act_measure = rbg_env$sdiff[i], 
                                               n = 0, 
                                               obs = 0, 
                                               pres_true = 0, 
                                               pres_false = 0)
    
    #print(rbg_env$sdiff[i])
  }
  
  operant_frame <- operant_frame %>% arrange(act_measure)
  return(operant_frame)
  
}

ProcessBins <- function(processing_frame) {
  i <- 1
  # Empty the result frame for cases where there is > 1 groups to process
  rbg_env$result <- rbg_env$result[0, ]
  while (i <= nrow(processing_frame)) {
    current_sum <- 0
    # number of present observations for logistic
    pos_sum <- 0
    # number of not-present observations for logistic
    neg_sum <- 0
    rows_to_add <- list()
    vObs <- vector()
    vPos <- vector()
    vNeg <- vector()
    vSppCd <- vector()
    vDc <- vector()
    
    while (current_sum < rbg_env$bin_size && i <= nrow(processing_frame)) {
      # does this have a first row error and always add the first row to the second?
      current_sum <- current_sum + sum(processing_frame[i, c("n")])
      pos_sum <- pos_sum + sum(processing_frame[i, c("pres_true")])
      neg_sum <- neg_sum + sum(processing_frame[i, c("pres_false")])
      
      vObs <- append(vObs, current_sum)
      vPos <- append(vPos, pos_sum)
      vNeg <- append(vNeg, neg_sum)
      vSppCd <- append(vSppCd, processing_frame[i, c("group_name")])
      vDc <- append(vDc, processing_frame[i, 2])
      result_frame <- data.frame(unlist(vSppCd), vObs, min(unlist(vDc)), max(unlist(vDc)), vPos, vNeg)
      
      # set the current row's max_dc to the next row's min_dc
      if (!is.na(processing_frame[i + 1, 2])) {
        result_frame[1, 4] <- processing_frame[i + 1, 2]
      } else {
        result_frame[1, 4] <- processing_frame[i, 2]
      }
      
      rows_to_add <- append(rows_to_add, result_frame)
      i <- i + 1
    }
    
    #colnames(result_frame) <- c("species_cd", "obs", "min_dc", "max_dc", "pos_obs", "neg_obs")
    colnames(result_frame) <- c(rbg_env$primary_group, "observations", "min_measure", "max_measure", "pos_observations", "neg_observations")
    intermed_result_frame <- result_frame %>%
      group_by(rbg_env$the_frame[1,c("group_name")]) %>%
      summarise(
        max(observations),
        min(min_measure),
        min(min_measure),
        max(max_measure),
        sum(pos_observations),
        sum(neg_observations)
      )
    
    #change this to use the variables
    #is this necessary?
    #colnames(intermed_result_frame) <- c("species_cd", "obs", "min_dc", "max_dc", "pos_obs", "neg_obs")
    colnames(intermed_result_frame) <- c(rbg_env$primary_group, "observations", "min_measure", "max_measure", "pos_observations", "neg_observations")
    
    rbg_env$result <- rbind(rbg_env$result, intermed_result_frame)
  }
  
  # If the last row is < the threshold, add it to the preceding row, and remove the last
  # has to be a better way to do this
  if (nrow(rbg_env$result) > 1 && rbg_env$result[nrow(rbg_env$result), 2] < rbg_env$bin_size) {
    rbg_env$result[nrow(rbg_env$result) - 1, 2] <- rbg_env$result[nrow(rbg_env$result) - 1, 2] + rbg_env$result[nrow(rbg_env$result), 2]
    rbg_env$result[nrow(rbg_env$result) - 1, 4] <- rbg_env$result[nrow(rbg_env$result), 4]
    rbg_env$result[nrow(rbg_env$result) - 1, 5] <- rbg_env$result[nrow(rbg_env$result) - 1, 5] + rbg_env$result[nrow(rbg_env$result), 5]
    rbg_env$result[nrow(rbg_env$result) - 1, 6] <- rbg_env$result[nrow(rbg_env$result) - 1, 6] + rbg_env$result[nrow(rbg_env$result), 6]
    
    rbg_env$result <- rbg_env$result %>% filter(!row_number() %in% nrow(rbg_env$result))
  }
  
  # Need to find out if we need to do this
  # Set the first row min value to 0 as it actually starts at the first data point which will usually will most likely be 1.
  if(nrow(rbg_env$result) > 0){
    rbg_env$result[1,3] <- 0
  }
  
  # print(result)
  # return the result frame
  return(rbg_env$result)
}


ProcessLogistic <- function(processing_frame) {
  # go through result frame and add subsequent rows until pos_obs and neg_obs both >= 1
  # do pos_obs first
  vCols <<- c("pos_obs", "neg_obs")
  
  for (j in seq_along(vCols)) {
    result <- data.frame()
    sit <- 1
    current_sum <- 0
    vSppCd <- vector()
    vObs <- vector()
    vMinDc <- vector()
    vMaxDc <- vector()
    vPosObs <- vector()
    vNegObs <- vector()
    intermed_result_frame <- data.frame(species_cd = character(), obs = numeric(), min_dc = numeric(), max_dc = numeric(), pos_obs = numeric(), neg_obs = numeric())
    # this loop takes care of everything except potentially the last row
    while (sit <= nrow(processing_frame)) {
      while (current_sum < 1 && sit <= nrow(processing_frame)) {
        current_sum <- current_sum + sum(processing_frame[sit, vCols[j]])
        vSppCd <- append(vSppCd, processing_frame[sit, c("species_cd")])
        vObs <- append(vObs, processing_frame[sit, c("obs")])
        vMinDc <- append(vMinDc, processing_frame[sit, c("min_dc")])
        vMaxDc <- append(vMaxDc, processing_frame[sit, c("max_dc")])
        vPosObs <- append(vPosObs, processing_frame[sit, c("pos_obs")])
        vNegObs <- append(vNegObs, processing_frame[sit, c("neg_obs")])
        sit <- sit + 1
      }
      tempdf <- data.frame(unlist(vSppCd), unlist(vObs), unlist(vMinDc), unlist(vMaxDc), unlist(vPosObs), unlist(vNegObs))
      colnames(tempdf) <- c("species_cd", "obs", "min_dc", "max_dc", "pos_obs", "neg_obs")
      result_frame <- tempdf %>%
        group_by(species_cd) %>%
        summarise(sum(obs), min(min_dc), max(max_dc), sum(pos_obs), sum(neg_obs))
      colnames(result_frame) <- c("species_cd", "obs", "min_dc", "max_dc", "pos_obs", "neg_obs")
      result <- rbind(result, result_frame)
      
      # clear all the variables for next loop
      intermed_result_frame <- intermed_result_frame[0, ]
      result_frame <- result_frame[0, ]
      tempdf <- tempdf[0, ]
      vSppCd <- vector()
      vObs <- vector()
      vMinDc <- vector()
      vMaxDc <- vector()
      vPosObs <- vector()
      vNegObs <- vector()
      current_sum <- 0
    }
    
    colnames(result) <- c("species_cd", "obs", "min_dc", "max_dc", "pos_obs", "neg_obs")
    if (result[nrow(result), vCols[j]] == 0) {
      processing_frame <- result
      result[nrow(result) - 1, c("obs")] <- result[nrow(result) - 1, c("obs")] + result[nrow(result), c("obs")]
      result[nrow(result) - 1, c("max_dc")] <- result[nrow(result), c("max_dc")]
      result[nrow(result) - 1, vCols[j]] <- result[nrow(result) - 1, vCols[j]]
      result[nrow(result) - 1, vCols[vCols != vCols[j]]] <- result[nrow(result) - 1, vCols[vCols != vCols[j]]] + result[nrow(result), vCols[vCols != vCols[j]]]
      result <- result %>% filter(!row_number() %in% nrow(result))
    }
    processing_frame <- result
  }
  
  # really need to turn this into a function probably called by the function caller
  notcol <- vCols[!(vCols %in% vCols[j])]
  if ((processing_frame[nrow(processing_frame), vCols[j]] == 0) || (processing_frame[nrow(processing_frame), vCols[!(vCols %in% vCols[j])]] == 0)) {
    processing_frame[nrow(processing_frame) - 1, c("obs")] <- processing_frame[nrow(processing_frame) - 1, c("obs")] + processing_frame[nrow(processing_frame), c("obs")]
    processing_frame[nrow(processing_frame) - 1, c("max_dc")] <- processing_frame[nrow(processing_frame), c("max_dc")]
    processing_frame[nrow(processing_frame) - 1, vCols[j]] <- processing_frame[nrow(processing_frame) - 1, vCols[j]]
    processing_frame[nrow(processing_frame) - 1, vCols[vCols != vCols[j]]] <- processing_frame[nrow(processing_frame) - 1, vCols[vCols != vCols[j]]] + processing_frame[nrow(processing_frame), vCols[vCols != vCols[j]]]
    processing_frame <- processing_frame %>% filter(!row_number() %in% nrow(processing_frame))
  }
  return(processing_frame)
}


RegressionBinGenerator <- function(baseframe, measure, bin_size, logit = FALSE, measure_increment = 1, primary_group = "species_cd", merge_final = FALSE, results_filename = NULL){
  if(!is.numeric(measure_increment)){
    stop(paste0("Input 'measure_increment' must be a numeric value. You entered '", measure_increment, "'."))
  }
  if (measure_increment <= 0) {
    stop(paste0("Input 'measure_increment' must be a non-zero, non-negative number. You entered ", measure_increment, "."))
  }
  
  # Create an environment to hold variables
  # Might have to change this to closures in the future but a global child environment shouldn't cause any issues
  rbg_env <<- new.env()
  rbg_env$baseframe <- baseframe
  rbg_env$measure <- measure
  rbg_env$bin_size <- bin_size
  rbg_env$logit <- logit
  rbg_env$measure_increment <- measure_increment
  rbg_env$primary_group = primary_group
  rbg_env$merge_final <- merge_final
  rbg_env$results_filename <- results_filename
  rbg_env$measure_increment_dec <- GetDecimalPlaces(measure_increment)
  rbg_env$result <- data.frame()
  #final_result <- data.frame()

  if(rbg_env$measure_increment_dec > 0 && rbg_env$measure_increment > 1){
    stop(paste0("Input 'measure_increment' must be between 0 and 1 if it contains a decimal value. You entered ", measure_increment, "."))
  }
  #print(primary_group)
  rbg_env$measure_multiplier <- CalculateIncrementMultiplier(rbg_env$measure_increment, rbg_env$measure_increment_dec)
  
  p <- PrepareData()
  
  
  if (length(rbg_env$group_names) > 0) {
    final_result <- data.frame(
      group_name = character(0),
      observations = double(0),
      min_measure = double(0),
      max_measure = double(0),
      pos_observations = double(0),
      neg_observations = double(0)
    )
    
    # Loop through the group names and calculate results for each one
    for (i in rbg_env$group_names) {
      # i == group_names, normally species code
      rbg_env$the_frame <- PrimaryGroupExtractor(i)
      final_result <- union(final_result, ProcessBins(rbg_env$the_frame))
      
      # Empty the_frame so we have a clean slate for subsquent loops
      rbg_env$the_frame <- rbg_env$the_frame[0, ]
    }

    # Add a final midpoint column that is the mid between min_measure and max_measure
    # and then convert back to the original base 
    # The midpoint calculation happens after the min and max columns have been adjusted, hence no need to divide by the multiplier as it has already been done
    final_result <- final_result %>%
      mutate(min_measure = min_measure/rbg_env$measure_multiplier,
             max_measure = max_measure/rbg_env$measure_multiplier,
             mid_measure = (min_measure + max_measure) / 2)
    
    # Reorder the columns for clarity
    final_result <- final_result %>%
      relocate(mid_measure, .after = max_measure)
    
    
    return(final_result)
  }
}

#Data and variable setup function
PrepareData <- function() {
  rbg_env$ana_frame <-
    rbg_env$baseframe %>%
    mutate("act_measure" = floor(rbg_env$measure_multiplier * baseframe[[rbg_env$measure]]), 
           "min_measure" = 0, 
           "max_measure" = 0, 
           "mid_measure" = 0)
  
  rbg_env$group_names <- unique(rbg_env$ana_frame[[rbg_env$primary_group]])
  
  if(length(rbg_env$group_names) < 1){
    #free objects and memory
    #rm(rbg_env)
    #gc()
    #Message user on having no data to process
    stop("No data to process due to null primary_group. Check to ensure that your primary_group variable contains values and is properly named.")
  }
}




# Setup
baseframe <- read.csv("data/test_data_basic.csv", header = TRUE)
#baseframe <- read.csv("data/test_data_basic_mult.csv", header = TRUE)
primary_group <- "group_name"
measure <- "measure_val"
measure_increment <- 1
measure_increment_dec <- 0
measure_multiplier <- 1
group_names <- c()

huh <- RegressionBinGenerator(baseframe, "measure_val", 10, primary_group="group_name")

# create new variables for our calculated measurements
# yeah, I know we don't need a new dataframe but I like to be able to debug easily
ana_frame <-
  baseframe %>%
  mutate("act_measure" = floor(baseframe[[rbg_env$measure]]), "min_measure" = 0, "max_measure" = 0, "mid_measure" = 0)

# vector of spp codes
group_names <- unique(ana_frame[[primary_group]])

a_frame = PrimaryGroupExtractor("tst_spp")
#write.csv(a_frame, "data/pge_stepart.csv")

huh <- RegressionBinGenerator(baseframe, "measure_val", 5)
#huh <- RegressionBinGenerator(baseframe, "measure_val_2", 20, FALSE, .01, "group_name")

mult <- CalculateIncrementMultiplier(1, 0)