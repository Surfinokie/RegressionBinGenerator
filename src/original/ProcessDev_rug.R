#David Graham 10/19/25
#R version 4.4.3
#Tidyverse 2.0.0 (dplyr 1.1.4)
#Rugosity version
#Go to line 194 for instructions on execution
library(tidyverse)

#get all rows with the species code and create a new summarised df on which to operate
spp_extractor <- function(spp_code){
  filter(ana_frame, species_cd == spp_code) %>%
    group_by(species_cd, rugcat) %>%
    summarise(n=n(), 
              obs=max(pres), 
              pres_true = sum(ifelse(pres==1, 1, 0)), 
              pres_false = sum(ifelse(pres==0, 1, 0)))
}

process_spp <- function(processing_frame, threshold){
  i <- 1
  while (i <= nrow(processing_frame)){
    current_sum <- 0
    #number of present observations for logistic
    pos_sum <- 0
    #number of not-present observations for logistic
    neg_sum <- 0
    rows_to_add <- list()
    vObs <- vector()
    vPos <- vector()
    vNeg <- vector()
    vSppCd <- vector()
    vRc <- vector()
    
    while (current_sum < threshold && i <= nrow(processing_frame)) {
      #does this have a first row error and always add the first row to the second?
      current_sum <- current_sum + sum(processing_frame[i, c("n")])
      pos_sum <- pos_sum + sum(processing_frame[i, c("pres_true")])
      neg_sum <- neg_sum + sum(processing_frame[i, c("pres_false")])
      
      vObs <- append(vObs, current_sum)
      vPos <- append(vPos, pos_sum)
      vNeg <- append(vNeg, neg_sum)
      vSppCd <- append(vSppCd, processing_frame[i, 1])
      vRc <- append(vRc, processing_frame[i, 2])
      
      #debug code
      # print(paste("vObs: ", length(vObs)))
      # print(paste("vPos: ", length(vPos)))
      # print(paste("vNeg: ", length(vNeg)))
      # print(paste("vSppCd: ", length(vSppCd)))
      # print(paste("vRc: ", length(vRc)))
      
      #end debug
      
      result_frame <- data.frame(unlist(vSppCd), vObs, min(unlist(vRc)), max(unlist(vRc)), vPos, vNeg)
      
      #set the current row's max_dc to the next row's min_dc
      if(!is.na(processing_frame[i+1, 2]))
      {
        result_frame[1, 4] = processing_frame[i+1, 2]
      } else 
      {
        result_frame[1, 4] = processing_frame[i, 2]
      }
      
      rows_to_add <- append(rows_to_add, result_frame)
      i <- i + 1
    }
    
    colnames(result_frame) <- c("species_cd", "obs", "min_rc", "max_rc", "pos_obs", "neg_obs")
    intermed_result_frame <- result_frame %>% 
      group_by(species_cd) %>% 
      summarise(max(obs), 
                min(min_rc), 
                min(min_rc), 
                max(max_rc), 
                sum(pos_obs), 
                sum(neg_obs))
    colnames(intermed_result_frame) <- c("species_cd", "obs", "min_rc", "max_rc", "pos_obs", "neg_obs")
    
    result <- rbind(result, intermed_result_frame)
    
  }
  
  #If the last row is < the threshold, add it to the preceding row, and remove the last
  if(nrow(result) > 1 && result[nrow(result), 2] < threshold){
    result[nrow(result)-1, 2] <- result[nrow(result)-1, 2] + result[nrow(result), 2]
    result[nrow(result)-1, 4] <- result[nrow(result), 4]
    result[nrow(result)-1, 5] <- result[nrow(result)-1, 5] + result[nrow(result), 5]
    result[nrow(result)-1, 6] <- result[nrow(result)-1, 6] + result[nrow(result), 6]
    
    result <- result %>% filter(!row_number() %in% nrow(result))
  }
  
  #print(result)
  
  #make sure we have the minimum threshold values for rugosity categories
  #2 is a magic number right now, turn it into a variable, and it is 2 because we're working with rc's multiplied by 100 at this point
  result <- merge_rows_by_rugosity_threshold(result, "min_rc", "max_rc", 2)
  
  #print(result)
  #return the result frame
  result
}

process_logit <- function(logit_frame){
  #go through result frame and add subsequent rows until pos_obs and neg_obs both >= 1
  #do pos_obs first
  vCols <<- c("pos_obs", "neg_obs")
  
  for(j in seq_along(vCols)){
    result <- data.frame()
    sit<-1
    current_sum <- 0
    vSppCd <- vector()
    vObs <- vector()
    vMinRc <- vector()
    vMaxRc <- vector()
    vPosObs <- vector()
    vNegObs <- vector()
    intermed_result_frame <- data.frame(species_cd=character(), obs=numeric(), min_rc=numeric(), max_rc=numeric(), pos_obs=numeric(), neg_obs=numeric())
    #this loop takes care of everything except potentially the last row
    while(sit<=nrow(logit_frame)){
      while(current_sum < 1 && sit<=nrow(logit_frame)){
        current_sum <- current_sum + sum(logit_frame[sit, vCols[j]])
        vSppCd <- append(vSppCd, logit_frame[sit,c("species_cd")])
        vObs <- append(vObs, logit_frame[sit,c("obs")])
        vMinRc <- append(vMinRc, logit_frame[sit, c("min_rc")])
        vMaxRc <- append(vMaxRc, logit_frame[sit, c("max_rc")])
        vPosObs <- append(vPosObs, logit_frame[sit, c("pos_obs")])
        vNegObs <- append(vNegObs, logit_frame[sit, c("neg_obs")])
        sit<-sit+1
      }
      tempdf <- data.frame(unlist(vSppCd), unlist(vObs), unlist(vMinRc), unlist(vMaxRc), unlist(vPosObs), unlist(vNegObs))
      colnames(tempdf) <- c("species_cd", "obs", "min_rc", "max_rc", "pos_obs", "neg_obs")
      result_frame <- tempdf %>% group_by(species_cd) %>% summarise(sum(obs), min(min_rc), max(max_rc), sum(pos_obs), sum(neg_obs))
      colnames(result_frame) <- c("species_cd", "obs", "min_rc", "max_rc", "pos_obs", "neg_obs")
      result <- rbind(result, result_frame)
      
      #clear all the variables for next loop
      intermed_result_frame <- intermed_result_frame[0, ]
      result_frame <- result_frame[0, ]
      tempdf <- tempdf[0, ]
      vSppCd <- vector()
      vObs <- vector()
      vMinRc <- vector()
      vMaxRc <- vector()
      vPosObs <- vector()
      vNegObs <- vector()
      current_sum <- 0
    }
    
    colnames(result) <- c("species_cd", "obs", "min_rc", "max_rc", "pos_obs", "neg_obs")
    if(result[nrow(result), vCols[j]] == 0){
      logit_frame<-result
      result[nrow(result)-1, c("obs")] <- result[nrow(result)-1, c("obs")] + result[nrow(result), c("obs")]
      result[nrow(result)-1, c("max_rc")] <- result[nrow(result), c("max_rc")]
      result[nrow(result)-1, vCols[j]] <- result[nrow(result)-1, vCols[j]]
      result[nrow(result)-1, vCols[vCols != vCols[j]]] <- result[nrow(result)-1, vCols[vCols != vCols[j]]] + result[nrow(result), vCols[vCols != vCols[j]]]
      result <- result %>% filter(!row_number() %in% nrow(result))
    }
    logit_frame<-result
  }  
  
  #really need to turn this into a function probably called by the function caller
  notcol <- vCols[!(vCols %in% vCols[j])]
  if((logit_frame[nrow(logit_frame), vCols[j]] == 0) || (logit_frame[nrow(logit_frame), vCols[!(vCols %in% vCols[j])]] == 0)){
    logit_frame[nrow(logit_frame)-1, c("obs")] <- logit_frame[nrow(logit_frame)-1, c("obs")] + logit_frame[nrow(logit_frame), c("obs")]
    logit_frame[nrow(logit_frame)-1, c("max_rc")] <- logit_frame[nrow(logit_frame), c("max_rc")]
    logit_frame[nrow(logit_frame)-1, vCols[j]] <- logit_frame[nrow(logit_frame)-1, vCols[j]]
    logit_frame[nrow(logit_frame)-1, vCols[vCols != vCols[j]]] <- logit_frame[nrow(logit_frame)-1, vCols[vCols != vCols[j]]] + logit_frame[nrow(logit_frame), vCols[vCols != vCols[j]]]
    logit_frame <- logit_frame %>% filter(!row_number() %in% nrow(logit_frame))
  }
  
  #make sure we have the minimum threshold values for rugosity categories
  #2 is a magic number right now, turn it into a variable, and it is 2 because we're working with rc's multiplied by 100 at this point
  logit_frame <- merge_rows_by_rugosity_threshold(logit_frame, "min_rc", "max_rc", 2)
  
  return(logit_frame)
}

merge_rows_by_rugosity_threshold <- function(df, min_rc, max_rc, threshold){
  #Ensure the input is a data.frame for base R indexing
  df <- as.data.frame(df)
  
  #initialize the index for the current row we are checking
  i <- 1
  
  #Loop until the index 'i' is at the last row
  while (i < nrow(df)){
    #calculate the absolute difference between the current row (i)
    #and the next row (i+1) for the two specified columns
    diff_val <- abs(df[i, max_rc] - df[i, min_rc])
    
    #check if the difference is less than the threshold
    if(diff_val < threshold){
      # ***** MERGE STEP *****
      #1. Add/adjust the values of the next row (i+1) to the current row (i)
      #species_cd = species_cd
      #obs = obs[i] + obs[i+1]
      #min_rc = min_rc[i]
      #max_rc = max_rc[i+1]
      #pos_obs = pos_obs[i] + pos_obs[i+1]
      #neg_obs = neg_obs[i] + neg_obs[i+1]
      df[i, max_rc] <- df[i+1, max_rc]
      df[i, "pos_obs"] <- df[i, "pos_obs"] + df[i+1, "pos_obs"]
      df[i, "neg_obs"] <- df[i, "neg_obs"] + df[i+1, "neg_obs"]
      
      #2. Remove the next row we just added to the first
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

#for each element in distinct_spp call the function that does all the work and add the result to the final result df
generate_bins <- function(bin_size = 25, logit = FALSE){
  if(length(distinct_spp) > 0){
    for(i in distinct_spp){
      #i == the species_cd
      the_frame <- spp_extractor(i)
      if(which(distinct_spp == i) > 1){
        final_result <- union_all(final_result, process_spp(the_frame, bin_size))
      } else {
        final_result <- process_spp(the_frame, bin_size)
      }
    }
    
    ir <- final_result[0,]
    
    #logistic portion
    if(logit == TRUE){
      if(length(distinct_spp > 0)){
        for(s in distinct_spp){
          the_frame <-filter(final_result, species_cd == s)
          as.data.frame(the_frame)
          ir <- union_all(ir, as.data.frame(process_logit(the_frame)))
        }
      }
      final_result <- ir
    }
    
    #Add a final midpoint column that is the mid between min_dc and max_dc
    final_result <- final_result %>%
      mutate(min_rc = ((min_rc)/100),
             max_rc = ((max_rc)/100),
             midpoint = (min_rc + max_rc)/2)
    #mutate(midpoint = (min_rc + max_rc)/2)
    
    #print(final_result)
    return(final_result)
  } #what to do if not true?
}

####### How to run this#####
# 1. Run or source all the preceding lines
# 2. Run lines 268 - 296 to read in the data and prep the variables
# 3. Generate the result data frame with one of the methods starting at line 298

#variable set up and prep
#Read in the data
#windows
#baseframe <- read.csv("D:\\work\\Analyses\\SAS_to_R\\pr_usvi1623_fish18sppLH_depregdat.csv", header = TRUE)
#mac
#baseframe <- read.csv("../pr_usvi1623_fish17sppLH_ARdat2v2_rug_tst.csv", header = TRUE)
#baseframe <- read.csv("../rugosity/aca_coer_rug_tst.csv", header = TRUE)
baseframe <- read.csv("../rugosity/pr_usvi1623_fish17sppLH_ARdat2v2_rug_tst.csv", header = TRUE)

#create a new variable for depth category (DC) in a new dataframe
#yeah, I know we don't need a new dataframe but I like to be able to debug easily
#rugcat -> rugosity category, functinal equivalent to dc (depth category)
ana_frame <-
  baseframe %>%
  mutate(rugcat = floor(AVG_HARD_RELIEF*100))
#mutate(dc = floor(DEPTH))

#Create new, currently empty columns for the min, max, and midpoint rugosity categories
ana_frame <- ana_frame %>%
  add_column("rc_min"=0, "rc_max"=0, "rc_midpoint"=0)

#vector of spp codes
distinct_spp <- ana_frame %>% 
  distinct(species_cd) %>% 
  pull()

result <- data.frame()
final_result <- data.frame()
the_frame <- data.frame()

#different ways to run this
#my_result <- generate_bins(25)
#my_result <- generate_bins(25, FALSE)
my_result <- generate_bins(40, TRUE)
#my_result <- generate_bins(bin_size = 25)
#my_result <- generate_bins(bin_size = 40, logit=TRUE)

#my_result <- generate_bins(25)

write.csv(my_result, "../rugosity/rugosity_test_40.csv")