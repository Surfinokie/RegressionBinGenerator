# David Graham 11/8/25
# R version 4.4.3
# Tidyverse 2.0.0 (dplyr 1.1.4)
# Go to line 194 for instructions on execution
library(tidyverse)

# Get all rows with the species code and create a new summarised df on which to operate
PrimaryGroupExtractor <- function(primary_group_value) {
  filter(ana_frame, !!sym(primary_group) == primary_group_value) %>%
    group_by(act_measure) %>%
    summarise(
      n = n(),
      obs = max(pres),
      pres_true = sum(ifelse(pres == 1, 1, 0)),
      pres_false = sum(ifelse(pres == 0, 1, 0))
    )
}

# Setup
baseframe <- read.csv("data/orig_test_data.csv", header = TRUE)
primary_group <- "species_cd"
measure <- "DEPTH"
measure_threshold <- 0
group_names <- c()

# create new variables for our calculated measurements
# yeah, I know we don't need a new dataframe but I like to be able to debug easily
ana_frame <-
  baseframe %>%
  mutate("act_measure" = floor(baseframe[[measure]]), "min_measure" = 0, "max_measure" = 0, "mid_measure" = 0)

# vector of spp codes
group_names <- unique(ana_frame[[primary_group]])

a_frame = PrimaryGroupExtractor("STE PART")
write.csv(a_frame, "data/pge_stepart.csv")
