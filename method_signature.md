BinnedDataGenerator
DataBinGenerator

measure
default = depth
Depth
Rugosity
The main variable over which to calculate bin sizes.

bin_size
default = 25
Minimum bin size for observations

logit
default = FALSE
Whether this is for logistic regression or not. True triggers requiring at least 1 positive and 1 negative observation in addition to meeting the bin_size minimum.

measure_threshold
default = 0

merge_final
default = FALSE
whether to merge results with original df or not

results_filename
default = NULL
if null, results to df, if a filepath, save results to that filename

primary_group
default = species_cd
The primary variable to group by

Do we need a partition value for the measure, e.g. 0-1, 0-5 (5-10, 10-15, ...)?

BinnedDataGenerator(measure, bin_size, logit, measure_threshold, primary_group = "species_cd", merge_final = FALSE, results_filename = NULL)