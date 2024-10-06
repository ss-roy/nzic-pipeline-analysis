
# Script Info -------------------------------------------------------------

# This script creates the output file
# Output file consists of ProjectID, Name of project, location, estimated completion date
# and finally the total_delay and likelihood of completing it on time.

# source("source/3_linear_model.R")
source("source/4_linear_model_macro.R")


results_summary <- read.csv("out/results_model_2.csv")

results_summary <- results_summary %>% select(PrimaryKey,ProjectName,ProjectRegion,
                                              EstimatedQuarterProjectRangeCompletion, predicted_delay, likelihood)
write.csv(results_summary,"out/final_predictions_summary.csv",row.names = FALSE)
