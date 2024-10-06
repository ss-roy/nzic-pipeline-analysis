
# Script Info -------------------------------------------------------------

# This script is essentially the same as 4_linear_model_macro.R
# Only difference is that it allows user to specify which Sector the model needs to run
# Currently it works for Transport", "Water","Community",
# "Industrial and Commercial","Social","Education and Research")

# TODO: Fix other sectors

# You can run the script on the console by using command - system("Rscript source/5_autorrun.R 'X' ") where X is the sector
# Example system("Rscript source/5_autorrun.R 'Transport' ")


# Function to get the CommissionAssignedInfrastructureSector from command-line args or prompt user

get_sector <- function() {
  args <- commandArgs(trailingOnly = TRUE)  # Get command-line arguments
  
  if (length(args) == 0) {
    # If no argument is passed, prompt the user for input
    sector <- readline(prompt = "Please provide the CommissionAssignedInfrastructureSector: ")
  } else {
    sector <- args[1]  # Use the first argument as the sector
  }
  
  # Optionally, validate the sector
  valid_sectors <- c("Transport", "Water","Community",
                     "Industrial and Commercial","Social","Education and Research")
  if (!(sector %in% valid_sectors)) {
    stop(paste("Invalid sector provided. Please choose from:", paste(valid_sectors, collapse = ", ")))
  }
  
  return(sector)
}

# Get the sector from the user input or command-line argument
sector <- get_sector()

# Print the chosen sector for confirmation
cat("Running the script for sector:", sector, "\n")

# Example: Load your preprocessed data
source("source/2_preprocess_features.R")

# Filter for completed projects based on the chosen sector
completed_projects <- projects_data %>%
  filter(complete == 1 & EstimatedQuarterConstructionCompletion <= "2024-10-01") %>%
  filter(CommissionAssignedInfrastructureSector == sector)  # Use the sector from user input

# Calculate avg_group_delay for completed projects
avg_delay_by_region <- completed_projects %>%
  group_by(ProjectRegion) %>%
  summarise(avg_group_delay = mean(total_delay, na.rm = TRUE), .groups = 'drop')

# Join avg_group_delay back to completed_projects
completed_projects <- completed_projects %>%
  left_join(avg_delay_by_region, by = "ProjectRegion")

# Fit the  model
delay_model_2 <- lm(total_delay ~ avg_group_delay + budget_min + budget_max + planned_duration +
                   funding_status_indicator + project_status_indicator + ProjectRegionCode +
                   CPI_Delta_Percent + GDP_Delta_Percent,
                 data = completed_projects)

# Perform stepwise regression to find important features
delay_model_2 <- step(delay_model_2, direction = "both", trace = TRUE)

# Display the summary of the final model
summary(delay_model_2)

# Display the summary of the model to check accuracy and coefficients
summary(delay_model_2)

# Diagnostics: Scale-Location Plot
ggplot(completed_projects, aes(x = fitted(delay_model_2), y = sqrt(abs(residuals(delay_model_2))))) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, color = "blue") +
  labs(title = "Scale-Location Plot",
       x = "Fitted Values",
       y = "Square Root of |Standardized Residuals|") +
  theme_minimal()

# 6. Filter for uncompleted projects based on the sector
incomplete_projects <- projects_data %>%
  filter(complete == 0) %>%
  filter(CommissionAssignedInfrastructureSector == sector)

# 7. Join avg_group_delay data back to incomplete projects
results_model2 <- incomplete_projects %>%
  left_join(avg_delay_by_region, by = "ProjectRegion")

# 8. Add columns for predictions
results_model2 <- results_model2 %>%
  mutate(
    budget_min = as.numeric(gsub("[^0-9]", "", sapply(strsplit(EstimatedProjectValueRange, "-"), `[`, 1))),
    budget_max = as.numeric(gsub("[^0-9]", "", sapply(strsplit(EstimatedProjectValueRange, "-"), `[`, 2))),
    budget_max = coalesce(budget_max, budget_min),
    funding_status_indicator = ifelse(FundingStatus == "Funding source confirmed", 1, 0),
    project_status_indicator = ifelse(ProjectStatus == "In planning", 1, 0),
    planned_duration = EstimatedQuarterProjectRangeCompletion - EstimatedQuarterProjectRangeStart
  )

# 9. Predict delays for uncompleted projects using the model
results_model2$predicted_delay <- predict(delay_model_2, newdata = results_model2)

# 10. View the results with predicted delays
print(results_model2)

# Plot 1: Predicted Delay by Project Region
ggplot(results_model2, aes(x = ProjectRegion, y = predicted_delay, fill = CommissionAssignedInfrastructureSector)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  facet_wrap(~ CommissionAssignedInfrastructureSector, scales = "free_y") +
  labs(title = "Predicted Delay by Project Region and Sector",
       x = "Project Region",
       y = "Predicted Delay") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
