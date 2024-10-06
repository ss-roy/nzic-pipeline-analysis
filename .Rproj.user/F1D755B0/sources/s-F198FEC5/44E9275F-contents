
# Script Info -------------------------------------------------------------


# This script fits a linear model for features generated from pipeline data

# A linear regression model (delay_model_1) is fitted to predict total_delay based on several features, including:
# avg_group_delay (average regional delay), budget_min and budget_max (estimated project budget range), 
# planned_duration (planned project duration), funding_status_indicator (whether funding is confirmed),
# project_status_indicator (project’s planning status).

# Diagnostic plots (Scale-Location plot, residuals histogram) are generated to check model assumptions like homoscedasticity and normality of residuals.
# Additional visualizations show residuals vs. fitted values and predicted vs. actual values for further model validation.

# The predicted delay values are scaled within each project region and sector.
# A likelihood score is computed by normalizing the predicted delays on a scale of 0 to 100%. 
# This gives a measure of the probability of project completion relative to delay

# Scaling the predicted delays between 0 and 1 for each region and sector, 
# converting them to percentages to represent the "likelihood" of project completion.

# Boxplots and violin plots are generated to visualize:
# Predicted delays across regions and sectors.
# Likelihood of project completion, faceted by sector and region.


source("source/2_preprocess_features.R")

# Filter for Completed Projects

completed_projects <- projects_data %>%
  filter(complete == 1 & EstimatedQuarterConstructionCompletion <= "2024-10-01")  

# Calculate avg_group_delay for completed projects
avg_delay_by_region <- completed_projects %>%
  group_by(ProjectRegion) %>%  # Group by project region
  summarise(avg_group_delay = mean(total_delay, na.rm = TRUE), .groups = 'drop')  # Calculate average delay per region

# Join avg_group_delay back to completed_projects
completed_projects <- completed_projects %>%
  left_join(avg_delay_by_region, by = "ProjectRegion")  # Merge average delay back to completed projects

# Fit the linear regression model
delay_model_1 <- lm(total_delay ~ avg_group_delay + budget_min + budget_max + planned_duration +
                    funding_status_indicator + project_status_indicator, data = completed_projects)

# Display the summary of the model to check accuracy and coefficients
summary(delay_model_1)

# Diagnostics - Scale-Location Plot to check homoscedasticity
ggplot(completed_projects, aes(x = fitted(delay_model_1), y = sqrt(abs(residuals(delay_model_1))))) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE, color = "blue") +
  labs(title = "Scale-Location Plot",
       x = "Fitted Values",
       y = "Square Root of |Standardized Residuals|") +
  theme_minimal()

# Residuals Histogram to check normality of residuals
ggplot(data.frame(residuals = residuals(delay_model_1)), aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

# Multiple plots using gridExtra for better visualization

# Residuals vs Fitted Values Plot
p1 <- ggplot(completed_projects, aes(x = fitted(delay_model_1), y = residuals(delay_model_1))) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Predicted vs Actual Values Plot
p2 <- ggplot(completed_projects, aes(x = total_delay, y = fitted(delay_model_1))) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Values",
       x = "Actual Total Delay",
       y = "Predicted Total Delay") +
  theme_minimal()

# Arrange the two plots in a grid layout
# p3<-grid.arrange(p1, p2, ncol = 2)
# ggsave("out/plots/12_lm1_diagnostics.png", plot = p3, width = 10, height = 6)


# Predicting delay/ likelihood for uncompleted projects -------------------

# Filter for Uncompleted Projects
incomplete_projects <- projects_data %>%
  filter(complete == 0)  # Select projects that are not completed

# 7. Calculate avg_group_delay for uncompleted projects
results_model1 <- incomplete_projects %>%
  left_join(avg_delay_by_region, by = "ProjectRegion")  # Join average delay data

# 8. Create missing columns for the prediction
results_model1 <- results_model1 %>%
  mutate(
    budget_min = as.numeric(gsub("[^0-9]", "", sapply(strsplit(EstimatedProjectValueRange, "-"), `[`, 1))),  # Extract minimum budget for incomplete projects
    budget_max = as.numeric(gsub("[^0-9]", "", sapply(strsplit(EstimatedProjectValueRange, "-"), `[`, 2))),  # Extract maximum budget for incomplete projects
    budget_max = coalesce(budget_max, budget_min),  # Use min if max is NA
    funding_status_indicator = ifelse(FundingStatus == "Funding source confirmed", 1, 0),  # Funding status indicator
    project_status_indicator = ifelse(ProjectStatus == "In planning", 1, 0),  # Project status indicator
    planned_duration = EstimatedQuarterProjectRangeCompletion - EstimatedQuarterProjectRangeStart  # Calculate planned duration
  )

# Predict delays for uncompleted projects using the model
results_model1$predicted_delay <- predict(delay_model_1, newdata = results_model1)  # Generate predictions

# Scale the predicted delays between 0 and 1 for each sector and convert to percent
results_model1 <- results_model1 %>%
  group_by(ProjectRegion, CommissionAssignedInfrastructureSector) %>%  # Group by region and sector
  mutate(
    # Check if there are any non-NA values in predicted_delay
    non_na_count = sum(!is.na(predicted_delay)),
    
    # Calculate min and max of predicted delays for the group if there are non-NA values
    min_predicted_delay = ifelse(non_na_count > 0, min(predicted_delay, na.rm = TRUE), NA),
    max_predicted_delay = ifelse(non_na_count > 0, max(predicted_delay, na.rm = TRUE), NA),
    
    # Scale the predicted delays and convert to percent
    likelihood = case_when(
      is.na(predicted_delay) ~ NA_real_,  # Preserve NAs
      min_predicted_delay == max_predicted_delay ~ 50,  # Assign 50% if all values are the same
      TRUE ~ ((predicted_delay - min_predicted_delay) / (max_predicted_delay - min_predicted_delay)) * 100  # Normal scaling to percent
    )
  ) %>%
  ungroup() %>%
  select(-min_predicted_delay, -max_predicted_delay, -non_na_count)  # Optionally remove temporary columns

# Clean up the incomplete projects data frame by removing unnecessary columns
results_model1 <- results_model1 %>%
  select(-InformationSubmissionQuarter, -complete, -completedonorbefore,
         -total_delay, -actual_duration, -delay_ratio, -budget_min, -budget_max,
         -project_status_indicator, -completed_on_time)

print(results_model1)

# Plot 1: Predicted Delay Faceted by Region and Sector
p1_lm1 <- ggplot(results_model1, aes(x = ProjectRegion, y = predicted_delay, fill = CommissionAssignedInfrastructureSector)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  # Boxplot to show distribution of predicted delay
  facet_wrap(~ CommissionAssignedInfrastructureSector, scales = "free_y") +  # Create facets by sector
  labs(title = "Predicted Delay by Project Region and Sector",
       x = "Project Region",
       y = "Predicted Delay") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis text for better readability
ggsave("out/plots/13_lm1_predicted_delay.png", plot = p1_lm1, width = 16, height = 12)

# Plot 2: Likelihood Faceted by Region and Sector
p2_lm1 <- ggplot(results_model1, aes(x = ProjectRegion, y = likelihood, fill = CommissionAssignedInfrastructureSector)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  # Boxplot for likelihood
  facet_wrap(~ CommissionAssignedInfrastructureSector, scales = "free_y") +  # Create facets by sector
  labs(title = "Likelihood of Project Completion by Project Region and Sector",
       x = "Project Region",
       y = "Likelihood (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis text for better readability
ggsave("out/plots/13a_lm1_predicted_likelihood.png", plot = p2_lm1, width = 16, height = 12)

# Violin Plot for Predicted Delay - Provide a richer view of the distribution of the data compared to boxplots
p1_violin_lm1 <- ggplot(results_model1, aes(x = ProjectRegion, y = predicted_delay, fill = CommissionAssignedInfrastructureSector)) +
  geom_violin(trim = TRUE, alpha = 0.6) +  # Violin plot for predicted delay
  facet_wrap(~ CommissionAssignedInfrastructureSector, scales = "free_y") + 
  labs(title = "Predicted Delay by Project Region and Sector",
       x = "Project Region",
       y = "Predicted Delay") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("out/plots/14_lm1_predicted_delay_violin.png", plot = p1_violin_lm1, width = 16, height = 12)

# Violin Plot for Likelihood
p2_violin_lm1 <- ggplot(results_model1, aes(x = ProjectRegion, y = likelihood, fill = CommissionAssignedInfrastructureSector)) +
  geom_violin(trim = TRUE, alpha = 0.6) +  # Violin plot for likelihood
  facet_wrap(~ CommissionAssignedInfrastructureSector, scales = "free_y") + 
  labs(title = "Likelihood of Project Completion by Project Region and Sector",
       x = "Project Region",
       y = "Likelihood (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("out/plots/14a_lm1_predicted_likelihood_violin.png", plot = p2_violin_lm1, width = 16, height = 12)

# Print the plots
print(p1_lm1)
print(p2_lm1)
print(p1_violin_lm1)
print(p2_violin_lm1)

# write.csv(results_model1,"out/results_model1.csv",row.names = FALSE)
