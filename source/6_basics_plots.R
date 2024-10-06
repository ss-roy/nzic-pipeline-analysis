
# Script Info -------------------------------------------------------------

# This script calculates summary statistics (total projects, average delay, average duration, average budget)
#for both completed and planned projects, grouped by ProjectRegion and CommissionAssignedInfrastructureSector.

# Input: output of 2_preprocess_features.R

# Plots:
# Various bar plots to visualize the total number of completed and planned projects by region and sector.
# Heatmaps to represent the distribution of completed and planned projects across different sectors and locations.
# Plots that display average delays and durations for both completed and planned projects.

source("source/2_preprocess_features.R")

# Load and process the data
viz_data <- read.csv("out/processed_with_features.csv")

# Convert estimated quarters to dates using the defined function
viz_data <- viz_data %>%
  mutate(
    EstimatedQuarterBusinessCaseStart = as.Date(EstimatedQuarterBusinessCaseStart),
    EstimatedQuarterBusinessCaseCompletion = as.Date(EstimatedQuarterBusinessCaseCompletion),
    EstimatedQuarterProjectRangeStart = as.Date(EstimatedQuarterProjectRangeStart),
    EstimatedQuarterProjectRangeCompletion = as.Date(EstimatedQuarterProjectRangeCompletion),
    EstimatedQuarterConstructionStart = as.Date(EstimatedQuarterConstructionStart),
    EstimatedQuarterConstructionCompletion = as.Date(EstimatedQuarterConstructionCompletion)
  )

# Separate completed and planned projects
completed_projects <- viz_data %>% filter(complete == 1 & EstimatedQuarterConstructionCompletion <= "2024-10-01")  
planned_projects <- viz_data %>% filter(complete == 0)

# 1. Summary Statistics by ProjectRegion for completed projects
region_summary_completed <- completed_projects %>%
  group_by(ProjectRegion) %>%
  summarise(
    total_projects = n(),
    average_delay = mean(total_delay, na.rm = TRUE),
    average_duration = mean(planned_duration, na.rm = TRUE),
    average_budget_min = mean(budget_min, na.rm = TRUE),
    average_budget_max = mean(budget_max, na.rm = TRUE)
  )

# 1.1 Summary Statistics by ProjectRegion for planned projects
region_summary_planned <- planned_projects %>%
  group_by(ProjectRegion) %>%
  summarise(
    total_projects = n(),
    average_delay = mean(total_delay, na.rm = TRUE),
    average_duration = mean(planned_duration, na.rm = TRUE),
    average_budget_min = mean(budget_min, na.rm = TRUE),
    average_budget_max = mean(budget_max, na.rm = TRUE)
  )

# 2. Plot: Total Projects by ProjectRegion for completed projects
completed_projects_plot <- ggplot(data = region_summary_completed, aes(x = reorder(ProjectRegion, total_projects), y = total_projects)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Total Completed Projects by Project Region",
       x = "Project Region",
       y = "Total Completed Projects")
ggsave("out/plots/1_total_completed_projects_by_region.png", plot = completed_projects_plot, width = 10, height = 6)

# 2.1 Plot: Total Projects by ProjectRegion for planned projects
planned_projects_plot <- ggplot(data = region_summary_planned, aes(x = reorder(ProjectRegion, total_projects), y = total_projects)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Total Planned Projects by Project Region",
       x = "Project Region",
       y = "Total Planned Projects")
ggsave("out/plots/2_total_planned_projects_by_region.png", plot = planned_projects_plot, width = 10, height = 6)

# 3. Summary Statistics by CommissionAssignedInfrastructureSector for completed projects
sector_summary_completed <- completed_projects %>%
  group_by(CommissionAssignedInfrastructureSector) %>%
  summarise(
    total_projects = n(),
    average_delay = mean(total_delay, na.rm = TRUE),
    average_duration = mean(planned_duration, na.rm = TRUE),
    average_budget_min = mean(budget_min, na.rm = TRUE),
    average_budget_max = mean(budget_max, na.rm = TRUE)
  )

# 3.1 Summary Statistics by CommissionAssignedInfrastructureSector for planned projects
sector_summary_planned <- planned_projects %>%
  group_by(CommissionAssignedInfrastructureSector) %>%
  summarise(
    total_projects = n(),
    average_delay = mean(total_delay, na.rm = TRUE),
    average_duration = mean(planned_duration, na.rm = TRUE),
    average_budget_min = mean(budget_min, na.rm = TRUE),
    average_budget_max = mean(budget_max, na.rm = TRUE)
  )

# 4. Plot: Total Projects by CommissionAssignedInfrastructureSector for completed projects
sector_completed_plot <- ggplot(data = sector_summary_completed, aes(x = reorder(CommissionAssignedInfrastructureSector, total_projects), y = total_projects)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +
  labs(title = "Total Completed Projects by Infrastructure Sector",
       x = "Infrastructure Sector",
       y = "Total Completed Projects")
ggsave("out/plots/3_total_completed_projects_by_sector.png", plot = sector_completed_plot, width = 10, height = 6)

# 4.1 Plot: Total Projects by CommissionAssignedInfrastructureSector for planned projects
sector_planned_plot <- ggplot(data = sector_summary_planned, aes(x = reorder(CommissionAssignedInfrastructureSector, total_projects), y = total_projects)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(title = "Total Planned Projects by Infrastructure Sector",
       x = "Infrastructure Sector",
       y = "Total Planned Projects")
ggsave("out/plots/4_total_planned_projects_by_sector.png", plot = sector_planned_plot, width = 10, height = 6)

# 5. Plot: Average Delay by ProjectRegion for completed projects
average_delay_completed_plot <- ggplot(data = region_summary_completed, aes(x = reorder(ProjectRegion, average_delay), y = average_delay)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Average Delay by Project Region (in Days)",
       x = "Project Region",
       y = "Average Delay (Days)")
ggsave("out/plots/5_average_delay_completed.png", plot = average_delay_completed_plot, width = 10, height = 6)

# 5.1 Plot: Average Delay by ProjectRegion for planned projects
average_delay_planned_plot <- ggplot(data = region_summary_planned, aes(x = reorder(ProjectRegion, average_delay), y = average_delay)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  coord_flip() +
  labs(title = "Average Delay by Project Region (in Days)",
       x = "Project Region",
       y = "Average Delay (Days)")
ggsave("out/plots/6_average_delay_planned.png", plot = average_delay_planned_plot, width = 10, height = 6)

# 6.1 Plot: Average Duration by ProjectRegion for planned projects
average_duration_planned_plot <- ggplot(data = region_summary_planned, aes(x = reorder(ProjectRegion, average_duration), y = average_duration)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Average Planned Duration by Project Region (in Days)",
       x = "Project Region",
       y = "Average Duration (Days)")
ggsave("out/plots/7_average_duration_planned.png", plot = average_duration_planned_plot, width = 10, height = 6)

# 7. Count the number of completed projects by ProjectRegion and CommissionAssignedInfrastructureSector
sector_location_summary_completed <- completed_projects %>%
  group_by(ProjectRegion, CommissionAssignedInfrastructureSector) %>%
  summarise(total_projects = n(), .groups = 'drop')

# 7.1 Count the number of planned projects by ProjectRegion and CommissionAssignedInfrastructureSector
sector_location_summary_planned <- planned_projects %>%
  group_by(ProjectRegion, CommissionAssignedInfrastructureSector) %>%
  summarise(total_projects = n(), .groups = 'drop')

# 8. Bar plot: Completed vs. Planned Projects by Region and Sector
completed_vs_planned_plot <- ggplot(data = sector_location_summary_completed, aes(x = ProjectRegion, y = total_projects, fill = CommissionAssignedInfrastructureSector)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Completed Projects by Sector and Location",
       x = "Project Region",
       y = "Total Completed Projects") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~ CommissionAssignedInfrastructureSector)
ggsave("out/plots/8_completed__sector_location.png", plot = completed_vs_planned_plot, width = 10, height = 6)

# 8.1 Bar plot: Planned Projects by Region and Sector
planned_vs_completed_plot <- ggplot(data = sector_location_summary_planned, aes(x = ProjectRegion, y = total_projects, fill = CommissionAssignedInfrastructureSector)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Planned Projects by Sector and Location",
       x = "Project Region",
       y = "Total Planned Projects") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~ CommissionAssignedInfrastructureSector)
ggsave("out/plots/9_planned__projects_sector_location.png", plot = planned_vs_completed_plot, width = 10, height = 6)

# 9. Heatmap: Completed Projects by Region and Sector
completed_heatmap_plot <- ggplot(data = sector_location_summary_completed, aes(x = ProjectRegion, y = CommissionAssignedInfrastructureSector, fill = total_projects)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap of Completed Projects by Sector and Location",
       x = "Project Region",
       y = "Infrastructure Sector",
       fill = "Total Completed Projects") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ CommissionAssignedInfrastructureSector)
ggsave("out/plots/10_completed_heatmap_sector_location.png", plot = completed_heatmap_plot, width = 10, height = 6)

# 9.1 Heatmap: Planned Projects by Region and Sector
planned_heatmap_plot <- ggplot(data = sector_location_summary_planned, aes(x = ProjectRegion, y = CommissionAssignedInfrastructureSector, fill = total_projects)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap of Planned Projects by Sector and Location",
       x = "Project Region",
       y = "Infrastructure Sector",
       fill = "Total Planned Projects") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ CommissionAssignedInfrastructureSector)
ggsave("out/plots/11_planned_heatmap_sector_location.png", plot = planned_heatmap_plot, width = 10, height = 6)

# 10. Extracting budget details for completed projects
completed_projects <- completed_projects %>%
  mutate(
    EstimatedProjectValue = (budget_min + budget_max) / 2,  # Example of calculating estimated project value
    BudgetStatus = case_when(
      budget_min < 0 ~ "Negative Budget",
      budget_max < 0 ~ "Negative Budget",
      TRUE ~ "Positive Budget"
    )
  )

budget_summary_completed <- completed_projects %>%
  group_by(BudgetStatus) %>%
  summarise(
    total_projects = n(),
    average_estimated_value = mean(EstimatedProjectValue, na.rm = TRUE),
    .groups = 'drop'
  )