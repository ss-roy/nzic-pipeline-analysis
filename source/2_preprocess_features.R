
# Script Info -------------------------------------------------------------

# This script creates new features that cab be used for modelling.
# Features are as follows,
# complete: A binary indicator (1 or 0) showing whether the project is complete based on the estimated construction completion date.completedonorbefore: A binary indicator (1 or 0) indicating if the project was completed on or before the estimated completion date.
# total_delay: A numeric value representing the total delay, calculated as the difference between actual construction completion and estimated project range completion. This is only for projects that are completed
# planned_duration: The planned duration, calculated as the difference between estimated project range completion and start.
# actual_duration: The actual duration, determined based on completion status:If complete, from project range start to construction completion.f incomplete, from project range start to estimated completion.
# delay_ratio: The ratio of total delay to actual duration.
# budget_min: The minimum budget extracted from EstimatedProjectValueRange.
# budget_max: The maximum budget extracted from EstimatedProjectValueRange, or the minimum if the maximum is NA.
# funding_status_indicator: A binary indicator (1 or 0) for confirmed funding sources.
# project_status_indicator: A binary indicator (1 or 0) for projects in planning.
# completed_on_time: A binary indicator (1 or 0) showing if the project was completed on time (estimated completion equals actual completion)


source("source/1_read_inputs.R")

# 1. Create Features
projects_data <- projects_data %>%
  # Filter out rows where the project range start is after the completion date
  filter(EstimatedQuarterProjectRangeStart <= EstimatedQuarterProjectRangeCompletion) %>%
  # Create new columns based on conditions
  mutate(
    complete = ifelse(!is.na(EstimatedQuarterConstructionCompletion), 1, 0),  # 1 if complete, else 0
    completedonorbefore = ifelse(EstimatedQuarterProjectRangeCompletion >= EstimatedQuarterConstructionCompletion, 1, 0),  # 1 if completed on or before the estimated completion
    total_delay = as.numeric(EstimatedQuarterConstructionCompletion - EstimatedQuarterProjectRangeCompletion),  # Calculate total delay
    planned_duration = EstimatedQuarterProjectRangeCompletion - EstimatedQuarterProjectRangeStart,  # Duration from start to completion
    actual_duration = ifelse(complete == 1, 
                             EstimatedQuarterConstructionCompletion - EstimatedQuarterProjectRangeStart,  
                             EstimatedQuarterProjectRangeCompletion - EstimatedQuarterProjectRangeStart),  # Calculate actual duration
    delay_ratio = total_delay / actual_duration,  # Calculate delay ratio
    budget_min = as.numeric(gsub("[^0-9]", "", sapply(strsplit(EstimatedProjectValueRange, "-"), `[`, 1))),  # Extract minimum budget
    budget_max = as.numeric(gsub("[^0-9]", "", sapply(strsplit(EstimatedProjectValueRange, "-"), `[`, 2))),  # Extract maximum budget
    budget_max = coalesce(budget_max, budget_min),  # Use min if max is NA
    funding_status_indicator = ifelse(FundingStatus == "Funding source confirmed", 1, 0),  # Indicator for funding status
    project_status_indicator = ifelse(ProjectStatus == "In planning", 1, 0),  # Indicator for project status
    completed_on_time = ifelse(EstimatedQuarterProjectRangeCompletion == EstimatedQuarterConstructionCompletion, 1, 0)  # Indicator for on-time completion
  )

# Create a named vector for recoding regions
region_recoding <- c(
  "Auckland" = 1,
  "Bay of Plenty" = 2,
  "Canterbury" = 3,
  "Gisborne" = 4,
  "Hawke's Bay" = 5,
  "Manawatū - Whanganui" = 6,
  "Marlborough" = 7,
  "Nelson" = 8,
  "North Island" = 9,
  "Northland" = 10,
  "Otago" = 11,
  "South Island" = 12,
  "Southland" = 13,
  "Taranaki" = 14,
  "Tasman" = 15,
  "Waikato" = 16,
  "West Coast" = 17,
  "Wellington" = 18,
  "Other" = 19,
  "Nationwide" = 20  # Add more regions as necessary
)

# Recode the ProjectRegion column to numeric
projects_data <- projects_data %>%
  mutate(ProjectRegionCode = region_recoding[ProjectRegion])


# Apply the function for start and completion dates
projects_data <- projects_data %>%
  rowwise() %>%
  mutate(
    GDP_Start = find_nearest(EstimatedQuarterProjectRangeStart, macros)[1],
    CPI_Start = find_nearest(EstimatedQuarterProjectRangeStart, macros)[2],
    GDP_End = find_nearest(EstimatedQuarterProjectRangeCompletion, macros)[1],
    CPI_End = find_nearest(EstimatedQuarterProjectRangeCompletion, macros)[2]
  ) %>%
  ungroup()

# Calculate deltas for GDP and CPI
projects_data <- projects_data %>%
  mutate(
    GDP_Delta_Percent = ((GDP_End - GDP_Start) / GDP_Start) * 100,
    CPI_Delta_Percent = ((CPI_End - CPI_Start) / CPI_Start) * 100
  )

# Prep for modelling
projects_data$GDP_Start <- as.numeric(unlist(projects_data$GDP_Start))
projects_data$GDP_End <- as.numeric(unlist(projects_data$GDP_End))
projects_data$CPI_Start <- as.numeric(unlist(projects_data$CPI_Start))
projects_data$CPI_End <- as.numeric(unlist(projects_data$CPI_End))
projects_data$GDP_Delta_Percent <-as.numeric(unlist(projects_data$GDP_Delta_Percent))
projects_data$CPI_Delta_Percent <-as.numeric(unlist(projects_data$CPI_Delta_Percent))
projects_data$ProjectRegionCode <-as.numeric(unlist(projects_data$ProjectRegionCode))


# write.csv(projects_data,"out/processed_with_features.csv", row.names = FALSE)

