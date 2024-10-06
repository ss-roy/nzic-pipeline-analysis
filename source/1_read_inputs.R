
# Script Info -------------------------------------------------------------

# This scripts loads, selects the relevant data from 'NZIC pipeline' and external sources (Treasury)
# Basic transformations, joining of data from different sources

source("source/0_functions.R")

# Read the pipeline data
data <- read_excel(path = "data/pipeline/Pipeline-data-as-at-2024-09-30.xlsx", sheet = 'pipeline-data')

# Load the GDP and CPI data from Treasury
gdp <- read_excel("data/external/treasury/befu24-charts-data.xlsx", sheet = "Data 1.1", range = "B6:D79")  # Adjust row range as needed
cpi <- read_excel("data/external/treasury/befu24-charts-data.xlsx", sheet = "Data 1.2", range = "B6:C86")  # Adjust row range as needed

# Select relevant columns for analysis
df <- data %>% 
  select(PrimaryKey, InformationSubmissionQuarter, ProjectName, ProjectStatus,
         FundingStatus, ProcurementType, ProjectRegion, ProjectCityTown,
         CommissionAssignedInfrastructureSector, EstimatedProjectValueRange,
         EstimatedQuarterBusinessCaseStart, EstimatedQuarterBusinessCaseCompletion,
         EstimatedQuarterConstructionStart, EstimatedQuarterConstructionCompletion,
         EstimatedQuarterProjectRangeStart, EstimatedQuarterProjectRangeCompletion)

# Convert the data frame to avoid factors
projects_data <- data.frame(df, stringsAsFactors = FALSE)
data<-NULL

# Convert estimated quarters to dates using the defined function
projects_data <- projects_data %>%
  mutate(
    EstimatedQuarterBusinessCaseStart = convert_quarter_to_date(EstimatedQuarterBusinessCaseStart),
    EstimatedQuarterBusinessCaseCompletion = convert_quarter_to_date(EstimatedQuarterBusinessCaseCompletion),
    EstimatedQuarterProjectRangeStart = convert_quarter_to_date(EstimatedQuarterProjectRangeStart),
    EstimatedQuarterProjectRangeCompletion = convert_quarter_to_date(EstimatedQuarterProjectRangeCompletion),
    EstimatedQuarterConstructionStart = convert_quarter_to_date(EstimatedQuarterConstructionStart),
    EstimatedQuarterConstructionCompletion = convert_quarter_to_date(EstimatedQuarterConstructionCompletion)
  )

# Rename the columns for macro data
colnames(gdp) <- c("Period", "GDP", "GDP_PC")
colnames(cpi) <- c("Period", "CPI")

# Filter CPI data, Merge with GDP
cpi <- cpi %>% filter(Period >= "2010-06-01")
macros <- left_join(gdp, cpi, by = "Period")

# Check date formats
macros$Period <- as.Date(macros$Period, format = "%m/%d/%Y")
projects_data$EstimatedQuarterProjectRangeStart <- as.Date(projects_data$EstimatedQuarterProjectRangeStart)
projects_data$EstimatedQuarterProjectRangeCompletion <- as.Date(projects_data$EstimatedQuarterProjectRangeCompletion)

