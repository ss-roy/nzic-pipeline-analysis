
# Script Info -------------------------------------------------------------

# This scripts loads the libraries and functions used for modelling

# Libraries
library(readxl)      
library(dplyr)       
library(lubridate)   
library(stringr)     
library(ggplot2)     
library(tidyr)       


# Function to convert quarter-year format to date
convert_quarter_to_date <- function(quarter_year) {
  date_values <- as.Date(NA, origin = "1970-01-01")  # Initialize NA for dates
  
  # Loop through the input vector to convert each quarter-year to a date
  for (i in seq_along(quarter_year)) {
    qy <- quarter_year[i]
    
    if (is.na(qy)) {
      date_values[i] <- NA
    } else if (grepl("^[0-9]{4}-Q[1-4]$", qy)) {
      # Extract year and quarter using regex
      matches <- regmatches(qy, regexec("([0-9]{4})-Q([1-4])", qy))
      year <- as.numeric(matches[[1]][2])
      quarter <- as.numeric(matches[[1]][3])
      
      # Convert quarter to a month (1 = January, 4 = October)
      month <- (quarter - 1) * 3 + 1
      
      # Set the first day of the quarter
      date_values[i] <- as.Date(paste(year, month, 1, sep = "-"))
    } else {
      cat("Invalid Quarter-Year format:", qy, "\n")
      date_values[i] <- NA
    }
  }
  
  return(date_values)
}



# Function to find the nearest date and corresponding values
find_nearest <- function(date, df) {
  diff <- abs(as.numeric(df$Period - date))
  index <- which.min(diff)
  return(df[index, c("GDP", "CPI")])
}