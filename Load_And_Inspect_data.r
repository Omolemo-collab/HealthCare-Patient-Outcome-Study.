#Load necessary libaries
library(dplyr)
library(lubridate)

#Load the synthetic data (Assuming its a CSV file)
raw_data <- read.csv("some data")


#Inspect the first row
head(raw_data)

#Check the structure of the dataset
str(raw_data)

#Summary statistics to see missing values and ranges
summary(raw_data)

#Standardize gender coding 
cleaned_data <- raw_data %>%
    mutate (gender = case_when(
        gender %in% c("M", "Male") ~ "Male",
        gender %in% c("F", "Female") ~ "Female",
        TRUE ~ NA_character_
    ))

#Replace missing treatement_type or department with "Unknown"
cleaned_data <- cleaned_data %>%
    mutate(
        treatment_type = ifelse(is.na(treatment_type), "Unknown", treatment_type),
        department = ifelse(is.na(department), "Unknown", department)
    )

# Remove rows missing admission_date or event_status (essential for survival analysis)
raw_data <- raw_data %>%
 filter(!is.na(admission_date) & !is.na(event_status))

#Remove unrealistic ages (keep patients 0 to 120)
survival_data <- survival_data %>%
    filter(age >= 0 & age <= 120)

#Remove negative or zero survival time
survival_data <- survival_data %>%
    filter(time > 0)


#Ensure event column is only 0 or 1
survival_data <- cleaned_data %>%
    filter(event %in% c(0, 1))

