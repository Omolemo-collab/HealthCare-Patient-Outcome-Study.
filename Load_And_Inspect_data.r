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

#Remove duplicate patient records
survival_data <- survival_data %>%
    distinct(patient_id, .keep_all = TRUE)

#Survival Variables
survival_data <- survival_data %>%
    mutate(
        #calculates follow up time in days
        time_to_event = as.numeric(difftime(event_date, 
        admission_date, units = "days")),

        #events are in binary (0 = censored, 1 = event)
        event_status = ifelse(event %in% c(1), 1, 0)
    )

#Cleaning gender Data
 library((dplyr))

 cleaned_data <- raw_data %>%
    mutate(gender = case_when(
        gender %in% c("M","Male", "m")~ "Male",
        gender %in% c("F","Female", "f") ~ "Female",
        TRUE ~ NA_character_
    ))

#Handling missing trearment type and department

cleaned_data<- cleaned_data %>%
    mutate(
        treatment_type = ifelse(is.na (treatment_type),
        "Unknown", treatment_type), 
        department = ifelse(is.na(department), "Unknown", department)
    )

#Handling missing admission date or event status

cleaned_data <- cleaned_data %>%
    filter (!is.na(admission_date) & !is.na(event_status))

#Cleaning survival time and event data

cleaned_data <- cleanded_data %>% # removed rows with zero or negative values
    filter(time > 0)

cleaned_data <- cleaned_data %>% # Keep valid event code (0 = censored), (1 = event)
    filter(event %in% c(0,1))

survival_data <- cleaned_data %>%
    select(patient_id, time, event, age, gender, treatment_type,
    department)

print(head(survival_data, 10))


#Summaries
# min, max, median, quartiles for numeric columns (like age, time)
summary(survival_data) 

table(survival_data$gender) # Counts Male vs Females

#Counts no. of patients that got each treatment

table(survival_data$treatment_type)

#Shows how many patients have a censored (0) or event (1)
table(survival_data$event)

#Survival analysis step 1 ~ Summary of data

library(survival)

surv_object <- Surv(time = survival_data$time, event = survival_data$event)

head(surv_object, 10)

#Survival analysis step 2 ~ Kaplan Meier Survival curve

km_fit <- survfit (surv_object ~ 1, data = survival_data)

plot(km_fit,
    xlab = "Time(days)",
    ylab = "Survival Probability",
    main = "Kaplan-Meier Survival Curve",
    col ="blue",
    lwd = 2)

summary(km_fit)