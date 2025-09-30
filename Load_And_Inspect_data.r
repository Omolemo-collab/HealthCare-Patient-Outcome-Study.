# ============================
# Load Libraries
# ============================
library(dplyr)
library(lubridate)
library(survival)

# ============================
# Load Data
# ============================
raw_data <- read.csv("your_file.csv")

# Quick look
head(raw_data)
str(raw_data)
summary(raw_data)

# ============================
# Data Cleaning
# ============================
cleaned_data <- raw_data %>%
  # Standardize gender
  mutate(gender = case_when(
    gender %in% c("M", "Male", "m") ~ "Male",
    gender %in% c("F", "Female", "f") ~ "Female",
    TRUE ~ NA_character_
  )) %>%
  # Replace missing treatment_type / department
  mutate(
    treatment_type = ifelse(is.na(treatment_type), "Unknown", treatment_type),
    department = ifelse(is.na(department), "Unknown", department)
  ) %>%
  # Remove rows missing admission_date or event_status
  filter(!is.na(admission_date) & !is.na(event_status)) %>%
  # Calculate time-to-event in days
  mutate(time = as.numeric(difftime(event_date, admission_date, units = "days"))) %>%
  # Keep valid ages and positive survival times
  filter(age >= 0 & age <= 120, time > 0) %>%
  # Keep only valid event codes
  filter(event %in% c(0, 1)) %>%
  # Remove duplicate patient records
  distinct(patient_id, .keep_all = TRUE)

# Keep only survival-relevant variables
survival_data <- cleaned_data %>%
  select(patient_id, time, event, age, gender, treatment_type, department)

# ============================
# Summary Checks
# ============================
summary(survival_data)
table(survival_data$gender)
table(survival_data$treatment_type)
table(survival_data$event)

# ============================
# Kaplan-Meier Analysis
# ============================
surv_object <- Surv(time = survival_data$time, event = survival_data$event)

# Overall KM
km_fit <- survfit(surv_object ~ 1, data = survival_data)
plot(km_fit,
     xlab = "Time (days)",
     ylab = "Survival Probability",
     main = "Kaplan-Meier Survival Curve",
     col = "blue", lwd = 2)

# KM by gender
km_fit_gender <- survfit(surv_object ~ gender, data = survival_data)
plot(km_fit_gender,
     col = c("blue", "red"), lwd = 2,
     xlab = "Time (days)", ylab = "Survival Probability",
     main = "Survival Curves by Gender")
legend("bottomleft", legend = c("Male", "Female"),
       col = c("blue", "red"), lwd = 2)

# Log-rank test by gender
logrank_test <- survdiff(surv_object ~ gender, data = survival_data)
print(logrank_test)

# ============================
# Kaplan-Meier by Treatment
# ============================
km_fit_treat <- survfit(Surv(time, event) ~ treatment_type, data = survival_data)
plot(km_fit_treat, col = 1:length(unique(survival_data$treatment_type)), lwd = 2,
     xlab = "Time (days)", ylab = "Survival Probability",
     main = "Survival Curves by Treatment")
legend("bottomleft", legend = levels(factor(survival_data$treatment_type)),
       col = 1:length(unique(survival_data$treatment_type)), lwd = 2)

# Log-rank test by treatment
survdiff(Surv(time, event) ~ treatment_type, data = survival_data)

# ============================
# Cox Proportional Hazards Model
# ============================
cox_model <- coxph(Surv(time, event) ~ age + gender + treatment_type,
                   data = survival_data)
summary(cox_model)

# Test proportional hazards assumption
ph_test <- cox.zph(cox_model)
print(ph_test)
plot(ph_test)

# ============================
# Residual Diagnostics
# ============================
residuals_dev <- residuals(cox_model, type = "deviance")
plot(fitted(cox_model), residuals_dev,
     xlab = "Fitted values",
     ylab = "Deviance Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# ============================
# Clean Results Table
# ============================
cox_summary <- summary(cox_model)
results_table <- data.frame(
  Variable = rownames(cox_summary$coefficients),
  Hazard_Ratio = round(exp(cox_summary$coefficients[, "coef"]), 2),
  Lower_CI = round(exp(cox_summary$conf.int[, "lower .95"]), 2),
  Upper_CI = round(exp(cox_summary$conf.int[, "upper .95"]), 2),
  P_value = round(cox_summary$coefficients[, "Pr(>|z|)"], 3)
)
print(results_table)

























































# Export a Cox summary table
cox_summary <- summary(cox_model)$coefficients
write.csv(cox_summary, "cox_summary.csv", row.names = TRUE)

# Export survival plot
png("survival_curve.png")
plot(km_fit, main="Kaplan-Meier Curve")
dev.off()
