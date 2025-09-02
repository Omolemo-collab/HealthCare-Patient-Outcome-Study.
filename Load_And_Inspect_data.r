#Load necessary libaries
library(dplyr)
library(lubridate)

#Load the synthetic data (Assuming its a CSV file)
raw_Data<- read.CSV()

#Inspect the first row
head(raw_Data)

#Check the structure of the dataset
str(raw_Data)

#Summary statistics to see missing values and ranges
summary(raw_Data)