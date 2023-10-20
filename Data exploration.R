#installing the packages
install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("tidyverse")
library("dplyr")
library("readxl")
library("ggplot2")
library("tidyverse")
#importing the dataset
data_set <- read.csv("weatherAUS.csv")
#To know the dimensions of dataset
dim(data_set)
#To know the structure of data set
str(data_set)
#To know head and tail of data set
head(data_set)
tail(data_set)
#To filter the data to keep the observations ONLY 2017 
data_set$Date <- as.Date(data_set$Date)
filtered_data <- data_set %>% filter(Date >= as.Date("2017-01-01") & Date <= as.Date("2017-06-20"))
dim(filtered_data)
#To know the total number of missing values
sum(is.na(filtered_data))
#To know the number of missing values in each column
missing_values <- colSums(is.na(filtered_data))
# To print the missing values 
print(missing_values)
#To remove rows with missing values in the target variable (RainTomorrow)
cleaned_data <- filtered_data %>% filter(!is.na(RainTomorrow))
#Replacing missing values for numeric variables with median and for categorical variables with mode
numeric_variable <- c("MinTemp", "MaxTemp", "Rainfall", "WindGustSpeed", "WindSpeed9am", "WindSpeed3pm", "Humidity9am", "Humidity3pm", "Pressure9am", "Pressure3pm", "Temp9am", "Temp3pm")
for(var in numeric_variable) {
  filtered_data[[var]] <- ifelse(is.na(filtered_data[[var]]), median(filtered_data[[var]], na.rm = TRUE), filtered_data[[var]])
}
factor_variable <- c("RainToday", "RainTomorrow")
for(var in factor_variable) {
  filtered_data[[var]] <- ifelse(is.na(filtered_data[[var]]), mode(filtered_data[[var]], na.rm = TRUE), filtered_data[[var]])
}

# Remove unnecessary columns (e.g., location, wind gust direction, etc.)
cols_to_remove <- c("Location", "WindGustDir", "WindDir9am", "WindDir3pm", "Evaporation", "Sunshine")
my_data <- my_data %>% select(-cols_to_remove)




