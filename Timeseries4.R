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
data_set <-  read.csv("weatherAUS.csv")

#To know the dimensions of dataset
dim(data_set)

#To know the structure of data set
str(data_set)

#To know head and tail of data set
head(data_set)
tail(data_set)

#To know the total number of missing values
sum(is.na(data_set))

#To know the number of missing values in each column
missing_values <- colSums(is.na(data_set))

# To print the missing values 
print(missing_values)

#To remove rows with missing values in the target variable (RainTomorrow)
newdata <- data_set %>% filter(!is.na(RainTomorrow))

#To remove rows with missing values in the target variable (RainToday)
newdata1 <- newdata %>% filter(!is.na(RainToday))

#to check the missing values
sum(is.na(newdata1))

#to check incomplete records
newdata1 %>% filter(!complete.cases(.)) %>%
  view()

#converting the categorical Rain tomorrow and Rain today variable into Numrical variable
newdata1$num_var <- ifelse(newdata1$RainTomorrow == "Yes", 1, 0)
newdata1$num_var1 <- ifelse(newdata1$RainToday == "Yes", 1, 0)

#to filter out the numerical values in the data
mydata <-newdata1 %>% select_if(is.numeric)

# Replacing the missing values with median of all quantitative variable
for (var in names(mydata)) {
  mydata[[var]][is.na(mydata[[var]])] <- median(mydata[[var]], na.rm = TRUE)
}
#to confirm that there is no missing values
sum(is.na(mydata))

#instaliing correlation pplot package
install.packages("corrplot")
library(corrplot)

#Finding the correlation
cor_data = cor(mydata)
print(cor_data)

#plotting the values in a matrix form
corrplot(cor_data, method="number")


# plot of mean rainfall across the cities in Australia
ggplot(data_set, 
       aes(x = data_set$Location, 
           y = data_set$Rainfall)) +
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90))

# creating a new dataset by filtering with Cairns location to perform time series analysis
timedata <- subset(newdata1,Location == "Cairns",c("Date","Rainfall"))
head(timedata)

install.packages("lubridate")
library(lubridate)

class(timedata$Date)
str(timedata)

# Convert the date variable to yyyy-mm-dd format
timedata$Date<-dmy(timedata$Date)
view(timedata)


#filtering data with same time line
newtimedata <- timedata %>% filter(Date >= as.Date("2009-01-01") & Date <= as.Date("2016-12-31"))
str(newtimedata)

newtimedata$Month <- format(newtimedata$Date, "%m")
newtimedata$Year <- format(newtimedata$Date, "%Y")

monthly_mean <- aggregate(Rainfall ~ Month + Year, data = newtimedata, mean)
view(monthly_mean)
library(tidyr)

monthly_mean_complete <- complete(monthly_mean, Month, Year, fill = list(Rainfall = NA))

mean_rainfall <- mean(newtimedata$Rainfall, na.rm = TRUE)

monthly_mean_complete$Rainfall[is.na(monthly_mean_complete$Rainfall)] <- mean_rainfall


install.packages("xts")
library(xts)

# Create a time series object from the monthly_mean data
ts_data <- ts(monthly_mean$Rainfall, start = c(2009, 1), frequency = 12)


# Plot the time series data
plot(ts_data, main = "Monthly Mean Rainfall", ylab = "Rainfall (mm)")

ggseasonplot(ts_data)
# Decompose the time series into its trend, seasonal, and random components
decomp <- decompose(ts_data, type="multiplicative")

# Plot the decomposition
plot(decomp)

# Split the time series data into training and test data
train_data <- window(ts_data, start = c(2009, 1), end = c(2014, 12))
test_data <- window(ts_data, start = c(2015, 1))

# Plot the training and test data
plot(train_data, main = "Monthly Mean Rainfall (Training Data)", ylab = "Rainfall (mm)")
lines(test_data, col = "blue")

# Load required libraries
library(forecast)
library(tseries)

# Fit seasonal naive model
snaive_model <- snaive(train_data)
snaive_forecast <- forecast(snaive_model, h = length(test_data))


# Fit drift model
drift_model <- ets(train_data, model = "AAN", damped = TRUE, alpha = 0.2, beta = NULL, phi = NULL)
drift_forecast <- forecast(drift_model, h = length(test_data))



# Fit ARIMA model
arima_model <- auto.arima(train_data)
arima_forecast <- forecast(arima_model, h = length(test_data))


# Fit TSLM model
tslm_model <- tslm(train_data ~ trend + season)
tslm_forecast <- forecast(tslm_model, h = length(test_data))


# RMSE for Seasonal NAive model
snaive_rmse <- sqrt(mean((snaive_forecast$mean - test_data)^2))

# RMSE for Drift model
drift_rmse <- sqrt(mean((drift_forecast$mean - test_data)^2))

# RMSE for ARIMA model
arima_rmse <- sqrt(mean((arima_forecast$mean - test_data)^2))

# RMSE for TSLM model
tslm_rmse <- sqrt(mean((tslm_forecast$mean - test_data)^2))

# MAE for Seasonal Naive model
snaive_mae <- mean(abs(snaive_forecast$mean - test_data))

# MAE for Drift model
drift_mae <- mean(abs(drift_forecast$mean - test_data))

# MAE for ARIMA model
arima_mae <- mean(abs(arima_forecast$mean - test_data))

# MAE for TSLM model
tslm_mae <- mean(abs(tslm_forecast$mean - test_data))

# MAPE for Seasonal Naive model
snaive_mape <- mean(abs(snaive_forecast$mean - test_data) / test_data) * 100

# MAPE for Drift model
drift_mape <- mean(abs(drift_forecast$mean - test_data) / test_data) * 100

# MAPE for ARIMA model
arima_mape <- mean(abs(arima_forecast$mean - test_data) / test_data) * 100

# MAPE for TSLM model
tslm_mape <- mean(abs(tslm_forecast$mean - test_data) / test_data) * 100

# SMAPE for Seasonal Naive model
snaive_smape <- mean(2 * abs(snaive_forecast$mean - test_data) / (abs(snaive_forecast$mean) + abs(test_data))) * 100

# SMAPE for Drift model
drift_smape <- mean(2 * abs(drift_forecast$mean - test_data) / (abs(drift_forecast$mean) + abs(test_data))) * 100

# SMAPE for ARIMA model
arima_smape <- mean(2 * abs(arima_forecast$mean - test_data) / (abs(arima_forecast$mean) + abs(test_data))) * 100

# SMAPE for TSLM model
tslm_smape <- mean(2 * abs(tslm_forecast$mean - test_data) / (abs(tslm_forecast$mean) + abs(test_data))) * 100


# Create a data frame to store the evaluation metrics for each model
model_eval <- data.frame(
  Model = c("Seasonal Naive", "Drift", "ARIMA", "TSLM"),
  RMSE = c(snaive_rmse, drift_rmse, arima_rmse, tslm_rmse),
  MAE = c(snaive_mae, drift_mae, arima_mae, tslm_mae),
  MAPE = c(snaive_mape, drift_mape, arima_mape, tslm_mape),
  sMAPE = c(snaive_smape, drift_smape, arima_smape, tslm_smape)
)

# Print the evaluation metrics for each model
print(model_eval)



# View the forecasted values
print(snaive_forecast$mean)

# Extract forecasted values for test set
snaive_fc <- snaive_forecast$mean[length(train_data)+1:length(test_data)]

# Compare with actual values
comparison <- cbind(test_data, snaive_forecast$mean)
colnames(comparison) <- c("Actual", "Forecasted")
# Print comparison table
print(comparison)

# Calculate accuracy measures
snaive_acc <- accuracy(snaive_forecast, test_data)
drift_acc <- accuracy(drift_forecast, test_data)
arima_acc <- accuracy(arima_forecast, test_data)
tslm_acc <- accuracy(tslm_forecast, test_data)

# Print the accuracy measures
print(paste("SNaive model accuracy measures: MAE =", round(snaive_acc[2, "MAE"], 4), ", RMSE =", round(snaive_acc[2, "RMSE"], 4), ", MAPE =", round(snaive_acc[2, "MAPE"], 4)))
print(paste("Drift model accuracy measures: MAE =", round(drift_acc[2, "MAE"], 4), ", RMSE =", round(drift_acc[2, "RMSE"], 4), ", MAPE =", round(drift_acc[2, "MAPE"], 4)))
print(paste("Mean model accuracy measures: MAE =", round(arima_acc[2, "MAE"], 4), ", RMSE =", round(arima_acc[2, "RMSE"], 4), ", MAPE =", round(arima_acc[2, "MAPE"], 4)))
print(paste("TSLM model accuracy measures: MAE =", round(tslm_acc[2, "MAE"], 4), ", RMSE =", round(tslm_acc[2, "RMSE"], 4), ", MAPE =", round(tslm_acc[2, "MAPE"], 4)))

plot(snaive_forecast)
plot(arima_forecast)
plot(drift_forecast)
plot(tslm_forecast)

library(ggplot2)

# Combine training and test data for plotting
all_data <- c(train_data, test_data)
dates <- seq(as.Date("2015-01-01"), as.Date("2016-12-01"), by = "month")
data_df <- data.frame(dates, test_data)

library(forecast)

library(ggplot2)

ggplot(data = data_df, aes(x = dates)) +
  geom_line(aes(y = test_data, color = "Actual"), size = 1) +
  geom_line(aes(y = snaive_forecast$mean, color = "Seasonal Naive"), size = 1) +
  geom_line(aes(y = drift_forecast$mean, color = "Drift"), size = 1) +
  geom_line(aes(y = arima_forecast$mean, color = "ARIMA"), size = 1) +
  geom_line(aes(y = tslm_forecast$mean, color = "TSLM"), size = 1) +
  scale_color_manual(values = c("Actual" = "blue", 
                                "Seasonal Naive" = "red",
                                "Drift" = "green",
                                "ARIMA" = "purple",
                                "TSLM" = "orange")) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 year") +
  xlab("Date") + ylab("Rainfall") +
  ggtitle("Actual vs. Forecasted Rainfall")


# Subset the forecasts to include only the test period
snaive_forecast_test <- snaive_forecast$mean[73:96]
drift_forecast_test <- drift_forecast$mean[73:96]
arima_forecast_test <- arima_forecast$mean[73:96]
tslm_forecast_test <- tslm_forecast$mean[73:96]

# Create a new data frame with the relevant data
plot_data <- data.frame(dates = monthly_mean_complete$, 
                        all_data = all_data,
                        snaive_forecast = snaive_forecast_test,
                        drift_forecast = drift_forecast_test,
                        arima_forecast = arima_forecast_test,
                        tslm_forecast = tslm_forecast_test)

# Create the plot
ggplot(plot_data, aes(x = dates)) +
  geom_line(aes(y = all_data), color = "blue", size = 1) +
  geom_line(aes(y = snaive_forecast), color = "red", size = 1) +
  geom_line(aes(y = drift_forecast), color = "green", size = 1) +
  geom_line(aes(y = arima_forecast), color = "purple", size = 1) +
  geom_line(aes(y = tslm_forecast), color = "orange", size = 1) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 year") +
  xlab("Date") + ylab("Rainfall") +
  ggtitle("Actual vs. Forecasted Rainfall")



length(snaive_forecast$mean)
length(drift_forecast$mean)
length(arima_forecast$mean)
length(tslm_forecast$mean)
length(test_data)

install.packages("tseries")
library(tseries)
# Test for stationarity using Augmented Dickey-Fuller (ADF) test
adf_test <- adf.test(ts_data)
print(adf_test)

# Check the p-value of the test
if (adf_test$p.value < 0.05) {
  print("The time series is stationary")
} else {
  print("The time series is non-stationary")
}

install.packages("forecast")
# Load the required packages
library(forecast)


# Determine the order of differencing
d <- ndiffs(ts_data)


# Determine the order of seasonal differencing
D <- nsdiffs(ts_data)

# Identify the order of the AR, MA, seasonal AR, and seasonal MA terms using auto.arima()
model <- auto.arima(ts_data, d=d, D=D, stepwise=FALSE, approximation=FALSE, trace=FALSE, seasonal=TRUE)

# Print the model summary
summary(model)

# Generate forecasts
forecast <- forecast(model, h=365)

# Plot the forecasts
plot(forecast)

# Calculate accuracy measures
accuracy(model)


# Calculate accuracy measures
accuracy <- accuracy(forecast)




