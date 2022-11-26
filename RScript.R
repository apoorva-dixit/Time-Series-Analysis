#Downloading and loading packages
install.packages("forecast")
install.packages("tsutils")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tseries")
library(tseries)
library(lubridate)
library(dplyr)
library(forecast)
library(tsutils)

#Load data into the R 
mydata <- read.csv("data.csv") #D449.csv, D478.csv, D506.csv

#Defining the format for date in data.
mydata$Day <- as.Date(mydata$Day, format="%d/%m/%Y")

#Create daily time series from the data
dailyts <- ts(mydata$Micro, frequency = 365.25, start = c(2001,3))  #start = "start date of your respective time series"
daily_cma <- cmav(daily_ts, ma = 30, fill = FALSE)

#Check if there are any missing values
sum(is.na(daily_ts))
range(daily_ts)

#Plot daily time series
plot(daily_ts, col = "blue", main = "Daily Time Series", ylab= "")
lines(daily_cma, col = "red")

#Plot seasonality of daily time series.
seasplot(daily_ts)

#Convert dates into their respective weeks.
mydata$Week <- floor_date(mydata$Day, "week")

#Create new dataframe specifically for weekly data by using aggregate function.
weekly<- aggregate(mydata$Micro, list(mydata$Week), FUN = mean)  #FUN = sum is used in time series 50.
names(weekly) <- c("Week", "Week_Average")
weekly_ts <- ts(weekly$Week_Average, frequency = 52, start = c(2000,1))  #start = "start date of your respective time series"
weekly_cma <- cmav(weekly_ts, ma = 52, fill = FALSE)

#Convert dates into their respective months.
mydata$Week <- floor_date(mydata$Day, "month")
#Create new dataframe specifically for monthly data by using aggregate function.
monthly<- aggregate(mydata$Micro, list(mydata$Month), FUN = mean)     #FUN = sum is used in time series 50.
names(monthly) <- c("Month", "Month_Average")
monthly_ts <- ts(monthly$Month_Average, frequency = 12, start = c(2000,1))   #start = "start date of your respective time series"
monthly_cma <- cmav(monthly_ts, ma = 12, fill = FALSE)

#Convert dates into their respective months.
mydata$Quarter <- floor_date(mydata$Day, "quarter")

#Create new dataframe specifically for quarterly data by using aggregate function.
quarterly<- aggregate(mydata$Micro, list(mydata$Quarter), FUN = mean)     #FUN = sum is used in time series 50.
View(quarterly)
names(quarterly) <- c("Quarter", "Quarter_Average")
quarterly_ts <- ts(quarterly$Quarter_Average, frequency = 4, start = c(2000,1))   #start = "start date of your respective time series"
quarterly_cma <- cmav(quarterly_ts, ma = 4, fill = FALSE)

#Plot each time seires (weekly, monthly, quarterly) after aggregation.
plot(weekly_ts, col = "blue", main = "Weekly Time Series", ylab= "")
lines(weekly_cma, col = "red")
plot(monthly_ts, col = "blue", main = "Monthly Time Series", ylab= "")
lines(monthly_cma, col = "red")
plot(quarterly_ts, col = "blue", main = "Quarterly Time Series", ylab= "")
lines(quarterly_cma, col = "red")

#Plot seasonality of weekly, monthly and quarterly time series.
seasplot(weekly_ts)
seasplot(monthly_ts)
seasplot(quarterly_ts)

#After finalising the aggreagte level we will now start with the decomposition of that time series.

#Perform decomposition of data
decomposition_m <- decomp(weekly_ts,decomposition = "multiplicative",outplot = TRUE)
decomposition_a <- decomp(weekly_ts,decomposition = "additive",outplot = TRUE)

#Plot seasonal component of decomposition
plot(decomposition_m$season, col = 'blue')
plot(decomposition_a$season, col = 'blue')

#Calculating error from actual – regular components (additive model)
weeklya_reg <- decomposition_a$trend + decomposition_a$season
weeklya_error <- weekly_ts - weeklya_reg
weeklya_abs_err <- abs(weekly_ts - weeklya_reg)
#Calculate ME
mean(weeklya_error) 
#Calculate MAE
mean(weeklya_abs_err) 
#Calculate MAPE for easier comparison
mean(abs(weeklya_error/weekly_ts))

#Calculating error from actual – regular components (multiplicative model)
weeklym_reg <- decomposition_m$trend * decomposition_m$season
weeklym_error <- weekly_ts - weeklym_reg
weeklym_abs_err <- abs(weekly_ts - weeklym_reg)
#Calculate ME
mean(weeklym_error)   
#Calculate MAE
mean(weeklym_abs_err)  
median(weeklym_abs_err) 
#Calculate MAPE
mean(abs(weeklym_error/weekly_ts))*100

#MAPE is less in multiplicative so we will go ahead with multiplicative decomp here. Series 50 and 78 will have different results.

tsdisplay(weekly_ts)

#Test stationarity
kpss.test(weekly_ts)
adf.test(weekly_ts)

#Calculating first difference if time series is not stationary.
diff_weekly_ts <- diff(weekly_ts)
kpss.test(diff_weekly_ts)
adf.test(diff_weekly_ts)

#If the test shows that data is not stationary with high peaks at lags 12, 24 and 36 (as in series 50) we do seasonal differencing.
weekly_s_diff <- diff(diff(weekly_ts), lag = 12)
kpss.test(weekly_s_diff)
adf.test(weekly_s_diff)

#Check the suggested no. of differencing for the time series (this is used in series 50 for monthly time series)
ndiffs(weekly_ts) 
nsdiffs(weekly_ts) 

#If the test shows that data is stationary and then we can check for ACF and PACF
tsdisplay(diff_weekly_ts)

#Based on the peaks in ACF & PACF so we will decide the orders of AR & MA, for this we start with ARI(1,1)
first_fit <- Arima(weekly_ts, order=c(1,1,0))


# Plot series, ACF and PACF of the residuals
tsdisplay(residuals(first_fit))

#If ACF & PACF both don't have any more peaks, then this is a good fit model, else try different orders for ARIMA.
second_fit <- Arima(weekly_ts, order=c(0,1,1))
tsdisplay(residuals(second_fit))

third_fit <- Arima(weekly_ts, order=c(1,1,1))
tsdisplay(residuals(third_fit))

#Use auto.arima to find the best fitting model
auto.arima(weekly_ts)  
#In this (time series 22) case using auto.arima also we get the same order for our best fit i.e., #ARIMA(1,1,0)

#Find best method via AIC
auto.arima(weekly_ts, ic="aic")

#Find best method with ADF Test
auto.arima(weekly_ts, test="adf")

#Additional codes used
#To make 3 plots in a single row
par(mfrow=c(1,3))
#To counter “Error in plot.new() : figure margins too large”
par(mar=c(3,2,2,1)) #default is 
