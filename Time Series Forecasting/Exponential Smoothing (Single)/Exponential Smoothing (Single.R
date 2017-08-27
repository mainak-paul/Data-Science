#Exponential Smoothing - Double
#Constant trend - no seasonality

#Will work on time series of the Total annual rainfall, inches, London, England, 1813-1912

library(data.table)
library(forecast)
#reading the skirt series data from url using fread which belongs to data.table library
rainfall<-fread("https://robjhyndman.com/tsdldata//hurst/precip1.dat",skip=1,showProgress=TRUE)

#######################################################################################################
#Converting and ploting time series data
#In time forecasting using HoltWinters


rainfallseries<-ts(rainfall,start=c(1813)) 
plot(rainfallseries) # ploting the rain fall time series


rainfallseriesforecasts<-HoltWinters(rainfallseries,beta = FALSE, gamma=FALSE)
#Note: Here we are passing beta and gamma value as false, because alpha value is required to smoothen the random fluctuation

rainfallseriesforecasts 
#o/p alpha =0.020

rainfallseriesforecasts$SSE
#o/p: 1796 Too Big

plot(rainfallseriesforecasts)
plot(rainfallseriesforecasts$fitted)
#############################################################################################################
#Out of time forecast using forecast function belonging to forecast library
rainfallseriesforecasts2 <-forecast(rainfallseriesforecasts, h=6)

plot(rainfallseriesforecasts2)


