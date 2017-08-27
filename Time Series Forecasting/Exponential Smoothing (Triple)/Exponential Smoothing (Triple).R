#Exponential Smoothing - Trend
#Trend Increases/Decreases - Presence of Seasonality

#Will work on Log of monthly sales for souvenir shop at beach resort in Australia

library(data.table)
library(forecast)

#reading the monthly sales souvenir shop from url using fread which belongs to data.table library
sales<-fread("https://robjhyndman.com/tsdldata/data/sales.dat",showProgress=TRUE)

#######################################################################################################
#Converting and ploting time series data
#In time forecasting using HoltWinters


salesseries<-ts(sales,frequency=12,start=c(1987,1)) 
plot(salesseries) # ploting the time series
plot.ts(salesseries)

logsalesseries<-log(salesseries) # log to convert multiplicative time series to additive
plot(logsalesseries)

decomposelogsalesseries <-decompose(logsalesseries)
plot(decomposelogsalesseries)
decomposelogsalesseries

#Adjusting seasonal component
#logsalesseries <-logsalesseries - decomposelogsalesseries$seasonal

salesseriesforecasts<-HoltWinters(logsalesseries)
#In time forecast using holt winter without passing alpha,beta and gamma values

salesseriesforecasts 
#o/p alpha =0.5533, beta = 0.3475, gamma = 1

salesseriesforecasts$SSE
#o/p: 0.0090  Too Small

plot(salesseriesforecasts)
#############################################################################################################
#Out of time forecast using forecast function belonging to forecast library
salesseriesforecasts2 <-forecast(salesseriesforecasts, h=48) # forecast for future 48 months

plot(salesseriesforecasts2)
plot.ts(salesseriesforecasts2)

################################################################################################################
#Validating the forecast

# 1 .Check the autocorrrelation of residuals.
acf(salesseriesforecasts2$residuals,lag.max=20, na.action = na.contiguous)
# go back 20 points backwards
#As we can see spikes are not crossing the blue lines. Therefore no need to smooth the data further.

#2. Check Ljung-Box test
Box.test(salesseriesforecasts2$residuals,lag=20,type="Ljung-Box")
#p-value is above 0.05 which means autocorrelation between residuals is not significant.
#ie residuals are not autocorrelated

#3. Check for random distribution of residuals
plot.ts(salesseriesforecasts2$residuals)
#No pattern is associated

#4. Plot histogram
hist(salesseriesforecasts2$residuals, col = 'red')

