#Exponential Smoothing - Double
#trend varies - constant seasonality

#Will work on time series of the annual diameter of womens skirts at the hem, fro 1866 to 1911 skirts.dat

library(data.table)
library(forecast)
#reading the skirt series data from url using fread which belongs to data.table library
skirts<-fread("https://robjhyndman.com//tsdldata//roberts//skirts.dat",skip=5,showProgress=TRUE)

#######################################################################################################
#Converting and ploting time series data
#In time forecasting using HoltWinters


skirtseries<-ts(skirts,start=c(1866)) 
plot(skirtseries) # ploting the time series

fit <- tbats(skirtseries) # tbats function belongs to forecast library
is.null(fit$seasonal)#Check If seasonal Component is present

skirtseriesforecasts<-HoltWinters(skirtseries,gamma=FALSE)
#Note: Here we only pass gamma value as false, because alpha value is required to smoothen the random fluctuation
#and beta is required to smoothen the trend.

skirtseriesforecasts 
#o/p alpha =0.83, beta = 1

skirtseriesforecasts$SSE
#o/p: 16954 Too Big

plot(skirtseriesforecasts)
#############################################################################################################
#Out of time forecast using forecast function belonging to forecast library
skirtseriesforecasts2 <-forecast(skirtseriesforecasts, h=20)

plot(skirtseriesforecasts2)
plot.ts(skirtseriesforecasts2)

################################################################################################################
#Validating the forecast

# 1 .Check the autocorrrelation of residuals.
acf(skirtseriesforecasts2$residuals,lag.max=20, na.action = na.contiguous)# go back 20 points backwards
#As we can see spikes are not crossing the blue lines. Therefore no need to smooth the data further.

#2. Check Ljung-Box test
Box.test(skirtseriesforecasts2$residuals,lag=20,type="Ljung-Box")
#p-value is above 0.05 which means autocorrelation between residuals is not significant.
#ie residuals are not autocorrelated

#3. Check for random distribution of residuals
plot.ts(skirtseriesforecasts2$residuals)
#No pattern is associated

#4. Plot histogram
hist(skirtseriesforecasts2$residuals, col = 'red')

