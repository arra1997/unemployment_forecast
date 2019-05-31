# Required libraries
library(lattice)
library(foreign)
library(MASS)
library(strucchange)
require(stats)
require(stats4)
library(KernSmooth)
library(fastICA)
library(cluster)
library(leaps)
library(mgcv)
library(rpart)
library(pan)
library(mgcv)
library(DAAG)
library(TTR)
library(tis)
library(xts)
library(forecast)
library(stats)
library(TSA)
library(timeSeries)
library(fUnitRoots)
library(fBasics)
library(tseries)
library(timsac)
library(TTR)
library(fpp)
library(seasonal)
library(strucchange)
library(dplyr)
library(vars)


setwd('/home/arvind/Desktop/project2')
input1 = read.csv('CAURN.csv')
input2 = read.csv('NYURN.csv')

# Set up abstractions
time_series1 = data.frame(index = input1$DATE)
time_series1$val = input1$CAURN

time_series2 = data.frame(index = input2$DATE)
time_series2$val = input2$NYURN

# Plot time series, acf, pacf
# California
plot(time_series1$val, type = "l", ylab = "CA Unemployment Rate (%)", xlab = "Date", col = "blue")
acf(time_series1$val, main = "CA Unemployment % ACF")
pacf(time_series1$val, main = "CA Unemployment % PACF")

# New York
plot(time_series2$val, type = "l", ylab = "NY Unemployment Rate (%)", xlab = "Date", col = "blue")
acf(time_series2$val, main = "NY Unemployment % ACF")
pacf(time_series2$val, main = "NY Unemployment % PACF")


unemployment_ca = ts(time_series1$val)
unemployment_ny = ts(time_series2$val)

# ARIMA (1,0,0), (0,0,12) models 
model1_ca = Arima(unemployment_ca, order=c(1,0,0), seasonal=c(0,0,12))
model1_ny = Arima(unemployment_ny, order=c(1,0,0), seasonal=c(0,0,12))
summary(model1_ca)
summary(model1_ny)

# Model fits
plot(unemployment_ca, type = "l", col = "green")
lines(model1_ca$fitted, col = "red")
plot(unemployment_ny, type = "l", col = "green")
lines(model1_ny$fitted, col = "red")

# Model residuals 
plot(model1_ca$residuals)
plot(model1_ny$residuals)

# Residual ACF & PACF
acf(model1_ca$residuals, main = "CA Residuals ACF")
pacf(model1_ca$residuals,main = "CA Residuals PACF")
# Recursive residuals
plot(recresid(model1_ca$residuals~ 1),type = "l",main = "CA Resursive Residuals", ylab = "Residuals")
plot(recresid(model1_ny$residuals~ 1),type = "l",main = "NY Resursive Residuals",ylab = "Residuals")


# CUSUM
plot(efp(model1_ca$residuals ~ 1, type = "Rec-CUSUM"),main = "CA Recursive CUSUM")
plot(efp(model1_ny$residuals ~ 1, type = "Rec-CUSUM"), main = "NY Recursive CUSUM")


# Model forecasts
plot(forecast(model1_ca, h = 12), main = "CA Seasonal ARIMA Forecast")
plot(forecast(model1_ny, h = 12), main = "NY Seasonal ARIMA Forecast")


# Cross-correlation function
ccf(unemployment_ca, unemployment_ny, main = "CA & NY Unemployment Cross-Correlation")

# VAR Model
unemployment_tot = cbind(cbind(unemployment_ca, unemployment_ny))
var_model = VAR(unemployment_tot,p=6)
summary(var_model)
plot(var_model)
par(mfrow=c(2,1))
acf(residuals(var_model)[,1])
pacf(residuals(var_model)[,1])


# Impulse Response Function
irf(var_model)
plot(irf(var_model, n.ahead=36), main = "IRF")


# Recursive CUSUM of VAR model
plot(stability(var_model, type = "Rec-CUSUM"), plot.type="single")

# Granger-Causality test
grangertest(unemployment_ca ~ unemployment_ny, order = 6)

#12-step ahead forecast
var.predict = predict(object=var_model, n.ahead=12)
plot(var.predict)





