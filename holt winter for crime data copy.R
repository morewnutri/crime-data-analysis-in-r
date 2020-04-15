
#street crime

#1.1 check frequancy
library(TSA)
periodogram(fulldata$street.crime)#The above periodogram plot is to identify the dominant periods(or frequncies) of a time series. From this plot, the time period is identified as 0.25 and the frequency is calculated as 1/0.25 = 4. Therefore, we can conclude that the behaviour in the series is quarterly
streetcrime.ts <- ts(fulldata$street.crime, frequency = 4, start = c(1))
streetcrime.ts
#1.2 making as time series
plot.ts(streetcrime.ts,main = "Timeseries of street crime", col = "blue")#plotting time series 
abline(reg = lm(streetcrime.ts~time(streetcrime.ts)))#We can see from the plot that there is a linear trend and the magnitude of seasonal pattern is almost constant as the time increases, hence the timeseries can be described using Additive Holt-Winter’s method and also forecast future street crime.
#1.3 check stationary
library(aTSA)
library(tseries)
acf(streetcrime.ts,main = 'street crime')
kpss.test(streetcrime.ts)#accorfing to the test,data is not stationary
stationary.test(streetcrime.ts,method = c("pp"))
#1.4 applying holt winter model
library(forecast)
fit <- hw(streetcrime.ts,seasonal = "additive")#predicted value
fit
fitted (fit)#The data is smoothed by applying Holt-Winter’s additive method.Above is the smoothed or predicted values of the given data.
fitted <- fitted(fit)
fitted
plot.ts(streetcrime.ts,main = "Smoothed Timeseries of street crime", col = "blue")
lines(fitted(fit),col = "red")
fit$model#estmate of model parameter
#1.5 forecast
library(forecast)
autoplot(forecast::forecast(fit,h=8))#运行这个函数时总是出错如unused argument等等是因为调用了forecast和atsa的原因，程序在调包时出错
#1.6 Decomposing the additive time series data
states <- fit$model$states[,1:3]
colnames(states) <- cbind('Level','Trend','Seasonality')
plot(states,col = "blue", main = "Decompostion of time series")
#1.7 measuring accuracy
plot(residuals(fit))#Thus, we can see that the above residual plot doesn’t show any pattern in it. Hence, we can conclude that the forecasted values are correct.









