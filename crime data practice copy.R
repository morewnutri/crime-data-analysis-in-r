


#part 2.2 street crime holt-winter 
#additive
data.train.hw <- ets(data.train,model = "AAA")#ets stands for error ,trend,seasonality.AAA stands for a model with additive error,additive trend,additive seasonality
autoplot(forecast(data.train.hw))
summary(data.train.hw)#we can see alpha beta and gamma from here
#error: additive (“A”), multiplicative (“M”), unknown (“Z”)
#trend: none (“N”), additive (“A”), multiplicative (“M”), unknown (“Z”)
#seasonality: none (“N”), additive (“A”), multiplicative (“M”), unknown (“Z”)
#Consequently, if you wanted to apply a Holt’s model where the error and trend were additive and no seasonality exists you would select model = "AAN". If you want to apply a Holt-Winters model where there is additive error, an exponential (multiplicative) trend, and additive seasonality you would select model = "AMA".
checkresiduals(data.train.hw)
forecast.wh <- forecast(data.train.hw,h=4)
summary(forecast.wh)
accuracy(forecast.wh,data.test)



library(ggplot2)
library(forecast)

smpl1 <- window(fulldata$street.crime, end = c(60))
smpl2 <- window(fulldata$street.crime, start = c(61), end = c(64))

hw <- HoltWinters(smpl1, beta = FALSE, gamma = TRUE) 
forecast <- forecast(hw, h = 12)  

autoplot(forecast) +
  autolayer(smpl2, series="Data") +
  autolayer(forecast$mean, series="Forecasts")

#multiplicative,model="MAM'
data.train.wh2 <- ets(data.train,model="MAM")
checkresiduals(data.train.wh2)
autoplot(forecast(data.train.wh2))
forecast.wh2 <- forecast(data.train.wh2,h=4)
accuracy(forecast.wh2,data.test)

#part 2.3 street crime arima
#staionarity
data <- ts(fulldata$street.crime,start = c(1),end = c(60),frequency = 4)
plot (data,xlab='time',ylab='street crime')#stationary
#plot(diff(data),ylab='differenced price')

#plot(log10(data),ylab='log(price)')
#plot(diff(diff(diff(diff((log10(data)))))),ylab='differenced log')

#acf and pacf to identify potential ar and ma model
par(mfrow=c(1,2))
acf(ts(data),main='autocorrelation')
pacf(ts(data),main='partial autocorrelation')
#there are enough spikes in the plots outside the insignificant zone,we can conclude that the residuals are not random
#there is information avalible in residuals to be extracted by AR and MA,maybe seasonal component between 0-5 time lags
Box.test((data),type = "Ljung-Box")
#according to the box test,data is not stationary

#build arima
require(forecast)
arimafit = auto.arima((data),approximation=F,trace=F)
summary(arimafit)

#forecast
par(mfrow = c(1,1))
pred = predict (arimafit,n.ahead = 4)
pred
data.frame(pred)
plot(data,xlim=c(1,64),ylim=c(500,40000),xlab='time',ylab='street crime')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')

#plot acf and pacf for residuals again,to ensure no more information is left for extraction











