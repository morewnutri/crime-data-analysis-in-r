
#exponential smoothing allows for weighted averages where greater weight can be placed on recent observations and lesser weight on older observations.
#Holt’s Method makes predictions for Simple Exponential Smoothing: Technique for data with no trend or seasonality.
#Holt’s Method: Technique for data with trend but no seasonality.
#Holt-Winters Seasonal Method: Technique for data with trend and seasonality.
#Damped Trend Methods: Technique for trends that are believed to become more conservative or “flat-line” over time.data with a trend using two smoothing parameters, α and β, which correspond to the level and trend components, respectively.

#holt method
data.train <- ts(gold.price.monthely[,2],start = c(2014,1),end=c(2018,1),frequency = 12)
data.test <- ts(gold.price.monthely[,2],start = c(2018,2),end=c(2020,1),frequency = 12)
plot (data.train,xlab='time',ylab='price')
plot (data.test,xlab='time',ylab='price')

library(fpp2)
holt <- holt(data.train,h=12)
autoplot(holt)
accuracy(holt,data.test)
#mape have about a 7% error rate

#holt-winter
#additive
data.train.hw <- ets(data.train,model = "AAA")#ets stands for error ,trend,seasonality.AAA stands for a model with additive error,additive trend,additive seasonality
autoplot(forecast(data.train.hw))
summary(data.train.hw)#we can see alpha beta and gamma from here
#error: additive (“A”), multiplicative (“M”), unknown (“Z”)
#trend: none (“N”), additive (“A”), multiplicative (“M”), unknown (“Z”)
#seasonality: none (“N”), additive (“A”), multiplicative (“M”), unknown (“Z”)
#Consequently, if you wanted to apply a Holt’s model where the error and trend were additive and no seasonality exists you would select model = "AAN". If you want to apply a Holt-Winters model where there is additive error, an exponential (multiplicative) trend, and additive seasonality you would select model = "AMA".
checkresiduals(data.train.hw)
forecast.wh <- forecast(data.train.hw,h=5)
summary(forecast.wh)
accuracy(forecast.wh,data.test)

#multiplicative,model="MAM'
data.train.wh2 <- ets(data.train,model="MAM")
checkresiduals(data.train.wh2)
autoplot(forecast(data.train.wh2))
forecast.wh2 <- forecast(data.train.wh2,h=12)
accuracy(forecast.wh2,data.test)








