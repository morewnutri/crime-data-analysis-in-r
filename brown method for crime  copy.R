
#street crime 

# 1. data input
bsc <- ts(fulldata$street.crime,frequency = 4)
bsc
bsct <- window(bsc,end = c(14))#training data
bsct
bscf <- window(bsc,start= c(14))#testing data
bscf
# 2. simple exponential smoothing,we dont use this model consider our data has seasonality
brownsimple <-ses(bsct,h=12)#ses=simple exponential smoothing
plot(brownsimple,main = "brwon simple exponential smoothing",ylab = "street crime",xlab = "time series")
lines(bscf,lty =3)#adding data in testing range
# 3. browns model
brownsc <- ets(bsct,model = "AAA")
plot(brownsc)
autoplot(forecast::forecast(brownsc))
