
# 1. street crime analysis with holt model
fulldata$street.crime

data.train <- ts(fulldata$street.crime,start = c(1),end=c(60),frequency = 4)
data.test <- ts(fulldata$street.crime,start = c(61),end=c(64),frequency = 4)
plot (data.train,xlab='time',ylab='street crime')
plot (data.test,xlab='time',ylab='street crime')

library(fpp2)
holt <- holt(data.train,h=8)
autoplot(holt)#just straight line,something wrong
accuracy(holt,data.test)
