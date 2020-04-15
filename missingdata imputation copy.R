
library(readxl)
crime <- read_excel('~/Desktop/subjects/master dissertation/crime dataset.xlsx')
str(crime)

datastr <- as.data.frame(crime)
library(missForest)
crime.fill <- missForest(datastr)
crime.fill$ximp#check imputed value
crime.fill$OOBerror#imputation error

write.csv(crime.fill$ximp,file="~/Desktop/c.csv",quote=F)

fulldata <- read.csv('~/Desktop/c.csv')





