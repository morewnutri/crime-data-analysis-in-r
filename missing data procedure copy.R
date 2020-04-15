
library(missForest)


library(readxl)
crime <- read_excel('~/Desktop/book5.xlsx')
crime

library(mi)
dataframe1 <- as.data.frame(crime)

library(missForest)
missdata <- missForest(dataframe1)
missdata$ximp#check imputed value
missdata$OOBerror#imputation error

crimemi <- mi(dataframe)
summary(crimemi)
str(crimemi)




library(mi)
crimemi <- mi(crime)

library(mice)
imputed_Data <- mice(crime, m=5, maxit = 50, method = 'pmm', seed = 500)

# deal with missing data
library(mice)
# Deterministic regression imputation via mice
imp <- mice(crime, method = "norm.predict", m = 1)
# Store data
data_imp <- complete(imp)
# Multiple Imputation
imp <- mice(mydata, m = 5)
#build predictive model
fit <- with(data = imp, lm(y ~ x + z))
#combine results of all 5 models
combine <- pool(fit)

library(DMwR)
knnseriouscrime <- knnImputation(serioucrime)


#visulazing missing data(unsuccssed)
#install.packages("VIM")
#library(VIM)
#mice_plot <- aggr(crime, col=c('navyblue','yellow'),numbers=TRUE, sortVars=TRUE,labels=names(crime), cex.axis=10, gap=10, ylab=c("Missing data","Pattern"))



#missing_data.frame(datastr)









#load data
data("iris")

#seed 10% missing values
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)
iris.mis

#
library(mi)
mi_data <- mi(iris.mis, seed = 335)
mi_data
summary(mi_data)



#impute missing values, using all parameters as default values
iris.imp <- missForest(iris.mis)
#check imputed values
iris.imp$ximp
#check imputation error
iris.imp$OOBerror