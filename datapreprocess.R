#importing file
insurance <- read.csv("C:/Users/Abhishek/Downloads/mAaoaxuZfEJxaT2L.csv", 
                             stringsAsFactors=TRUE)

#this command is to ignore the missing NA values

insurance =  na.omit(insurance)
summary(insurance)

#Column names in data set
str(insurance)
names(insurance)

#this command is to see how many children are actually there in dataset
insurance$children = as.factor(insurance$children)
summary(insurance)

#Step 1 : Data preprocessing
#Step 2 : Dummy variables
#Step 3 : Correlation
#Step 4 : Divide Dataset into training and testing
#Step 5 : Build model on training dataset
#Step 6 : Test model on testing basis

library(fastDummies)
insurance = dummy_cols(insurance,select_columns = "region")

#Label encoding
insurance$smoker= ifelse (insurance$smoker == "yes",1,0)
insurance$sex = ifelse (insurance$sex == "male",1,0)

#Correlation
library(corrplot)
cr = cor(insurance[c("age","sex","bmi","smoker","charges")])
cr
corrplot.mixed(cr)

#Training and testing dataset
library(caTools)
set.seed(111)
split = sample.split(insurance$charges,SplitRatio = 0.7)
training = subset(insurance, split== TRUE)
testing = subset(insurance, split== FALSE)

library(psych)
pairs.panels(training[c("age","sex","bmi","smoker","charges")])

#To solve problem of heteroscadicity (funnel)
#training$charges = log(training$charges)
pairs.panels(training[c("age","sex","bmi","smoker","charges")])

#Built model on training data
#Forward regression model
#dependent V = IDV1
#DV = IDV1+IDV2

library(car)
model1 = lm(charges~age, data = training)
summary(model1)
plot(model1$fitted.value,model1$residuals)

model2 = lm(charges~age+bmi, data = training)
summary(model2)
vif(model2)
plot(model2$fitted.value,model2$residuals)

model3 = lm(charges~age+bmi+smoker, data = training)
summary(model3)
vif(model3)
plot(model3$fitted.value,model3$residuals)

model4 = lm(charges~age+bmi+smoker+sex, data = training)
summary(model4)
vif(model4)
plot(model4$fitted.value,model4$residuals)

#Step6: Testing model on testing data for first 6 rows
prediction = predict(model3,testing)
head(prediction)
head(testing$charges)

plot(testing$charges,type="l",col="green")
lines(prediction,type="l",col="blue")

library(knitr)