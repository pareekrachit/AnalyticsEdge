#Read data
PollingData <- read.csv("~/The Analytics Edge/Week 3/Election Forecasting/PollingData.csv")

summary(PollingData)
str(PollingData)

table(PollingData$Year)
table(PollingData$State)

PollingData[PollingData$State == 'Wyoming',]

#How to deal with null values
sum(PollingData$Rasmussen, na.rm = TRUE)

#Install and load mice package
install.packages("mice")
library(mice)

#Multiple imputation
simple <- PollingData[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]

summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)

PollingData$Rasmussen <- imputed$Rasmussen
PollingData$SurveyUSA <- imputed$SurveyUSA
summary(PollingData)

#Divide data set into traing and test 
PollingDataTrain <- subset(PollingData, Year == 2004 | Year == 2008)
PollingDataTest <- subset(PollingData, Year == 2012)

#Normal Baseline model
table(PollingDataTrain$Republican)
#We go with most occuring output that is 1.
#So baseline accuracy is 53%, which is pretty low
#We need to improve accuracy of our baseline model

#We will use sign fn.
sign(20)
sign(-10)
sign(0)

#We will take Rasmussen to build baseline model
table(sign(PollingDataTrain$Rasmussen))

#Compare baseline model to actual result
table(PollingDataTrain$Republican, sign(PollingDataTrain$Rasmussen))

#Accuracy
(42+52)/100

#Multicollinearity
cor(PollingDataTrain[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])

# Logistic Regression Model
PollingDataModel <- glm(Republican ~ PropR, data = PollingDataTrain, family="binomial")
summary(PollingDataModel)

# Training set predictions
predTraindata <- predict(PollingDataModel, type = "response")
table(PollingDataTrain$Republican, predTraindata >= 0.5)

# Two-variable model
cor(PollingDataTrain[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])
#Use those 2 variables which are least corelated
PollingDataModel2 <- glm(Republican ~ SurveyUSA + DiffCount, data = PollingDataTrain, family = "binomial")
summary(PollingDataModel2)

predTraindata1 <- predict(PollingDataModel, type = "response")
table(PollingDataTrain$Republican, predTraindata1 >= 0.5)
