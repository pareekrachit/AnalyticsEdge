#Evaluating accuracy of smart baseline model
table(PollingDataTest$Republican, sign(PollingDataTest$Rasmussen))

#Test set predictions
predPollingData <- predict(PollingDataModel, newdata = PollingDataTest, family="binomial")

#Evaluating accuracy of polling data model
table(PollingDataTest$Republican, predPollingData >= 0.5)

#Find the row in which our prediction was wrong
subset(PollingDataTest, Republican  == 0 & predPollingData >= 0.5)
