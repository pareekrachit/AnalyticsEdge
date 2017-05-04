#Predicting FluTrend on basis of FluTrend1
predFluTrend1 = exp(predict(FluTrend1, newdata=FluTest))

which(FluTest$Week == '2012-03-11 - 2012-03-17')
FluTest[11,]
predFluTrend1[11]

SSE <- sum((predFluTrend1 - FluTest$ILI)^2)
RMSE <- sqrt(SSE/nrow(FluTest))
RMSE
