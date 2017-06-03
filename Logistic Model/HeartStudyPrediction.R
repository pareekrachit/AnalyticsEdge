predictHeartStudy <- predict(HeartStudyModel, newdata = framinghamTest, type = "response")
summary(predictHeartStudy)


#Confusion matrix with threshold of 0.5
table(framinghamTest$TenYearCHD, predictHeartStudy > 0.5)

#Accuracy of model
(1069+11)/(1069+6+187+11)
#Baseline accuracy(Choosing 0 as it is more prevelant)
(1069+6)/(1069+6+187+11)

library(ROCR)
ROCRpred <- prediction(predictHeartStudy, framinghamTest$TenYearCHD)

ROCRperf <- performance(ROCRpred, "tpr", "fpr")

plot(ROCRperf, colorize = TRUE)

as.numeric(performance(ROCRpred, "auc")@y.values)
