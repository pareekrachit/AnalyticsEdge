#Predict using QualityCareModel on training set, output will be probability
predictQualityCare <- predict(QualityCareModel, type = "response")

summary(predictQualityCare)

#Mean probility for poor care and quality care
tapply(predictQualityCare, qualityTrain$PoorCare, mean)


table(qualityTrain$PoorCare, predictQualityCare > 0.5)

install.packages("ROCR")
library(ROCR)

#Prediction function
ROCRpred <- prediction(predictQualityCare, qualityTrain$PoorCare)

#Perfomance function
ROCRperf <- performance(ROCRpred, "tpr", "fpr")

#Plot ROC curve
plot(ROCRperf)

#Add colors, on RHS of curve is threshold value
plot(ROCRperf, colorize = TRUE)

#Add threshold at interval value of 0.1
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))



#Predict using QualityCareModel on test set
predictCare <- predict(QualityCareModel, newdata = qualityTest, type = "response")

summary(predictCare)

#Mean of probability for poor care and good care
tapply(predictCare, qualityTest$PoorCare, mean)

table(qualityTest$PoorCare, predictCare > 0.3)

#ROCR Prediction fn
ROCRpredTest <- prediction(predictCare, qualityTest$PoorCare)

#ROCR Perfomance fn
ROCRperfTest <- performance(ROCRpredTest, "tpr", "fpr")

#Plot ROC curve
plot(ROCRperfTest)

plot(ROCRperfTest, colorize = TRUE)

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
