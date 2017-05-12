#Predict using QualityCareModel on training set
predictQualityCare <- predict(QualityCareModel, type = "response")

summary(predictQualityCare)

tapply(predictQualityCare, qualityTrain$PoorCare, mean)


