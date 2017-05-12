#Predict using QualityCareModel on training set
predictQualityCare <- predict(QualityCareModel, type = "response")

summary(predictQualityCare)

tapply(predictQualityCare, qualityTrain$PoorCare, mean)


table(qualityTrain$PoorCare, predictQualityCare > 0.5)
