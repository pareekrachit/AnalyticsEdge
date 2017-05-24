#Load data
quality <- read.csv("~/The Analytics Edge/Week 3/Modeling the Expert/quality.csv")
str(quality)

table(quality$PoorCare)

plot(quality$OfficeVisits, quality$Narcotics, col = ifelse(quality$PoorCare == 1, "red", "green"), pch = 20)

#Install package caTools
install.packages("caTools")
library(caTools)

#Split data set and start count from no. 88 so that training set remains as that of instructor
set.seed(88)

#Split col in ratio .75 to .25
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split
table(split)

qualityTrain <- subset(quality, split == TRUE)
qualityTest <- subset(quality, split == FALSE)

table(qualityTrain$PoorCare)
table(qualityTest$PoorCare)

QualityCareModel <- glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(QualityCareModel)
