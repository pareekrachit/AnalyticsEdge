#Read  data
framingham <- read.csv("~/The Analytics Edge/Week 3/Framingham Heart Study/framingham.csv")

str(framingham)
summary(framingham)

table(framingham$TenYearCHD)

#Split data set
library(caTools)
set.seed(1000)
split <- sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
framinghamTrain <- subset(framingham, split == TRUE)
framinghamTest <- subset(framingham, split == FALSE)

table(framinghamTrain$TenYearCHD)

#Logistic Regression Model
HeartStudyModel <- glm(TenYearCHD ~ ., data = framinghamTrain, family = binomial)
summary(HeartStudyModel)
