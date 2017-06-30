#Read data file
stevens <- read.csv("~/The Analytics Edge/Week 4/The Supreme Court/stevens.csv")
str(stevens)

#Split data set
library(caTools)
set.seed(200)
sample <- sample.split(stevens$Reverse, SplitRatio = 0.7)
stevensTrain <- subset(stevens, sample == TRUE)
stevensTest <- subset(stevens, sample == FALSE)

#Install Rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

#CART model
stevensTree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = stevensTrain, method="class", minbucket=25)
summary(stevensTree)
prp(stevensTree)

table(stevens$LowerCourt)
table(stevens$Respondent)


#Prediction
predictStevens <- predict(stevensTree, newdata = stevensTest, type = "class")
predictStevens
table(stevensTest$Reverse, predictStevens)
112/170

#Logistic model
summary(glm(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = stevensTrain, family = "binomial"))

#ROC curve
library(ROCR)
predictROC <- predict(stevensTree, newdata = stevensTest)
predictROC

pred = prediction(predictROC[,2], stevensTest$Reverse)
pred
ROCRperf = performance(pred, "tpr", "fpr")
plot(ROCRperf)

#Area under curve
auc = as.numeric(performance(pred, "auc")@y.values)
auc

#minbucket parameter to 5
prp(rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = stevensTrain, method="class", minbucket=5))

#minbucket parameter to 100
prp(rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = stevensTrain, method="class", minbucket=100))



#Install Random Forest package
install.packages("randomForest")
library(randomForest)

#Random forest doesn't have method argument, so we change dependent variable to factor variable because it's a classification problem
stevensTrain$Reverse = as.factor(stevensTrain$Reverse)
stevensTest$Reverse = as.factor(stevensTest$Reverse)

#Random Forest model
StevensForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = stevensTrain, ntree=200, nodesize=25 )
summary(StevensForest)

#Prediction
predictStevensForest <- predict(StevensForest, newdata = stevensTest)
predictStevensForest
table(stevensTest$Reverse, predictStevensForest)
120/170

#Install Cross-Validation package
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

#Define Cross-Validation experiment
numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.1,0.5,0.01))
cpGrid

# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = stevensTrain, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

#Create CART model using cp value
StevensCV <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = stevensTrain, method = "class", cp = 0.18)

#Make predictions
predictCV <- predict(StevensCV, newdata = stevensTest, type = "class")

#Accuracy
table(stevensTest$Reverse, predictCV)
123/170
