#Read Data
claimsData <- read.csv("~/The Analytics Edge/Week 4/D2Hawkeye/ClaimsData.csv")
str(claimsData)

#Percentage of patients in each bucket
table(claimsData$bucket2009)/nrow(claimsData)

#Splitting data
library(caTools)
set.seed(88)

split <- sample.split(claimsData$bucket2009, SplitRatio = 0.6)
claimsTrain <- subset(claimsData, split == TRUE)
claimsTest <- subset(claimsData, split == FALSE)

#Checking the distribution of datasets
table(claimsData$bucket2009)/nrow(claimsData)
table(claimsTrain$bucket2009)/nrow(claimsTrain)
table(claimsTest$bucket2009)/nrow(claimsTest)

mean(claimsTrain$age)
table(claimsTrain$diabetes)/nrow(claimsTrain)

#Baseline Accuracy
table(claimsTest$bucket2009, claimsTest$bucket2008) #Actual values on side and predicted on top
(110138+10721+2774+1539+104)/(nrow(claimsTest))

#Penalty matrix
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)
PenaltyMatrix

# Penalty Error of Baseline Method
as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008)) * PenaltyMatrix #Not actual matrix multiplication
sum(as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008)) * PenaltyMatrix)/nrow(claimsTest)

#What if baseline method would predict cost bucket 1 for everyone
#Accuracy
table(claimsTest$bucket2008)
122978/nrow(claimsTest)
table(claimsTest$bucket2009)

#Penalty error
claimsTest$mycol <- 1
table(claimsTest$bucket2009, claimsTest$mycol)
as.matrix(table(claimsTest$bucket2009, claimsTest$mycol)) * PenaltyMatrix[,1]
sum(as.matrix(table(claimsTest$bucket2009, claimsTest$mycol)) * PenaltyMatrix[,1])/nrow(claimsTest)

#Cross - Validation
library(caret)
library(e1071)

#Choosing no. of folds
numFolds <- trainControl(method = "cv", number = 10)
numFolds

#Picking possible values for cp
cpGrid <- expand.grid(.cp = seq(0.1,0.5,0.01))
cpGrid

#Perform cv
train(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 +  reimbursement2008, data = claimsTrain, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

#Create CART model:-
library(rpart)
library(rpart.plot)

claimsTree <- rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 +  reimbursement2008, data = claimsTrain, method = "class", cp = 0.00005)
prp(claimsTree)
