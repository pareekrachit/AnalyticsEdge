#Predicting claims data
predClaimsData <- predict(claimsTree, newdata = claimsTest, type = "class")

#Accuracy
table(predClaimsData)
table(claimsTest$bucket2009, predClaimsData)
(114141 + 16102 + 118 + 201)/nrow(claimsTest)

#Penalty Error
as.matrix(table(claimsTest$bucket2009, predClaimsData)) * PenaltyMatrix
sum(as.matrix(table(claimsTest$bucket2009, predClaimsData)) * PenaltyMatrix)/nrow(claimsTest)

#Although accuracy has increased as comapred to baseline model but penaly error has also increased.
#It is because rpart trys to maximize overall accuracy, and every type of error is seen having a penalty of 1

claimsTree <- rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 +  reimbursement2008, data = claimsTrain, method = "class", cp = 0.00005, parms = list(loss = PenaltyMatrix))
prp(claimsTree)

#Predicting claims data
predClaimsData <- predict(claimsTree, newdata = claimsTest, type = "class")

#Accuracy
table(predClaimsData)
table(claimsTest$bucket2009, predClaimsData)
(94310 + 18942 + 4692 + 636 + 2)/nrow(claimsTest)

#Penalty Error
as.matrix(table(claimsTest$bucket2009, predClaimsData)) * PenaltyMatrix
sum(as.matrix(table(claimsTest$bucket2009, predClaimsData)) * PenaltyMatrix)/nrow(claimsTest)
