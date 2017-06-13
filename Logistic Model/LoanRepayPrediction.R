#Predicting values for loan repay
predictLoanRepay <- predict(loanModel, newdata = loanTest, type = "response")

#Prediction for smart baseline model
predictLoanRepay1 <- predict(loanModel1, newdata = loanTest, type = "response")
summary(predictLoanRepay1)

#Confusion matrix with threshold of 0.5
table(loanTest$not.fully.paid, predictLoanRepay > 0.5)
table(loanTest$not.fully.paid, predictLoanRepay1 > 0.5)

#Accuracy of model
(2427)/(2427+446)

#Accuracy of baseline model
table(loanTest$not.fully.paid)
2413/(2413+460)

#AUC
library(ROCR)
pred = prediction(predictLoanRepay, loanTest$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values)

#AUC for smart baseline model
library(ROCR)
pred1 = prediction(predictLoanRepay1, loanTest$not.fully.paid)
as.numeric(performance(pred1, "auc")@y.values)

10*exp(0.18)

#Profitability of loan
loanTest$profit <- exp(loanTest$int.rate*3) - 1
loanTest$profit[loanTest$not.fully.paid == 1] <- -1

min(loanTest$profit)
sum(loanTest$profit)


highInterest = subset(loanTest, int.rate >= 0.15)
summary(highInterest$profit)

#Proportion of the high-interest loans not paid back in full
table(highInterest$not.fully.paid)
