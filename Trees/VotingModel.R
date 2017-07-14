gerber <- read.csv("C:/Users/532771/OneDrive - Cognizant/Documents/The Analytics Edge/Week 4/Why People Vote/gerber.csv")

#Proportion of people who voted
table(gerber$voting)/ nrow(gerber)

#Which treatment group had largest participation in voting
table(gerber$hawthorne)/nrow(gerber)
table(gerber$civicduty)/nrow(gerber)
table(gerber$neighbors)/nrow(gerber) #Each treatment group has equal no. of people
table(gerber$self)/nrow(gerber)
table(gerber$control)/nrow(gerber)

#Splits observation by hawthorne and applys mean to voting variable
tapply(gerber$voting, gerber$civicduty, mean) #Col 1 gives prop of people under civic duty who voted
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

#Find no of civic duty people who voted?
table(gerber$civicduty)
table(gerber$civicduty, gerber$voting)

#Logistic model for voting
votingmodel <- glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = "binomial")
summary(votingmodel)

#Predicting
predictvotingmodel <- predict(votingmodel, type = "response")
table(gerber$voting, predictvotingmodel > 0.3)
(51966+134513)/nrow(gerber) #Accuracy

table(gerber$voting, predictvotingmodel > 0.5)
(235388)/nrow(gerber) #Accuracy

table(gerber$voting)/nrow(gerber)
library(ROCR)
ROCRpred = prediction(predictvotingmodel, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)


#CART model for voting
library(rpart)
library(rpart.plot)

voting.tree = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(voting.tree)

voting.tree2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(voting.tree2)

#Predicting
predict.voting.tree2 <- predict(voting.tree2)

voting.tree3 <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(voting.tree3)

voting.tree4 <- rpart(voting ~ control, data = gerber, cp=0.0)
prp(voting.tree4, digits = 6)

voting.tree5 <- rpart(voting ~ control + sex, data = gerber, cp=0.0)
prp(voting.tree5, digits = 6)

#Logistic model for "sex" and "control"
votingmodel2 <- glm(voting ~ control + sex, data = gerber, family = "binomial")
summary(votingmodel2)
