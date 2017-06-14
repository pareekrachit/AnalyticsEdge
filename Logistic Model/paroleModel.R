#Read the data set
parole <- read.csv("~/The Analytics Edge/Week 3/parole.csv")

#No. of parolees who violated terms of their parole
table(parole$violator)

str(parole)#Although state and crime are integer variable but they should be factor variable 

parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
str(parole)
summary(parole)

#Spliting dataset
set.seed(144)
library(caTools)

split = sample.split(parole$violator, SplitRatio = 0.7)
paroleTrain = subset(parole, split == TRUE)
paroleTest = subset(parole, split == FALSE)

#Creating model using all variables
paroleModel <- glm(violator ~  ., data = paroleTrain, family = "binomial")
summary(paroleModel)

exp(-4.2411574 + 0.3869904 + 0.8867192 - 50*0.0001756 - 3*0.1238867 + 12*0.0802954 + 0.6837143)
