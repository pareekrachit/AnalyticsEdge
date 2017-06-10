#Load training and testing datasets
pisaTrain <- read.csv("C:/Users/532771/Documents/The Analytics Edge/Week 2/Reading Test Score/pisa2009train.csv")
pisaTest <- read.csv("C:/Users/532771/Documents/The Analytics Edge/Week 2/Reading Test Score/pisa2009test.csv")

#Average test score of male and female
summary(pisaTrain)
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

#Removing missing data from dataset
pisaTrain1 = na.omit(pisaTrain)
pisaTest1 = na.omit(pisaTest)

#raceeth is unordered variable
str(pisaTrain1)
table(pisaTrain1$raceeth)
levels(pisaTrain1$raceeth)

#Releveling factor variable raceeth
pisaTrain1$raceeth = relevel(pisaTrain1$raceeth, "White")
pisaTest1$raceeth = relevel(pisaTest1$raceeth, "White")

str(pisaTrain1)
levels(pisaTrain1$raceeth)

#Linear Reg model to predict reading score
readingScoreReg <- lm(readingScore ~ ., data = pisaTrain1)
summary(readingScoreReg)

sse <- sum(readingScoreReg$residuals^2)
rmse <- sqrt(sse/nrow(pisaTrain1))

