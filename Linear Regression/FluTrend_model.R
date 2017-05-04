#Load the data
FluTrain <- read.csv("~/The Analytics Edge/Week 2/Detecting Flu Epidemics/FluTrain.csv")
FluTest <- read.csv("~/The Analytics Edge/Week 2/Detecting Flu Epidemics/FluTest.csv")

which(FluTrain$ILI == max(FluTrain$ILI))
FluTrain$Week[303]

which.max(FluTrain$Queries)

#Understanding the ILI values
hist(FluTrain$ILI)
hist(log(FluTrain$ILI))

plot(log(FluTrain$ILI), FluTrain$Queries)
cor(log(FluTrain$ILI), FluTrain$Queries)

#Linear Reg model to predict ILI values
FluTrend1 <- lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)
