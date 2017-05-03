#Load training and testing datasets
pisaTrain <- read.csv("C:/Users/532771/Documents/The Analytics Edge/Week 2/Reading Test Score/pisa2009train.csv")
pisaTest <- read.csv("C:/Users/532771/Documents/The Analytics Edge/Week 2/Reading Test Score/pisa2009test.csv")

#Average test score of male and female
summary(pisaTrain)
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

#Removing missing data from dataset
pisaTrain1 = na.omit(pisaTrain)
pisaTest1 = na.omit(pisaTest)


str(pisaTrain1)
table(pisaTrain1$raceeth)
levels(pisaTrain1$raceeth)

plot(pisaTrain1$raceeth)
