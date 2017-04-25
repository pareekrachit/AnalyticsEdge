#Average test score of male and female
tapply(pisatrain$readingScore, pisatrain$male, mean)

summary(pisatrain)

#Removing missing data from dataset
pisaTrain1 = na.omit(pisatrain)
pisaTest1 = na.omit(pisatest)


str(pisaTrain1)
table(pisaTrain1$raceeth)
