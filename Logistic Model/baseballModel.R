#Read the data set
baseball <- read.csv("~/The Analytics Edge/Week 3/baseball.csv");

table(baseball$Team)

#Length of table
length(table(baseball$Team))
length(table(baseball$Year))

#Limiting to Teams Making the Playoffs
baseball <- subset(baseball, Playoffs == 1)

nrow(baseball)

table(baseball$Team)

table(baseball$Year)

#Add NumCompetitiors(No. of teams qualifying for playoffs for that year)
PlayoffTable <- table(baseball$Year)
names(PlayoffTable)
str(PlayoffTable)

str(names(PlayoffTable))

baseball$NumCompetitiors <- PlayoffTable[as.character(baseball$Year)]

table(baseball$NumCompetitiors)

baseball$WorldSeries <- 0
baseball$WorldSeries[baseball$RankPlayoffs == 1] <- 1

table(baseball$WorldSeries)

#Creating bivariate model
summary(glm(WorldSeries~Year, data=baseball, family="binomial"))
summary(glm(WorldSeries~RA, data=baseball, family="binomial"))
summary(glm(WorldSeries~RankSeason, data=baseball, family="binomial"))
summary(glm(WorldSeries~NumCompetitiors, data=baseball, family="binomial"))


baseballModel <- glm(WorldSeries~Year + RA + RankSeason + NumCompetitiors, data=baseball, family="binomial")
summary(baseballModel)


cor(baseball[c("Year","RA","RankSeason","NumCompetitiors")])

summary(glm(WorldSeries~Year + RA, data=baseball, family="binomial"))
summary(glm(WorldSeries~Year + RankSeason, data=baseball, family="binomial"))
summary(glm(WorldSeries~Year + NumCompetitiors, data=baseball, family="binomial"))
summary(glm(WorldSeries~RA + RankSeason, data=baseball, family="binomial"))
summary(glm(WorldSeries~RA + NumCompetitiors, data=baseball, family="binomial"))
summary(glm(WorldSeries~RankSeason + NumCompetitiors, data=baseball, family="binomial"))
