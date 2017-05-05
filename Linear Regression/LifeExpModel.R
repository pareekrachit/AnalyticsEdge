#Load data
statedata <- read.csv("~/The Analytics Edge/Week 2/statedata.csv")

plot(statedata$x, statedata$y)


#Region having highest average high school graduation rate
tapply(statedata$HS.Grad, statedata$state.region, mean)

#Boxplot of the murder rate by region
boxplot(statedata$Murder ~ statedata$state.region)

which.max(statedata$Murder[statedata$state.region == 'Northeast'])

table(statedata$state.region)
statedata$state.name[statedata$state.region == 'Northeast']

#Create a model to predict Life expectacy
LifeExpReg <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(LifeExpReg)

plot(statedata$Income, statedata$Life.Exp)
cor(statedata[,c('Life.Exp' , 'Population' , 'Income' , 'Illiteracy' , 'Murder' , 'HS.Grad' , 'Frost' , 'Area')])

LifeExpReg1 <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExpReg1)

LifeExpReg2 <- lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExpReg2)

LifeExpReg3 <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(LifeExpReg3)

predLifeExp <- predict(LifeExpReg3)
which.max(predLifeExp)
statedata$state.name[47]
which.max(statedata$Life.Exp)
statedata$state.name[11]

which.max(abs(LifeExpReg3$residuals))
statedata$state.name[11]
