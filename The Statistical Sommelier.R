#Read data
str(wine)
summary(wine)

#Create a linear model between Age and Price
plot(wine$Age, wine$Price, pch = 20)
cor(wine$Age, wine$Price) #Correlation coefficient
lm(wine$Price ~ wine$Age)
abline(lm(wine$Price ~ wine$Age)) #Best-fit line

#Create a linear model between AGST and Price
plot(wine$AGST, wine$Price, pch = 20)
cor(wine$AGST, wine$Price) #Correlation coefficient
lm(wine$Price ~ wine$AGST)
model1 <- lm(wine$Price ~ wine$AGST)
summary(model1)
model1$residuals #Errors
sum(model1$residuals^2) #SSE
abline(lm(wine$Price ~ wine$AGST)) #Best-fit line



plot(wine$AGST + wine$HarvestRain, wine$Price, pch = 20)
cor(wine$AGST + wine$HarvestRain, wine$Price)
lm(wine$Price ~ wine$AGST + wine$HarvestRain)
model2 <- lm(wine$Price ~ wine$AGST + wine$HarvestRain)
summary(model2)
model2$residuals
sum(model2$residuals^2)
abline(lm(wine$Price ~ wine$AGST + wine$HarvestRain))


#Linear model including all independent variables
plot(wine$AGST + wine$HarvestRain + wine$WinterRain + wine$Age + wine$FrancePop, wine$Price, pch = 20)
cor(wine$AGST + wine$HarvestRain + wine$WinterRain + wine$Age + wine$FrancePop, wine$Price)
lm(wine$Price ~ wine$AGST + wine$HarvestRain + wine$WinterRain + wine$Age + wine$FrancePop)
model3 <- lm(wine$Price ~ wine$AGST + wine$HarvestRain + wine$WinterRain + wine$Age + wine$FrancePop)
summary(model3)
model3$residuals
sum(model3$residuals^2)


model5 <- lm(wine$Price ~ wine$Year + wine$AGST + wine$HarvestRain + wine$WinterRain + wine$Age + wine$FrancePop)
summary(model5)

model6 <- lm(wine$Price ~ wine$Year + wine$AGST + wine$HarvestRain + wine$WinterRain + wine$FrancePop)
summary(model6)


model8 <- lm(Price ~ FrancePop + AGST + HarvestRain + WinterRain, data = wine)
summary(model8)
abline(lm(Price ~ FrancePop + AGST + HarvestRain + WinterRain, data = wine))
model8$residuals #Errors
sum(model8$residuals^2) #SSE


model9 <- lm(Price ~ Age + AGST + HarvestRain + WinterRain, data = wine)
summary(model9)

plot(wine$WinterRain + wine$HarvestRain, wine$Price, pch = 20)
cor(wine$WinterRain + wine$HarvestRain, wine$Price)
lm(wine$Price ~ wine$WinterRain + wine$HarvestRain)
model4 <- lm(wine$Price ~ wine$WinterRain + wine$HarvestRain)
summary(model4)
model4$residuals
sum(model4$residuals^2)



plot(wine$HarvestRain, wine$Price, pch = 20)
cor(wine$HarvestRain, wine$Price)
lm(wine$Price ~ wine$HarvestRain)
abline(lm(wine$Price ~ wine$HarvestRain))


plot(wine$WinterRain, wine$Price, pch = 20)
cor(wine$WinterRain, wine$Price)
lm(wine$Price ~ wine$WinterRain)
abline(lm(wine$Price ~ wine$WinterRain))


plot(wine$FrancePop, wine$Price, pch = 20)
cor(wine$FrancePop, wine$Price)
lm(wine$Price ~ wine$FrancePop)
abline(lm(wine$Price ~ wine$FrancePop))

#Check coefficient of regression for each variable
cor(wine)

#Plot will be same if we change axis
plot(wine$FrancePop, wine$Age, pch = 20)
cor(wine$FrancePop, wine$Age)
plot(wine$Age, wine$FrancePop)
