#Read data
boston <- read.csv("~/The Analytics Edge/Week 4/Boston Housing Data/boston.csv")
str(boston)

#We are interested in building  a model of how prices vary by location over a region.

plot(boston$LON, boston$LAT)

# Tracts alongside the Charles River
plot(boston$LON, boston$LAT, col = ifelse(boston$CHAS == 1, "blue", "black"))

points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS==1], col="blue", pch=19)

# Plot MIT
points(boston$LON[boston$TRACT==3531], boston$LAT[boston$TRACT==3531], col="red", pch = 19)

#Plot pollution
summary(boston$NOX)
points(boston$LON[boston$NOX >= 0.55], boston$LAT[boston$NOX >= 0.55], col="green", pch = 19)

# Plot prices
plot(boston$LON, boston$LAT)
summary(boston$MEDV)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col="red", pch = 19)


#Linear Regression using Latitude and longitude
plot(boston$LON, boston$MEDV)
plot(boston$LAT, boston$MEDV)

latlonlm <- lm(MEDV ~ LAT + LON, data=boston)
summary(latlonlm)

# Visualize regression output
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col="red", pch = 19)

summary(latlonlm$fitted.values)
points(boston$LON[latlonlm$fitted.values >= 21.2], boston$LAT[latlonlm$fitted.values >= 21.2], col="blue", pch="$")

#Creating CART model using latitude and longitude
library(rpart)
library(rpart.plot)

latlontree <- rpart(MEDV ~ LAT + LON, data=boston)
prp(latlontree)

# Visualize regression output
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col="red", pch = 19)

fittedvalues <- predict(latlontree)
summary(fittedvalues)
points(boston$LON[fittedvalues>21.2], boston$LAT[fittedvalues>=21.2], col="blue", pch="$")

# Simplify tree by increasing minbucket
latlontree1 = rpart(MEDV ~ LAT + LON, data=boston, minbucket=50)
prp(latlontree1)
plot(latlontree1)
text(latlontree1)

# Visualize lowest price prediction
plot(boston$LON,boston$LAT)
abline(v=-71.07)
abline(h=42.28)
abline(h=42.17)
abline(h=42.21)

#Splitting the data
library(caTools)
set.seed(123)

split <- sample.split(boston$MEDV, SplitRatio = 0.7)
bostonTrain <- subset(boston, split == TRUE)
bostonTest <- subset(boston, split == FALSE)

# Create linear regression
pricelm <- lm(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = bostonTrain)
summary(pricelm)

#Predicting Values
pred.pricelm <- predict(pricelm, newdata = bostonTest)
pred.pricelm.sse = sum((pred.pricelm - bostonTest$MEDV)^2)
pred.pricelm.sse

#Create tree
pricetree <- rpart(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = bostonTrain)
prp(pricetree)

#Predicting Values
pred.pricetree <- predict(pricetree, newdata = bostonTest)
pred.pricetree.sse <- sum((pred.pricetree - bostonTest$MEDV)^2)
pred.pricetree.sse

#Load libraries for cross-validation
library(caret)
library(e1071)

#Number of folds
numFolds <- trainControl(method = "cv", number = 10)

#Possible values for cp
cpGrid <- expand.grid( .cp = (0:10)*0.001)
cpGrid

tr <- train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = bostonTrain, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

pricebesttree <- rpart(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = bostonTrain, cp = 0.003)
pricebesttree <- tr$finalModel
prp(pricebesttree)

#Predicting Values
pred.pricebesttree <- predict(pricebesttree, newdata = bostonTest)
pred.pricebesttree.sse <- sum((pred.pricebesttree - bostonTest$MEDV)^2)
pred.pricebesttree.sse
