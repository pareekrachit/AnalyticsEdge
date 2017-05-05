#Load data
elantra <- read.csv("~/The Analytics Edge/Week 2/elantra.csv")

elantraTrain <- elantra[elantra$Year <= 2012,]
elantraTest <- elantra[elantra$Year > 2012,]

str(elantraTrain)
plot(elantraTrain$Unemployment, elantraTrain$ElantraSales)
cor(elantraTrain)

#Basic linear model
saleReg <- lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + Month, data = elantraTrain)
summary(saleReg)

#Add month as a fatcor variable
elantraTrain$MonthFactor <- as.factor(elantraTrain$Month)
elantraTest$MonthFactor <- as.factor(elantraTest$Month)
View(elantraTest)

#Refined linear model
saleReg1 <- lm(ElantraSales ~ Unemployment + Queries + CPI_energy + CPI_all + MonthFactor, data = elantraTrain)
summary(saleReg1)

cor(elantraTrain[c('ElantraSales', 'Unemployment', 'Queries', 'CPI_energy', 'CPI_all', 'Month')])

saleReg2 <- lm(ElantraSales ~ Unemployment + CPI_energy + CPI_all + MonthFactor, data = elantraTrain)
summary(saleReg2)

#Predicted sale value
predSale <- predict(saleReg2, newdata = elantraTest)

SSE <- sum((predSale - elantraTest$ElantraSales)^2)

SST <- sum((mean(elantraTrain$ElantraSales) - elantraTest$ElantraSales)^2)

R2 <- 1 - SSE/SST
R2

which.max(abs(predSale - elantraTest$ElantraSales))
abs(c(predSale - elantraTest$ElantraSales))
