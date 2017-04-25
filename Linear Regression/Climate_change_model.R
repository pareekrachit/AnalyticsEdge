climate_change_training <- climate_change[climate_change$Year <= 2006,]
climate_change_test <- climate_change[climate_change$Year > 2006,]

#Linear regression model to predict the dependent variable Temp
TempReg <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = climate_change_training)
summary(TempReg)

cor(climate_change_training)

TempReg1 <- lm(Temp ~ MEI + N2O + CFC.11 + TSI + Aerosols, data = climate_change_training)
summary(TempReg1)

#Using step fn to build a linear model
TempReg2 <- step(TempReg)
summary(TempReg2)
