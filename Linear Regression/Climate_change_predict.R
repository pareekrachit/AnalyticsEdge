predictTemp <- predict(TempReg2, newdata = climate_change_test)
predictTemp
SSE <- sum((predictTemp - climate_change_test$Temp)^2)
SSE
SST <- sum((mean(climate_change_training$Temp) - climate_change_test$Temp)^2)
SST
R2 <- 1 - (SSE/SST)
R2
