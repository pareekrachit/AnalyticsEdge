#Predicting reading score on basis of readingScoreReg model
predReadingScore <- predict(readingScoreReg, newdata = pisaTest1)

summary(predReadingScore)

SSE <- sum((predReadingScore - pisaTest1$readingScore)^2)
RMSE <- sqrt(SSE/nrow(pisaTest1))

SSE
RMSE

mean(pisaTrain1$readingScore)
SST <- sum((pisaTest1$readingScore - mean(pisaTrain1$readingScore))^2)
SST

R2 <- 1 - SSE/SST
R2
