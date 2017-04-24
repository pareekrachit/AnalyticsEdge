PointsPredictions = predict(PointsReg1, newdata = NBA_test)

SSE <- sum((PointsPredictions - NBA_test$PTS)^2)
SST <- sum((NBA_test$PTS - mean(NBA_train$PTS))^2)
R2 = 1 - SSE/SST
R2
