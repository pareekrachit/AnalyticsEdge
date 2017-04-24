str(NBA_train)
summary(NBA_train)

#No of games a team need to win to qualify for playoffs
plot(NBA_train$W, NBA_train$SeasonEnd, pch = 20, col = ifelse(NBA_train$Playoffs == 1, "red", "blue"))

table(NBA_train$W, NBA_train$Playoffs)

#How many points a team should score to win a game

plot(NBA_train$PTS, NBA_train$W, col = ifelse(NBA_train$Playoffs == 1, "red", "blue"))
cor(NBA_train$PTS, NBA_train$W)

plot(NBA_train$oppPTS, NBA_train$W)
cor(NBA_train$oppPTS, NBA_train$W)

NBA_train$pd <- NBA_train$PTS - NBA_train$oppPTS

plot(NBA_train$pd, NBA_train$W, col = ifelse(NBA_train$Playoffs == 1, "red", "blue"))
cor(NBA_train$pd, NBA_train$W)
WinsReg = lm(W ~ pd, data = NBA_train)
WinsReg
summary(WinsReg)


#Create a model to predict PTS
PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK + TOV, data = NBA_train)
summary(PointsReg)
PointsReg1 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA_train)
summary(PointsReg1)
SSE <- sum((PointsReg$residuals)^2)
SSE1 <- sum((PointsReg1$residuals)^2)
SSE
sqrt(SSE/nrow(NBA_train))
mean(NBA_train$PTS)
SSE1
