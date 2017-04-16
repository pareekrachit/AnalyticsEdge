predictTest <- predict(model8, newdata = wine_test)
predictTest
str(wine_test)

#Compute R^2 for Test data
SSE <- sum((predictTest - wine_test$Price)^2)
SST <- sum((wine_test$Price - mean(wine_test$Price))^2)













predictTest1 <- predict(model9, newdata = wine_test)
predictTest1

