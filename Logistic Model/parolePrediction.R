#Predict output on test set
predictParole <- predict(paroleModel, newdata = paroleTest, type = "response")

summary(predictParole)

table(paroleTest$violator, predictParole > 0.5)

#Accuracy
179/202

#Sensitivity
12/23

#Specificity
167/179

#Accuracy of baseline model
table(paroleTest$violator)
179/202
