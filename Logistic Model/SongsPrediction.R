#Predict probability for song being in top10
predictSongs <- predict(SongsModel3, newdata = SongsTest, type = "response")

#Accuracy of model 
table(SongsTest$Top10, predictSongs > 0.45 )
328/(328+45)

#Baseline Accuracy
table(SongsTest$Top10)
314/(373)

#Sensitivity(Proportion of +ve's caught)
19/(19+40)

#Specitivity(Proportion of -ve's caught)
309/314