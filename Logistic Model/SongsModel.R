#Read data file
songs <- read.csv("~/The Analytics Edge/Week 3/songs.csv")

table(songs$year)

MichaelJackson <- subset(songs, artistname == "Michael Jackson")

#Songs of MJ which featured in Top10
MichaelJackson[MichaelJackson$Top10 == 1,c('songtitle', 'Top10')]

#Most frequent timesignature
table(songs$timesignature)

#Song with highest tempo
which.max(songs$tempo)
songs$songtitle[6206]

#Create training and test dataset
SongsTrain <- subset(songs, year <= 2009)
SongsTest <- subset(songs, year > 2009)

#Drop unnecessary variables
drop <- c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain <- SongsTrain[ , !(names(SongsTrain) %in% drop)]
SongsTest <- SongsTest[ , !(names(SongsTest) %in% drop)]

#Create model1
SongsModel1 <- glm(Top10 ~ ., data = SongsTrain, family = binomial)

summary(SongsModel1)

#Correlation
cor(SongsTrain$loudness, SongsTrain$energy)

#Create model2, remove loudness(multicollinearity)
SongsModel2 <- glm(Top10 ~ .-loudness, data = SongsTrain, family = binomial)
summary(SongsModel2)

#Create model3, remove energy(multicollinearity)
SongsModel3 <- glm(Top10 ~ .-energy, data = SongsTrain, family = binomial)
summary(SongsModel3)
