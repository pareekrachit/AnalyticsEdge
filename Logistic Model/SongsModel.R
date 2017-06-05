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

