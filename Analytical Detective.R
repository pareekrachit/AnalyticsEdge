#No of rows in data set
nrow(mvt)

#Print internal structure of data set
str(mvt)

#Statistical summary of dataset
summary(mvt)

#Check first entry of date
mvt$Date[1]

#Extract date out of Date column
DateConvert = as.Date(mvt$Date, "%m/%d/%y %H:%M")
head(DateConvert)
summary(DateConvert)

#Extract month and day out of DateConvert
mvt$Month <- months(DateConvert)
mvt$WeekDay <- weekdays(DateConvert)

str(mvt$Month)
str(mvt$WeekDay)

table(mvt$Month)
table(mvt$WeekDay)

mvt$Date <- DateConvert

str(mvt$Date)

#Which month has the largest number of motor vehicle thefts for which an arrest was made
table(mvt$Month, mvt$Arrest)

#Visualizing Crime Trends
hist(mvt$Date, breaks = 100)

boxplot(mvt$Date ~ mvt$Arrest)

#For what proportion of motor vehicle thefts in 2001 was an arrest made?
table(mvt$Year,mvt$Arrest)
prop.table(table(mvt$Year,mvt$Arrest),1)

#Top five locations where motor vehicle thefts occur
sort(table(mvt$LocationDescription))

#Create a subset of your data, only taking observations for which the theft happened in one of these five locations
Top5 <- subset(mvt, LocationDescription == 'GAS STATION' | LocationDescription == 'STREET' |LocationDescription == 'PARKING LOT/GARAGE(NON.RESID.)' | LocationDescription == 'ALLEY' | LocationDescription == 'DRIVEWAY - RESIDENTIAL')
nrow(Top5)

Top5$LocationDescription = factor(Top5$LocationDescription)
prop.table(table(Top5$LocationDescription, Top5$Arrest),1)

#On which day of the week do the most motor vehicle thefts at gas stations happen.
table(Top5$WeekDay[Top5$LocationDescription == 'GAS STATION'])
table(Top5$LocationDescription, Top5$WeekDay)
