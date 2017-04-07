#Summarizing the Dataset
str(CPSData)
summary(CPSData)

#Which state has the fewest interviewees
sort(table(CPSData$State))

#What proportion of interviewees are citizens of the United States
table(CPSData$Citizenship)

#The CPS differentiates between race and ethnicity.
table(CPSData$Race)
table(CPSData$Hispanic)
table(CPSData$Race, CPSData$Hispanic)

#Evaluating Missing Values
table(CPSData$Married)
is.na(CPSData$Married)
summary(CPSData$Married)

table(is.na(CPSData$Married), CPSData$Region)
table(is.na(CPSData$Married), CPSData$Sex)
table(is.na(CPSData$Married), CPSData$Age)
table(is.na(CPSData$Married), CPSData$Citizenship)

summary(CPSData$MetroAreaCode)
summary(CPSData$State)
prop.table(table(is.na(CPSData$MetroAreaCode), CPSData$Region),2)

prop.table(table(is.na(CPSData$MetroAreaCode), CPSData$State),2)

str(CountryCodes)
str(MetroAreaCodes)

#Integrating Metropolitan Area Data
CPSData <- merge(CPSData, MetroAreaCodes, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
sort(summary(CPSData$MetroArea))

#Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity
prop.table(table(CPSData$Hispanic, CPSData$MetroArea))
sort(prop.table(table(CPSData$Hispanic, CPSData$MetroArea)))

sort(tapply(CPSData$Hispanic, CPSData$MetroArea, mean))

#Determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian
table(CPSData$MetroArea, CPSData$Race)
sort(tapply(CPSData$Race == 'Asian', CPSData$MetroArea, mean))

#Proportion of interviewees from each metropolitan area who have not received a high school diploma
sort(tapply(CPSData$Education == "No high school diploma", CPSData$MetroArea, mean, na.rm=TRUE))

#Integrating Country Codes
CPSData <- merge(CPSData, CountryCodes, by.x = "CountryOfBirthCode", by.y = "Code", all.x = TRUE)

#Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(CPSData$Country))

#What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" metropolitan area have a country of birth that is not the United States?
summary(CPSData)
prop.table(table(CPSData$Country != 'United States' , CPSData$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA"),2)

#Which metropolitan area has the largest number of interviewees with a country of birth in India? 
table(CPSData$MetroArea, CPSData$Country == 'India')
sort(tapply(CPSData$Country == 'India', CPSData$MetroArea, sum, na.rm = TRUE))
sort(tapply(CPSData$Country == 'Brazil', CPSData$MetroArea, sum, na.rm = TRUE))
sort(tapply(CPSData$Country == 'Somalia', CPSData$MetroArea, sum, na.rm = TRUE))
