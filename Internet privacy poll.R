poll <- AnonymityPoll
#Number of people with smartphones
str(poll)
summary(poll)
sum(poll$Smartphone, na.rm = TRUE)
table(poll$Smartphone)

#States in the Midwest census region
table(poll$State, poll$Region == 'Midwest')

#State in the South census region with the largest number of interviewees
sort(table(poll$State[poll$Region == 'South']))

#Interviewees reported not having used the Internet and not having used a smartphone
table(poll$Internet.Use, poll$Smartphone)

#No interviewees have a missing value for their Internet use
summary(poll)

#No interviewees who reported Internet use or who reported smartphone use
Limited = subset(poll, Internet.Use == 1 | Smartphone == 1)

summary(Limited)

#Number of interviewees reported a value of 0 for Info.On.Internet
table(Limited$Info.On.Internet)

#What proportion of interviewees who answered the Worry.About.Info question worry about how much information is available about them on the Internet?
prop.table(table(poll$Worry.About.Info))

#What proportion of interviewees who answered the Anonymity.Possible question think it is possible to be completely anonymous on the Internet?
prop.table(table(poll$Anonymity.Possible))

#What proportion of interviewees who answered the Tried.Masking.Identity question have tried masking their identity on the Internet?
prop.table(table(poll$Tried.Masking.Identity))

#What proportion of interviewees who answered the Privacy.Laws.Effective question find United States privacy laws effective?
prop.table(table(poll$Privacy.Laws.Effective))

#Histogram of the age of interviewees
hist(poll$Age)

plot(Limited$Age, Limited$Info.On.Internet)

#What is the largest number of overlapping points in the plot plot(limited$Age, limited$Info.On.Internet)
sort(table(Limited$Age, Limited$Info.On.Internet))

jitter(c(1, 2, 3))
plot(jitter(Limited$Age), jitter(Limited$Info.On.Internet))
mean(Limited$Info.On.Internet[Limited$Age <= 30], na.rm = TRUE)
mean(Limited$Info.On.Internet[Limited$Age >= 60 ], na.rm = TRUE)


#What is the average Info.On.Internet value for smartphone users
mean(Limited$Info.On.Internet[Limited$Smartphone == 1], na.rm = TRUE)

#What is the average Info.On.Internet value for non-smartphone users
mean(Limited$Info.On.Internet[Limited$Smartphone == 0], na.rm = TRUE)

#What proportion of smartphone users who answered the Tried.Masking.Identity question have tried masking their identity when using the Internet?
prop.table(table(Limited$Tried.Masking.Identity, Limited$Smartphone),2)

rm(Limited, poll)
