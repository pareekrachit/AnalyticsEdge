#Load dataset
loan <- read.csv("~/The Analytics Edge/Week 3/loans.csv")

summary(loan)
str(loan)

#Proportion of loan not paid in full
table(loan$not.fully.paid)
1533/(8045+1533)

#Create a dataset of rows which have null values
missing <- subset(loan, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
table(missing$not.fully.paid)

names(loan)
