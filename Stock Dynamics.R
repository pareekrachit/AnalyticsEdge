#Snapshot of dataset
str(GE)

#Change data type date col to a form that R can understand
IBM$Date <- as.Date(IBM$Date, '%m/%d/%y')
Boeing$Date <- as.Date(Boeing$Date, '%m/%d/%y')
GE$Date <- as.Date(GE$Date, '%m/%d/%y')
CocaCola$Date <- as.Date(CocaCola$Date, '%m/%d/%y')
ProcterGamble$Date <- as.Date(ProcterGamble$Date, '%m/%d/%y')

#Earliest year in our datasets
max(GE$Date)

#Mean stock price
mean(IBM$StockPrice)

#Min stock price of GE
min(GE$StockPrice)

#Max stock price of Coca Cola
max(CocaCola$StockPrice)

#Median stock price of Boeing
median(Boeing$StockPrice)

#SD stock price of Procter & Gamble
sd(ProcterGamble$StockPrice)


#Plot the stock prices to see if we can visualize trends in stock prices during this time period
plot(CocaCola$Date, CocaCola$StockPrice, type="l",col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, lty = 2, col = 'blue')
abline(v=as.Date(c("1983-01-01")))


#How the stock prices changed from 1995-2005 for all five companies

which(GE$Date == '1995-01-01')
which(GE$Date == '2005-01-01')

plot(CocaCola$Date[301:421], CocaCola$StockPrice[301:421], type = 'l', ylim = c(0,200))
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = 'blue')
lines(Boeing$Date, Boeing$StockPrice, col = 'red')
lines(GE$Date, GE$StockPrice, col = 'green')
lines(IBM$Date, IBM$StockPrice, col = 'purple')
abline(v = as.Date(c("2004-01-01")))
abline(v = as.Date(c("2005-01-01")))


#Check if stocks tend to be higher or lower during certain months
IBM$Month <- months(IBM$Date)

tapply(IBM$StockPrice, IBM$Month, summary)
mean(IBM$StockPrice)

GE$Month <- months(GE$Date)
tapply(GE$StockPrice, GE$Month, summary)
mean(GE$StockPrice)

CocaCola$Month <- months(CocaCola$Date)
tapply(CocaCola$StockPrice, CocaCola$Month, summary)
mean(CocaCola$StockPrice)