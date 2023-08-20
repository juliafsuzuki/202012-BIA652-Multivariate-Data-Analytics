rm(list = ls())

library(dplyr)
library(ggplot2)

#read files
cases = read.csv('Data Sets/FromKendra/casesbydate.csv')
deaths= read.csv('Data Sets/FromKendra/deathsbydate.csv')
stock = read.csv('Data Sets/FromKendra/SP500_dailydata.csv')
tests = read.csv('Data Sets/FromKendra/testsbydate.csv')

#change dates into date.time 
cases$date <- as.Date(cases$date, "%m/%d/%Y")
stock$Date <- as.Date(stock$Date, "%m/%d/%Y")
tests$date <- as.Date(tests$date, "%m/%d/%Y")
deaths$date <- as.Date(deaths$date, "%m/%d/%Y")

#change date so the column names are the same
names(stock)[names(stock)=="Date"] <- "date"

#merge data sets into single table
merged <- merge(stock[,1:5], cases[1:3], by="date")
merged <- merge(merged, deaths, by="date")
merged <- merge(merged, tests, by="date")

#rename columns in merged file
names(merged) <- c('date', 'open', 'high', 'low', 'close', 'cum_cases',
                   'daily_cases', 'cum_deaths', 'daily_deaths', 'cum_tests', 'daily_tests', 'positivity.rate')

View(merged)

#Select only the values we want to analyze
stock_covid.select <- merged[,c(1,3,7,9,11,12)]
View(stock_covid.select)

#Write to a document
write.csv(stock_covid.select,"Data Sets/FromKendra/stockvscovid_select.csv")

dev.off()
par(mar =c(3,3,3,3))

#Plot Daily Cases vs. Positivity Rate
plot(merged$positivity.rate, merged$high, 
     xlab="Positivity Rate", ylab="Stock Prices", main='Stock Price vs. Positive Covid Rate')
abline(lm(high ~ positivity.rate, data=merged), col='blue') #add regression line
#summary of regression line
summary(lm(high ~ positivity.rate, data=merged)) 

#create a model
model = lm(high ~ positivity.rate, data=merged)
#plot residuals
plot(fitted(model),resid(model), main="Residuals for Stock Price vs. Pos. Covid Rate")
#plot remaining analysis 
plot(model)

write.csv(merged,"~/R/Multivariate/Project/stockvscases.csv")


### Continued with Multiple Linear Regression
stock.reg=lm( high ~ daily_cases + daily_tests, data=stock_covid.select)
summary(stock.reg)

##This one is not better than the original, so stick with original
stock.reg2 = lm(high ~ positivity.rate + daily_deaths, data=stock_covid.select)
summary(stock.reg2)

## Plot Correlation Graphs:
par(mar =c(3,3,3,3))
plot(stock_covid.select[2:6])

#Look at residuals for our first model
res=residuals(stock.reg)
pred=fitted(stock.reg)

plot(stock.reg)

install.packages("car", dependencies = T)
library(car)
crPlots(stock.reg)
attach(stock_covid.select)

polynomial.reg=lm(high~poly(daily_cases,2)+poly(daily_tests,1))
summary(polynomial.reg)

res=resid(polynomial.reg)
qqnorm(res)
plot(polynomial.reg)

