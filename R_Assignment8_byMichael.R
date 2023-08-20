library(dplyr)
library(ggplot2)

rm(list = ls())

#read files
cases = read.csv('~/R/Multivariate/Project/casesbydate.csv')
stock = read.csv('R/Multivariate/Project/SP500_dailydata.csv')
tests = read.csv('R/Multivariate/Project/testsbydate.csv')

#change dates into date.time 
cases$date <- as.Date(cases$date, "%m/%d/%Y")
stock$Date <- as.Date(stock$Date, "%m/%d/%Y")
tests$date <- as.Date(tests$date, "%m/%d/%Y")

#change date so the column names are the same
names(stock)[names(stock)=="Date"] <- "date"

#merge data sets into single table
merged <- merge(stock[,1:5], cases[1:3], by="date")
merged <- merge(merged, tests, by="date")

#rename columns in merged file
names(merged) <- c('date', 'open', 'high', 'low', 'close', 'cum_cases',
                   'daily_cases', 'cum_tests', 'daily_tests', 'positivity.rate')

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


#Multi-linear regression

Stock <- read.csv("C:\\Users\\michael.weiss\\OneDrive - Accenture\\Job\\Stevens Graduate Certificate\\Multivariate Analysis\\Group project\\stockvscases.csv", header=TRUE)

#summary of regression line
summary(lm(high ~ positivity.rate + daily_cases + daily_deaths, data=Stock))

#summary of regression line
summary(lm(high ~ positivity.rate + daily_cases, data=Stock))
