#
# Assignment #4
# Julia Suzuki
#

rm(list = ls())

x <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\COVID-19_National-History_csv.csv", header=TRUE)
# x <- na.omit(x)

# str(x)
# summary(new_death)

## Delete rows with missing value

#############################################################################

str(x)

cumulative.death <- x$death / 1000
summary(cumulative.death)

# Create a Time-series graph on the no. of death by COVID-19
plot(cumulative.death~as.Date(x$date,"%Y-%m-%d"),
     main="Cumulative Deaths by COVID-19",
     xlab="Time", ylab="No. of Deaths (in Thousand)",
     xlim=as.Date(c("2020-01-22", "2020-10-06")),
     ylim=c(0, 210))

# Add a regression line
abline(lm(cumulative.death~as.Date(x$date,"%Y-%m-%d")), col="red")

#

daily.death <- x$deathIncrease / 1000
summary(daily.death)

# Create a Time-series graph on the no. of death by COVID-19
plot(daily.death~as.Date(x$date,"%Y-%m-%d"),
     main="Daily Deaths by COVID-19",
     xlab="Time", ylab="No. of Deaths (in Thousand)",
     xlim=as.Date(c("2020-01-22", "2020-10-06")),
     ylim=c(0, 3))

# Add a regression line
abline(lm(daily.death~as.Date(x$date,"%Y-%m-%d")), col="red")

#

daily.positivity.rate <- x$dailyPositivityRate * 100
daily.death.rate <- x$dailyDeathRate * 100 
summary(daily.positivity.rate)
summary(daily.death.rate)

# Create a Time-series graph on the no. of death by COVID-19
plot(daily.death.rate~daily.positivity.rate,
     main="Daily Positivity Rate vs. Daily Death Rate",
     xlab="Positivity Rate (Percent)", ylab="Death Rate (Percent)",
     xlim=c(3, 40),
     ylim=c(0, 12))

# Add a regression line
abline(lm(daily.death.rate~daily.positivity.rate), col="red")

#############################################################################

cumulative.positive.Case <- x$positive / 1000000
summary(cumulative.positive.Case)

# Create a Time-series graph on the no. of death by COVID-19
plot(cumulative.positive.Case~as.Date(x$date,"%Y-%m-%d"),
     main="Cumulrative COVID-19 Positive Cases",
     xlab="Time", ylab="No. of Positive Cases (in Million)",
     xlim=as.Date(c("2020-01-22", "2020-10-06")),
     ylim=c(0, 8))

# Add a regression line
abline(lm(cumulative.positive.Case~as.Date(x$date,"%Y-%m-%d")), col="red")

#

daily.positive.Case <- x$positiveIncrease / 1000
summary(daily.positive.Case)

# Create a Time-series graph on the no. of death by COVID-19
plot(daily.positive.Case~as.Date(x$date,"%Y-%m-%d"),
     main="Daily COVID-19 Positive Cases",
     xlab="Time", ylab="No. of Positive Cases (in Thousand)",
     xlim=as.Date(c("2020-01-22", "2020-10-06")),
     ylim=c(0, 80))

# Add a regression line
abline(lm(daily.positive.Case~as.Date(x$date,"%Y-%m-%d")), col="red")

#############################################################################

x$death = x$death / 1000
x$hospitalizedCumulative = x$hospitalizedCumulative / 1000

plot(x$death~x$hospitalizedCumulative, 
     main="Correlation between Death and Hospitalization", 
     xlab="Hospitalization", ylab="Number of Deaths",
     xlim=c(0, 400),
     ylim=c(0, 200))

# Add a regression line
abline(lm(new_death~x$hospitalizedCumulative), col="red")



#############################################################################

y <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\Association_byAgeDepression.csv", header=TRUE)

str(y)
summary(y)

positive_case <- y$Positive.Case / 1000
death <- y$Death / 1000

positive_case <- y$Positive.Case / 1000
death <- Death / 1000

summary(positive_case)
summary(death)

## Plot

plot(positive_case, death,
     main="Positive Case vs. Deaths",
     xlab="Positive Cases (in Thousand)", ylab="Deaths (in Thousand)",
     col="black",
     xlim=c(1000, 7000),
     ylim=c(50, 200))

# Add a regression line
abline(lm(death~positive_case, data=y), col="violet")

## ggplot
ggplot(y, aes(y=death, x=positive_case, size=death, colour=y$"ï..Date")) + geom_point(show.legend=TRUE, na.rm=TRUE) + labs(title="Positive Case vs. Deaths", x="Positive Case (in Thousand)", y="Deaths (in Thousand)")




#############################################################################

## Sentiment Analysis by Age

# install.packages("ggplot2")

# library(ggplot2)

xx <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\Health_Sentiment_Summary_byAge.csv", header=TRUE)

str(xx)

# Ensured data is in integer
str(xx$Anxiety.Nearly.everyday)
str(xx$"Worry...Nearly.everyday")
str(xx$"Low.Interest...Nearly.everyday")
str(xx$"Depressed...Nearly.everyday")
str(xx)

# Summarized data
summary(xx$"Anxiety.Nearly.everyday")
summary(xx$"Worry...Nearly.everyday")
summary(xx$"Low.Interest...Nearly.everyday")

summary(xx$"Depressed..Not.at.all")
summary(xx$"Depressed...Several.days")
summary(xx$"Depressed...More.than.half.the.days")
summary(xx$"Depressed...Nearly.everyday")

# Normalize data
anxiety<-xx$"Anxiety.Nearly.everyday"/1000000
worry<-xx$"Worry...Nearly.everyday"/1000000
low_interest<-xx$"Low.Interest...Nearly.everyday"/1000000

d1<-xx$"Depressed..Not.at.all"/1000
d2<-xx$"Depressed...Several.days"/1000
d3<-xx$"Depressed...More.than.half.the.days"/1000
d4<-xx$"Depressed...Nearly.everyday"/1000

# Summarized data
summary(anxiety)
summary(worry)
summary(low_interest)

summary(d1)
summary(d2)
summary(d3)
summary(d4)

##

# Anxiety
ggplot(xx, aes(y=anxiety, x=as.Date(End.Date,"%m/%d/%y"), size=anxiety, colour=Age)) + geom_point(show.legend=TRUE, na.rm=TRUE) + labs(title="ANXIETY | Experienced Nearly Everyday", x="Time", y="No. of People (in Million)")
ggplot(xx, aes(y=anxiety, x=as.Date(End.Date,"%m/%d/%y"), size=anxiety, colour=Age)) + geom_point() + stat_smooth(method="lm") + labs(title="ANXIETY | Experienced Nearly Everyday", x="Time", y="No. of People (in Million)")

# Worry
ggplot(xx, aes(y=worry, x=as.Date(End.Date,"%m/%d/%y"), size=worry, colour=Age)) + geom_point(show.legend=TRUE, na.rm=TRUE) + labs(title="WORRY | Experienced Nearly Everyday", x="Time", y="No. of People (in Million)")
ggplot(xx, aes(y=worry, x=as.Date(End.Date,"%m/%d/%y"), size=worry, colour=Age)) + geom_point() + stat_smooth(method="lm") + labs(title="WORRY | Experienced Nearly Everyday", x="Time", y="No. of People (in Million)")

# Low Interest
ggplot(xx, aes(y=low_interest, x=as.Date(End.Date,"%m/%d/%y"), size=low_interest, colour=Age)) + geom_point(show.legend=TRUE, na.rm=TRUE) + labs(title="LOW INTEREST | Nearly Everyday", x="Time", y="No. of People (in Million)")
ggplot(xx, aes(y=low_interest, x=as.Date(End.Date,"%m/%d/%y"), size=low_interest, colour=Age)) + geom_point() + stat_smooth(method="lm") + labs(title="LOW INTEREST | Nearly Everyday", x="Time", y="No. of People (in Million)")

# Depressed
ggplot(xx, aes(y=depressed, x=as.Date(End.Date,"%m/%d/%y"), size=depressed, colour=Age)) + geom_point(show.legend=TRUE, na.rm=TRUE) + labs(title="DEPRESSED | Nearly Everyday", x="Time", y="No. of People (in Million)")
ggplot(xx, aes(y=depressed, x=as.Date(End.Date,"%m/%d/%y"), size=depressed, colour=Age)) + geom_point() + stat_smooth(method="lm") + labs(title="DEPRESSED | Nearly Everyday", x="Time", y="No. of People (in Million)")

# Depressed (4)
ggplot(xx, aes(y=d1, x=as.Date(End.Date,"%m/%d/%y"), size=depressed, colour=Age)) + geom_point(show.legend=TRUE, na.rm=TRUE) + labs(title="DEPRESSED | Not At All", x="Time", y="No. of People (in Thousand)")
ggplot(xx, aes(y=d2, x=as.Date(End.Date,"%m/%d/%y"), size=depressed, colour=Age)) + geom_point(show.legend=TRUE, na.rm=TRUE) + labs(title="DEPRESSED | Several Days", x="Time", y="No. of People (in Thousand)")
ggplot(xx, aes(y=d3, x=as.Date(End.Date,"%m/%d/%y"), size=depressed, colour=Age)) + geom_point(show.legend=TRUE, na.rm=TRUE) + labs(title="DEPRESSED | More Than Half The Days", x="Time", y="No. of People (in Thousand)")
ggplot(xx, aes(y=d4, x=as.Date(End.Date,"%m/%d/%y"), size=depressed, colour=Age)) + geom_point(show.legend=TRUE, na.rm=TRUE) + labs(title="DEPRESSED | Nearly Everyday", x="Time", y="No. of People (in Thousand)")

# ggplot(xx, aes(anxiety, colour=Age)) + geom_histogram()



#############################################################################

## Create Histograms

# Data Prep
summary(xx$"Anxiety.Nearly.everyday")
summary(xx$"Worry...Nearly.everyday")
summary(xx$"Low.Interest...Nearly.everyday")
summary(xx$"Depressed...Nearly.everyday")

hist_anxiety_byAge<-c(xx$"Anxiety.Nearly.everyday"/1000000)
hist_worry_byAge<-c(xx$"Worry...Nearly.everyday"/1000000)
hist_low_interest_byAge<-c(xx$"Low.Interest...Nearly.everyday"/1000000)
hist_depressed_byAge<-c(xx$"Depressed...Nearly.everyday"/1000000)

str(hist_anxiety_byAge)
str(hist_worry_byAge)
str(hist_low_interest_byAge)
str(hist_depressed_byAge)

summary(hist_anxiety_byAge)
summary(hist_worry_byAge)
summary(hist_low_interest_byAge)
summary(hist_depressed_byAge)

#

h1<-hist(hist_anxiety_byAge,
     main="Feeling Anxiety Every Day",
     xlab="No. of People (in Million)", border="black", col="light gray")
xfit<-seq(min(hist_anxiety_byAge), max(hist_anxiety_byAge), length=12)

yfit<-dnorm(xfit, mean=mean(hist_anxiety_byAge), sd=sd(hist_anxiety_byAge))
yfit<-yfit*diff(h1$mids[1:2]*length(hist_anxiety_byAge))

lines(xfit, yfit, col="purple")

#

h2<-hist(hist_worry_byAge,
     main="Worrying Every Day",
     xlab="No. of People (in Million)", border="black", col="light gray",
     breaks=7, xlim=c(0, 10))

xfit<-seq(min(hist_anxiety_byAge), max(hist_anxiety_byAge), length=12)

yfit<-dnorm(xfit, mean=mean(hist_anxiety_byAge), sd=sd(hist_anxiety_byAge))
yfit<-yfit*diff(h2$mids[1:2]*length(hist_anxiety_byAge))

lines(xfit, yfit, col="purple")

#

h3<-hist(hist_low_interest_byAge,
     main="Having Low Interest Every Day",
     xlab="No. of People (in Million)", border="black", col="light gray")

xfit<-seq(min(hist_anxiety_byAge), max(hist_anxiety_byAge), length=12)

yfit<-dnorm(xfit, mean=mean(hist_anxiety_byAge), sd=sd(hist_anxiety_byAge))
yfit<-yfit*diff(h3$mids[1:2]*length(hist_anxiety_byAge))

lines(xfit, yfit, col="purple")

#

h4<-hist(hist_depressed_byAge,
     main="Depressed Every Day",
     xlab="No. of People (in Million)", border="black", col="light gray")

xfit<-seq(min(hist_anxiety_byAge), max(hist_anxiety_byAge), length=8)

yfit<-dnorm(xfit, mean=mean(hist_anxiety_byAge), sd=sd(hist_anxiety_byAge))
yfit<-yfit*diff(h4$mids[1:2]*length(hist_anxiety_byAge))

lines(xfit, yfit, col="purple")

#



#############################################################################

## Sentiment Analysis by Income

xxx <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\Health_Sentiment_Summary_byIncome.csv", header=TRUE)

# Ensured data is in integer
str(xxx$Anxiety.Nearly.everyday)
str(xxx$"Worry...Nearly.everyday")
str(xxx$"Low.Interest...Nearly.everyday")

# Summarized data
summary(xxx$"Anxiety.Nearly.everyday")
summary(xxx$"Worry...Nearly.everyday")
summary(xxx$"Low.Interest...Nearly.everyday")

# Normalize data
anxiety<-xxx$"Anxiety.Nearly.everyday"/1000000
worry<-xxx$"Worry...Nearly.everyday"/1000000
low_interest<-xxx$"Low.Interest...Nearly.everyday"/1000000

# Summarized data
summary(anxiety)
summary(worry)
summary(low_interest)

##

# Anxiety
ggplot(xxx, aes(y=anxiety, x=as.Date(End.Date,"%m/%d/%y"), size=anxiety, colour=Income)) + geom_point(show.legend=TRUE, na.rm=TRUE) + labs(title="ANXIETY Nearly Everyday by Income", x="Time", y="No. of People (in Million)")
ggplot(xxx, aes(y=anxiety, x=as.Date(End.Date,"%m/%d/%y"), size=anxiety, colour=Income)) + geom_point() + stat_smooth(method="lm") + labs(title="ANXIETY Nearly Everyday by Income", x="Time", y="No. of People (in Million)")

# Worry
ggplot(xxx, aes(y=worry, x=as.Date(End.Date,"%m/%d/%y"), size=worry, colour=Income)) + geom_point(show.legend=TRUE, na.rm=TRUE) + labs(title="WORRY Nearly Everyday by Income", x="Time", y="No. of People (in Million)")
ggplot(xxx, aes(y=worry, x=as.Date(End.Date,"%m/%d/%y"), size=worry, colour=Income)) + geom_point() + stat_smooth(method="lm") + labs(title="WORRY Nearly Everyday by Income", x="Time", y="No. of People (in Million)")

# Low Interest
ggplot(xxx, aes(y=low_interest, x=as.Date(End.Date,"%m/%d/%y"), size=low_interest, colour=Income)) + geom_point(show.legend=TRUE, na.rm=TRUE) + labs(title="LOW INTEREST Nearly Everyday by Age", x="Time", y="No. of People (in Million)")
ggplot(xxx, aes(y=low_interest, x=as.Date(End.Date,"%m/%d/%y"), size=low_interest, colour=Income)) + geom_point() + stat_smooth(method="lm") + labs(title="WORRY Nearly Everyday by Income", x="Time", y="No. of People (in Million)")



#############################################################################

## Create Histograms

# Data Prep
summary(xxx$"Anxiety.Nearly.everyday")
summary(xxx$"Worry...Nearly.everyday")
summary(xxx$"Low.Interest...Nearly.everyday")
summary(xxx$"Depressed...Nearly.everyday")

hist_anxiety_byIncome<-c(xxx$"Anxiety.Nearly.everyday"/1000000)
hist_worry_byIncome<-c(xxx$"Worry...Nearly.everyday"/1000000)
hist_low_interest_byIncome<-c(xxx$"Low.Interest...Nearly.everyday"/1000000)
hist_depressed_byIncome<-c(xxx$"Depressed...Nearly.everyday"/1000000)

str(hist_anxiety_byIncome)
str(hist_worry_byIncome)
str(hist_low_interest_byIncome)
str(hist_depressed_byIncome)

summary(hist_anxiety_byIncome)
summary(hist_worry_byIncome)
summary(hist_low_interest_byIncome)
summary(hist_depressed_byIncome)

#

h1<-hist(hist_anxiety_byIncome,
         main="Feeling Anxiety Every Day",
         xlab="No. of People (in Million)", border="black", col="light gray")
xfit<-seq(min(hist_anxiety_byIncome), max(hist_anxiety_byIncome), length=12)

yfit<-dnorm(xfit, mean=mean(hist_anxiety_byIncome), sd=sd(hist_anxiety_byIncome))
yfit<-yfit*diff(h1$mids[1:2]*length(hist_anxiety_byIncome))

lines(xfit, yfit, col="blue")

#

h2<-hist(hist_worry_byIncome,
         main="Worrying Every Day",
         xlab="No. of People (in Million)", border="black", col="light gray",
         breaks=7, xlim=c(0, 10))

xfit<-seq(min(hist_worry_byIncome), max(hist_worry_byIncome), length=12)

yfit<-dnorm(xfit, mean=mean(hist_worry_byIncome), sd=sd(hist_worry_byIncome))
yfit<-yfit*diff(h2$mids[1:2]*length(hist_worry_byIncome))

lines(xfit, yfit, col="blue")

#

h3<-hist(hist_low_interest_byIncome,
         main="Having Low Interest Every Day",
         xlab="No. of People (in Million)", border="black", col="light gray")

xfit<-seq(min(hist_low_interest_byIncome), max(hist_low_interest_byIncome), length=12)

yfit<-dnorm(xfit, mean=mean(hist_low_interest_byIncome), sd=sd(hist_low_interest_byIncome))
yfit<-yfit*diff(h3$mids[1:2]*length(hist_low_interest_byIncome))

lines(xfit, yfit, col="blue")

#

h4<-hist(hist_depressed_byIncome,
         main="Depressed Every Day",
         xlab="No. of People (in Million)", border="black", col="light gray",
         breaks=7, xlim=c(0, 10))

xfit<-seq(min(hist_depressed_byIncome), max(hist_depressed_byIncome), length=10)

yfit<-dnorm(xfit, mean=mean(hist_depressed_byIncome), sd=sd(hist_depressed_byIncome))
yfit<-yfit*diff(h4$mids[1:2]*length(hist_depressed_byIncome))

lines(xfit, yfit, col="blue")

#



#############################################################################

y <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\Association_byAgeAnxiety.csv", header=TRUE)

str(y)
summary(y)

positive_case <- y$Positive.Case / 1000
death <- y$Death / 1000

positive_case <- y$Positive.Case / 1000
death <- Death / 1000

ageGroup1 <- y$"X18...29"/1000
ageGroup2 <- y$"X30...39"/1000
ageGroup3 <- y$"X40...49"/1000
ageGroup4 <- y$"X50...59"/1000
ageGroup5 <- y$"X60...69"/1000
ageGroup6 <- y$"X70...79"/1000
ageGroup7 <- y$"X..80"/1000

print(positive_case)
print(death)

#print (new_death)
#summary(new_death)



## Plot for Age Group 1

plot(positive_case, ageGroup1,
     main="Positive Case vs. Anxiety (Age Group: 18-29)",
     xlab="Positive Cases", ylab="No. of People",
     col="black",
     ylim=c(100, 11000))

# Add a regression line
abline(lm(ageGroup1~positive_case, data=y), col="red")



## Plot for Age Group 2

plot(positive_case, ageGroup2,
     main="Positive Case vs. Anxiety (Age Group: 30-39)",
     xlab="Positive Cases", ylab="No. of People",
     col="black",
     ylim=c(100, 11000))

# Add a regression line
abline(lm(ageGroup2~positive_case, data=y), col="red")



## Plot for Age Group 3

plot(positive_case, ageGroup3,
     main="Positive Case vs. Anxiety (Age Group: 40-49)",
     xlab="Positive Cases", ylab="No. of People",
     col="black",
     ylim=c(100, 11000))

# Add a regression line
abline(lm(ageGroup3~positive_case, data=y), col="red")



## Plot for Age Group 4

plot(positive_case, ageGroup4,
     main="Positive Case vs. Anxiety (Age: 50-59)",
     xlab="Positive Cases", ylab="No. of People",
     col="black",
     ylim=c(100, 11000))

# Add a regression line
abline(lm(ageGroup4~positive_case, data=y), col="red")



## Plot for Age Group 5

plot(positive_case, ageGroup5,
     main="Positive Case vs. Anxiety (Age: 60-69)",
     xlab="Positive Cases", ylab="No. of People",
     col="black",
     ylim=c(100, 11000))

# Add a regression line
abline(lm(ageGroup5~positive_case, data=y), col="red")



## Plot for Age Group 6

plot(positive_case, ageGroup6,
     main="Positive Case vs. Anxiety (Age: 70-79)",
     xlab="Positive Cases", ylab="No. of People",
     col="black",
     ylim=c(100, 11000))

# Add a regression line
abline(lm(ageGroup6~positive_case, data=y), col="red")



## Plot for Age Group 7

plot(positive_case, ageGroup7,
     main="Positive Case vs. Anxiety (Age: Over 80)",
     xlab="Positive Cases", ylab="No. of People",
     col="black",
     ylim=c(100, 11000))

# Add a regression line
abline(lm(ageGroup7~positive_case, data=y), col="red")



#############################################################################

y <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\Association_byAgeDepression.csv", header=TRUE)

str(y)
summary(y)

positive_case <- y$Positive.Case / 1000
death <- y$Death / 1000

positive_case <- y$Positive.Case / 1000
death <- Death / 1000

ageGroup1 <- y$"X18...29"/1000
ageGroup2 <- y$"X30...39"/1000
ageGroup3 <- y$"X40...49"/1000
ageGroup4 <- y$"X50...59"/1000
ageGroup5 <- y$"X60...69"/1000
ageGroup6 <- y$"X70...79"/1000
ageGroup7 <- y$"X..80"/1000

print(positive_case)
print(death)

#print (new_death)
#summary(new_death)



## Plot for Age Group 1

plot(positive_case, ageGroup1,
     main="Positive Case vs. Depression (Age Group: 18-29)",
     xlab="Positive Cases", ylab="No. of People",
     col="black",
     ylim=c(100, 11000))

# Add a regression line
abline(lm(ageGroup1~positive_case, data=y), col="orange")



## Plot for Age Group 2

plot(positive_case, ageGroup2,
     main="Positive Case vs. Depression (Age Group: 30-39)",
     xlab="Positive Cases", ylab="No. of People",
     col="black",
     ylim=c(100, 11000))

# Add a regression line
abline(lm(ageGroup2~positive_case, data=y), col="orange")



## Plot for Age Group 3

plot(positive_case, ageGroup3,
     main="Positive Case vs. Depression (Age Group: 40-49)",
     xlab="Positive Cases", ylab="No. of People",
     col="black",
     ylim=c(100, 11000))

# Add a regression line
abline(lm(ageGroup3~positive_case, data=y), col="orange")



## Plot for Age Group 4

plot(positive_case, ageGroup4,
     main="Positive Case vs. Depression (Age: 50-59)",
     xlab="Positive Cases", ylab="No. of People",
     col="black",
     ylim=c(100, 11000))

# Add a regression line
abline(lm(ageGroup4~positive_case, data=y), col="orange")



## Plot for Age Group 5

plot(positive_case, ageGroup5,
     main="Positive Case vs. Depression (Age: 60-69)",
     xlab="Positive Cases", ylab="No. of People",
     col="black",
     ylim=c(100, 11000))

# Add a regression line
abline(lm(ageGroup5~positive_case, data=y), col="orange")



## Plot for Age Group 6

plot(positive_case, ageGroup6,
     main="Positive Case vs. Depression (Age: 70-79)",
     xlab="Positive Cases", ylab="No. of People",
     col="black",
     ylim=c(100, 11000))

# Add a regression line
abline(lm(ageGroup6~positive_case, data=y), col="orange")



## Plot for Age Group 7

plot(positive_case, ageGroup7,
     main="Positive Case vs. Depression (Age: Over 80)",
     xlab="Positive Cases", ylab="No. of People",
     col="black",
     ylim=c(100, 11000))

# Add a regression line
abline(lm(ageGroup7~positive_case, data=y), col="orange")



#############################################################################

y <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\Association_byStatePositiveCases.csv", header=TRUE)

str(y)
summary(y)

pc_MA <- y$MA/1000
pc_CO <- y$CO/1000
pc_CA <- y$CA/1000
pc_MI <- y$MI/1000

summary(pc_MA)
summary(pc_CO)
summary(pc_CA)
summary(pc_MI)

print (y$ï..Date)



## Plot for MA

plot(pc_MA~as.Date(y$"ï..Date","%m/%d/%y"),
     main="Positive Cases in MA",
     xlab="Time", ylab="Positive Cases (in Thousand)",
     xlim=as.Date(c("2020-05-05", "2020-09-14")),
     ylim=c(0, 800))

# Add a regression line
abline(lm(pc_MA~as.Date(y$"ï..Date","%m/%d/%y")), col="red")



## Plot for CO

plot(pc_CO~as.Date(y$"ï..Date","%m/%d/%y"),
     main="Positive Cases in CO",
     xlab="Time", ylab="Positive Cases (in Thousand)",
     xlim=as.Date(c("2020-05-05", "2020-09-14")),
     ylim=c(0, 800))

# Add a regression line
abline(lm(pc_CO~as.Date(y$"ï..Date","%m/%d/%y")), col="red")



## Plot for CA

plot(pc_CA~as.Date(y$"ï..Date","%m/%d/%y"),
     main="Positive Cases in CA",
     xlab="Time", ylab="Positive Cases (in Thousand)",
     xlim=as.Date(c("2020-05-05", "2020-09-14")),
     ylim=c(0, 800))

# Add a regression line
abline(lm(pc_CA~as.Date(y$"ï..Date","%m/%d/%y")), col="red")



## Plot for MI

plot(pc_MI~as.Date(y$"ï..Date","%m/%d/%y"),
     main="Positive Cases in MI",
     xlab="Time", ylab="Positive Cases (in Thousand)",
     xlim=as.Date(c("2020-05-05", "2020-09-14")),
     ylim=c(0, 800))

# Add a regression line
abline(lm(pc_MI~as.Date(y$"ï..Date","%m/%d/%y")), col="red")



# Boxplot

boxplot(pc_MA, pc_CO, pc_CA, pc_MI,
        names=c("MA", "CO", "CA", "MI"),
        horizontal=FALSE,
        border="gray", col=c("dark red", "red"),
        main="Positive Cases by State",
        xlab="States",
        ylab="Positive Cases (in Thousand)",
        ylim=c(0, 800))



#############################################################################

y <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\Association_byStateDeaths.csv", header=TRUE)

str(y)
summary(y)

d_MA <- y$MA/1000
d_CO <- y$CO/1000
d_CA <- y$CA/1000
d_MI <- y$MI/1000

summary(d_MA)
summary(d_CO)
summary(d_CA)
summary(d_MI)

print (y$ï..Date)



## Plot for MA

plot(d_MA~as.Date(y$"ï..Date","%m/%d/%y"),
     main="Deaths in MA",
     xlab="Time", ylab="Deaths (in Thousand)",
     xlim=as.Date(c("2020-05-05", "2020-09-14")),
     ylim=c(0, 15))

# Add a regression line
abline(lm(d_MA~as.Date(y$"ï..Date","%m/%d/%y")), col="blue")



## Plot for CO

plot(d_CO~as.Date(y$"ï..Date","%m/%d/%y"),
     main="Deaths in CO",
     xlab="Time", ylab="Deaths (in Thousand)",
     xlim=as.Date(c("2020-05-05", "2020-09-14")),
     ylim=c(0, 15))

# Add a regression line
abline(lm(d_CO~as.Date(y$"ï..Date","%m/%d/%y")), col="blue")



## Plot for CA

plot(d_CA~as.Date(y$"ï..Date","%m/%d/%y"),
     main="Deaths in CA",
     xlab="Time", ylab="Deaths (in Thousand)",
     xlim=as.Date(c("2020-05-05", "2020-09-14")),
     ylim=c(0, 15))

# Add a regression line
abline(lm(d_CA~as.Date(y$"ï..Date","%m/%d/%y")), col="blue")



## Plot for MI

plot(d_MI~as.Date(y$"ï..Date","%m/%d/%y"),
     main="Deaths in MI",
     xlab="Time", ylab="Deaths (in Thousand)",
     xlim=as.Date(c("2020-05-05", "2020-09-14")),
     ylim=c(0, 15))

# Add a regression line
abline(lm(d_MI~as.Date(y$"ï..Date","%m/%d/%y")), col="blue")



# Boxplot

boxplot(d_MA, d_CO, d_CA, d_MI,
        names=c("MA", "CO", "CA", "MI"),
        horizontal=FALSE,
        border="gray", col=c("dark red", "red"),
        main="Deaths by State",
        xlab="States",
        ylab="Deaths (in Thousand)",
        ylim=c(0, 15))



#############################################################################

## Histogram | Unemployment Rate

z <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\Association_byYearUnemploymentRate.csv", header=TRUE)

str(z)

pc <- z$Positive.Case/1000
d <- z$Death/1000
ur <- z$Unemployment.Rate
ur <- ur[!is.na(ur)]
dc <- density(ur[!is.na(ur)])


summary(pc)
summary(d)


#
h<-hist(ur,
        main="Histogram | National Unemployment Rates (2010 - 2020)",
        xlab="Unemployment Rates", ylab="Frequency", 
        xlim=c(4, 16), ylim=c(0, 70),
        breaks=c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16),
        border="black", col="light gray")

xfit<-seq(min(ur), max(ur), length=12)

yfit<-dnorm(xfit, mean=mean(ur), sd=sd(ur))
yfit<-yfit*diff(h1$mids[1:2]*length(ur))

lines(xfit, yfit, col="purple")



#
plot(dc, 
     main="Kernel Density | Unemployment Rate (2010 - 2020)")
polygon(dc, col="gray", border="black")



#############################################################################

## Histogram | Unemployment Rate

z <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\Association_byCOVID-19UnemploymentRate.csv", header=TRUE)

print (z)
str(z)

pc <- z$Positive.Case/1000
ue <- z$Unemployment.Rate
d <- z$Death/1000

summary(pc)
summary(ue)
summary(d)

# Plot the association | Positive Case & Unemployment

plot(x=pc, y=ue,
     main="Positive Cases vs. Unemployment Rates",
     xlab="Positive Cases (in Thousand)", ylab="Unemployment Rates (%)",
     col="black",
     xlim=c(0, 7000),
     ylim=c(3, 15))

abline(lm(ue~pc, data=z), col="seagreen")



# Plot the association | Death & Unemployment

plot(x=d, y=ue,
     main="Deaths vs. Unemployment Rates",
     xlab="Deaths (in Thousand)", ylab="Unemployment Rates (%)",
     col="black",
     xlim=c(0, 200),
     ylim=c(3, 15))

abline(lm(ue~d, data=z), col="seagreen")



#############################################################################

y <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\Association_byAgeDepression.csv", header=TRUE)

str(y)
summary(y)

positive_case <- y$Positive.Case / 1000
death <- y$Death / 1000

positive_case <- y$Positive.Case / 1000
death <- Death / 1000

summary(positive_case)
summary(death)

## Plot

plot(positive_case, death,
     main="Positive Case vs. Deaths",
     xlab="Positive Cases (in Thousand)", ylab="Deaths (in Thousand)",
     col="black",
     xlim=c(1000, 7000),
     ylim=c(50, 200))

# Add a regression line
abline(lm(death~positive_case, data=y), col="violet")

## ggplot
ggplot(y, aes(y=death, x=positive_case, size=death, colour=y$"ï..Date")) + geom_point(show.legend=TRUE, na.rm=TRUE) + labs(title="Positive Case vs. Deaths", x="Positive Case (in Thousand)", y="Deaths (in Thousand)")

##

y=rt(21, df=20)

--

# Assignment, Problem 5.87 / 5.95 c

# normalcdf(lower, upper, mu = 0, sigma = 1)
        
??normalcdf
        
normalcdf(0.88, 1, mu=0.82, sigma=0.019)



#
# Assignment #6
# Julia Suzuki
#

## COVID-19 Daily Positive Rate vs. S&P 500 Daily High Price

r <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Assignment 6\\stockvscases.csv", header=TRUE)

head(r)

summary(r$positivity.rate)
summary(r$high)

##

#plotting the data:
plot(x=r$"positivity.rate", y=r$high,
     main="S&P 500 Daily High Stock Price vs. COVID-19 Daily Positivity Rate with the Least-Squares Line",
     xlab="COVID-19 Daily Positivity Rate (%)", ylab="S&P 500 Daily High Stock Price ($)",
     col="black",
     xlim=c(0, 25),
     ylim=c(2300, 3300))

# this is to plot regression line on the plot
reg_model<-lm(r$high~r$positivity.rate, data=r)
abline(reg_model, col="blue")

# this is the smoother function (just used one for description)
lines(lowess(cbind(r$positivity.rate, r$high)), col="red")

# Analyze the summary of the liner regression model
summary(reg_model)

# Analyze residuals
plot(reg_model)

plot(fitted(reg_model),resid(reg_model))

qqnorm(resid(reg_model))

# Correlation Coefficient
cor(r$positivity.rate, r$high)

# Coefficient of Determination
summary(reg_model)$r.squared





## COVID-19 Daily Positive Rate vs. S&P 500 Daily High Price

r2 <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Assignment 6\\stockvscases.csv", header=TRUE)

head(r2)

# r2<-subset(r2, r2$cum_cases > 5000)

x<-r2$cum_cases/1000
y<-r2$high

# head(r2)

summary(x)
summary(y)

##

#plotting the data:
plot(x=x, y=y,
     main="S&P 500 Daily High Stock Price vs. COVID-19 Cummulative Positive Cases with the Least-Squares Line",
     xlab="COVID-19 Cummulative Positive Cases (in Thousand)", ylab="S&P 500 Daily High Stock Price ($)",
     col="black",
     xlim=c(0, 4000),
     ylim=c(2300, 3300))

# this is to plot regression line on the plot
reg_model<-lm(y~x, data=r)
abline(reg_model, col="blue")

# this is the smoother function (just used one for description)
lines(lowess(cbind(x, y)), col="red")

# Analyze the summary of the liner regression model
summary(reg_model)

# Analyze residuals
plot(reg_model)

plot(fitted(reg_model),resid(reg_model))
qqnorm(resid(reg_model_logmodel))

# Correlation Coefficient
cor(x, y)

# Coefficient of Determination
summary(reg_model)$r.squared





## People with Depressed Feeling vs. COVID-19 Positive Case

rr <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\Association_byAgeAnxiety_csv.csv", header=TRUE)

head(rr)

x<-rr$Positive.Case/1000000
y<-rr$X18...29/1000000

summary(x)
summary(y)

##

#plotting the data:
plot(x=x, y=y,
     main="No. of People Feeling Anxiety vs. COVID-19 Positive Case with the Least-Squares Line",
     xlab="COVID-19 Positive Case (in Million)", ylab="No. of People Feeling Anxiety (in Millon)",
     col="black",
     xlim=c(0, 7),
     ylim=c(0, 12))

# this is to plot regression line on the plot
reg_model<-lm(y~x, data=rr)
abline(reg_model, col="blue")

# this is the smoother function (just used one for description)
lines(lowess(cbind(x, y)), col="red")

# Analyze the summary of the liner regression model
summary(reg_model)

# Analyze residuals
plot(reg_model)

plot(fitted(reg_model),resid(reg_model))
qqnorm(resid(reg_model_logmodel))

# Correlation Coefficient
cor(rr$Positive.Case, rr$X18...29)

# Coefficient of Determination
summary(reg_model)$r.squared





#
# Assignment #8
# Julia Suzuki
#

r <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\Association_CaseDeathDepression_csv.csv", header=TRUE)

# Age Group A: Relationship between COVID-19 Death Rate and Depression 

# x1<-r$AvgPositivityRate.XY
# x2<-r$AvgDeathRate.XY
str(r)

y1<-r$X18...29/1000000
y2<-r$X30...39/1000000
y3<-r$X40...49/1000000
y4<-r$X50...59/1000000
y5<-r$X60...69/1000000
y6<-r$X70...79/1000000
y7<-r$X..80/1000000

summary(x2)
summary(y1)
summary(y2)
summary(y3)
summary(y4)
summary(y5)
summary(y6)
summary(y7)

##
## 1. Age Group 18-29

plot(x=x2, y=y1,
     main="Depression among Age 18-29",
     xlab="COVID-19 Death Rate (Percent)", ylab="No. of Depressed People (in Million)",
     col="black",
     xlim=c(4, 7),
     ylim=c(0, 10))

# this is to plot regression line on the plot
reg_model_3<-lm(y1~x2, data=r)
abline(reg_model_3, col="red")

# Analyze the summary of the liner regression model
summary(reg_model_3)

# Analyze residuals
plot(reg_model_3)

##
## 2. Age Group 30-39

plot(x=x2, y=y2,
     main="Depression among Age 30-39",
     xlab="COVID-19 Death Rate (Percent)", ylab="No. of Depressed People (in Million)",
     col="black",
     xlim=c(4, 7),
     ylim=c(0, 10))

# this is to plot regression line on the plot
reg_model_4<-lm(y2~x2, data=r)
abline(reg_model_4, col="red")

# Analyze the summary of the liner regression model
summary(reg_model_4)

# Analyze residuals
plot(reg_model_4)

##
## 3. Age Group 40-49

plot(x=x2, y=y3,
     main="Depression among Age 40-49",
     xlab="COVID-19 Death Rate (Percent)", ylab="No. of Depressed People (in Million)",
     col="black",
     xlim=c(4, 7),
     ylim=c(0, 10))

# this is to plot regression line on the plot
reg_model_5<-lm(y3~x2, data=r)
abline(reg_model_5, col="red")

# Analyze the summary of the liner regression model
summary(reg_model_5)

# Analyze residuals
plot(reg_model_5)

##
## 4. Age Group 50-59

plot(x=x2, y=y4,
     main="Depression among Age 50-59",
     xlab="COVID-19 Death Rate (Percent)", ylab="No. of Depressed People (in Million)",
     col="black",
     xlim=c(4, 7),
     ylim=c(0, 10))

# this is to plot regression line on the plot
reg_model_6<-lm(y4~x2, data=r)
abline(reg_model_6, col="red")

# Analyze the summary of the liner regression model
summary(reg_model_6)

# Analyze residuals
plot(reg_model_6)

##
## 5. Age Group 60-69

plot(x=x2, y=y5,
     main="Depression among Age 60-69",
     xlab="COVID-19 Death Rate (Percent)", ylab="No. of Depressed People (in Million)",
     col="black",
     xlim=c(4, 7),
     ylim=c(0, 10))

# this is to plot regression line on the plot
reg_model_7<-lm(y5~x2, data=r)
abline(reg_model_7, col="red")

# Analyze the summary of the liner regression model
summary(reg_model_7)

# Analyze residuals
plot(reg_model_7)

##
## 5. Age Group 70-79

plot(x=x2, y=y6,
     main="Depression among Age 70-79",
     xlab="COVID-19 Death Rate (Percent)", ylab="No. of Depressed People (in Million)",
     col="black",
     xlim=c(4, 7),
     ylim=c(0, 10))

# this is to plot regression line on the plot
reg_model_8<-lm(y6~x2, data=r)
abline(reg_model_8, col="red")

# Analyze the summary of the liner regression model
summary(reg_model_8)

# Analyze residuals
plot(reg_model_8)

##
## 6. Age Group Over 80

plot(x=x2, y=y7,
     main="Depression among Age over 80",
     xlab="COVID-19 Death Rate (Percent)", ylab="No. of Depressed People (in Million)",
     col="black",
     xlim=c(4, 7),
     ylim=c(0, 10))

# this is to plot regression line on the plot
reg_model_9<-lm(y7~x2, data=r)
abline(reg_model_9, col="red")

# Analyze the summary of the liner regression model
summary(reg_model_9)

# Analyze residuals
plot(reg_model_9)

##

### Multiple Linear Regression
depressed.reg_model=lm(y ~ r$AvgDeathRate.XY + r$AvgPositivityRate.XY, data=r)
summary(depressed.reg_model)

# Select only 4 variables of interest

r <- na.omit(r) 

# attach(r)
r.df=data.frame(r$AvgPositivityRate.XY, r$AvgDeathRate.XY, r$Total_DepressedPeople)
r.df=data.frame(r$AvgPositivityRate.XY, r$AvgDeathRate.XY, r$Total_DepressedPeople, 
                r$X18...29, r$X30...39, r$X40...49,
                r$X50...59, r$X60...69, r$X70...79, r$X..80)


plot(r.df)

round(cor(r.df),2)

plot(depressed.reg_model)





##
## Analysis of Two Way Tables 
##

depressed.feeling <- matrix(c(
        8438163,	9099121,	4729346,	5803991,
        14340688,	11706873,	4254864,	6211562,
        15565051,	9766326,	3586446,	4262690,
        17740226,	10224368,	3314858,	3845219,
        20844123,	10520779,	2860752,	2603599,
        12407324,	5791596,	1532057,	983803,
        3159472,	1073957,	170478,	        255250
)
,ncol=4,byrow=TRUE)

summary(depressed.feeling)

colnames(depressed.feeling) <- c("Not At All","Several Days","More Than Half The Days", "Nearly Every Day")
rownames(depressed.feeling) <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80 And Above")
depressed.feeling <- as.table(depressed.feeling/1000000)
depressed.feeling

barplot(depressed.feeling,legend=T,beside=T,main="Bar Plot | People by Depressed Level & Age Group",
        xlab="Level of Depression",
        ylab="Count (in Million)")

plot(depressed.feeling, main="Mosaic Plot: People by Depressed Level & Age Group",
     xlab="Age group",
     ylab="Level of Depression")

mosaicplot(depressed.feeling, main="Mosaic Plot: People by Depressed Level & Age Group",
           sort=c(1,2),
           xlab="Age Group",
           ylab="Level of Depression")

mosaicplot(depressed.feeling,main="Mosaic Plot: People by Depressed Level & Age Group",
           sort=c(2,1),
           xlab="Age Group",
           ylab="Level of Depression")

##

# Marginal Distribution - Count
margin.table(depressed.feeling) # Total no. of observations
margin.table(depressed.feeling, 1) # Marginal total for Age Level (row)
margin.table(depressed.feeling, 2) # Marginal total for total for Depressed Feeling Level (column)

# Marginal Distribution - Proportion

# Method 1: Marginal Distribution - Proportion
depressed.feeling/margin.table(depressed.feeling)
depressed.feeling/margin.table(depressed.feeling)*100
margin.table(depressed.feeling, 1)/margin.table(depressed.feeling)
margin.table(depressed.feeling, 2)/margin.table(depressed.feeling)

# Method 2: Marginal Distribution - Proportion
prop.table(depressed.feeling)
prop.table(depressed.feeling)*100
round(prop.table(depressed.feeling)*100, 2)
pt <- round(prop.table(depressed.feeling)*100, 2)

marginal.proportion <- pt

barplot(marginal.proportion,legend=T,beside=T,main="Bar Plot | People by Depressed Level & Age Group",
        xlab="Level of Depression",
        ylab="Percent")

plot(marginal.proportion, main="Mosaic Plot: People by Depressed Level & Age Group",
     xlab="Age Group",
     ylab="Level of Depression")

mosaicplot(marginal.proportion, main="Mosaic Plot: People by Depressed Level & Age Group",
           sort=c(1,2),
           xlab="Age Group",
           ylab="Level of Depression")

mosaicplot(marginal.proportion, main="Mosaic Plot: People by Depressed Level & Age Group",
           sort=c(2,1),
           xlab="Age Group",
           ylab="Level of Depression")

# Conditional Distribution - Proportion
prop.table(depressed.feeling, margin=1) # Conditional probability for depressed feeling level
prop.table(depressed.feeling, margin=2) # Conditional probability for age group level

# 

prop.table(depressed.feeling, margin=1) * 100
round(prop.table(depressed.feeling, margin=1) * 100, 2)
cp <- round(prop.table(depressed.feeling, margin=1) * 100, 2)

conditional.proportion.1 <- cp

barplot(conditional.proportion.1,legend=T,beside=T,main="Bar Plot | Depressed Level by Age Group",
        xlab="Level of Depression",
        ylab="Percent of People")

plot(conditional.proportion.1, main="Mosaic Plot: Proportion of Depressed People by Age Group",
     xlab="Age Group",
     ylab="Level of Depression")

#

prop.table(depressed.feeling, margin=2) * 100
round(prop.table(depressed.feeling, margin=2) * 100, 2)
cp2 <- round(prop.table(depressed.feeling, margin=2) * 100, 2)

conditional.proportion.2 <- cp2 

barplot(conditional.proportion.2,legend=T,beside=T,main="Bar Plot | Proportion by Depressed Feelings and Age Group",
        xlab="Level of Depression",
        ylab="Percent")

plot(conditional.proportion.2, main="Mosaic Plot: Proportion of Age Group by Depressed Level",
     xlab="Age",
     ylab="Level of Depression")

##

## ASK THE PROF.

# METHOD 1: Chi squared test 
summary(depressed.feeling) # Ask the prof. Why it says "Chi-squared approximation may be incorrect" 

# METHOD 2: Chi squared test
chisq.test(depressed.feeling)

## To replicate the formula:
expected <- as.array(margin.table(depressed.feeling,1)) %*% t(as.array(margin.table(depressed.feeling,2))) / margin.table(depressed.feeling)
expected
chi <- sum((expected - as.array(depressed.feeling))^2/expected)
chi
1-pchisq(chi,df=18)





#
# Final Project
# Julia Suzuki
#

#
# 1. Overall relationship between COVID-19 Positive Cases and Depression
#

rm(list = ls())

r <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\Association_CaseDeathDepression_All_csv.csv", header=TRUE)

# r <- na.omit(r) 
# View (r)

head(r)
str(r)

# Depressed - Allf
x1<-r$AvgPositivityRatePeriod
x2<-r$AvgDeathRatePeriod
y<-r$Total*100

summary(x1)
summary(y)

plot(x=x1, y=y,
     main="COVID-19 Positivity Rate and Overall Depressed Proportion",
     xlab="COVID-19 Positivity Rate (Percent)", ylab="Depressed Proportion (Percent)",
     col="black",
     xlim=c(4, 12),
     ylim=c(0, 10))
text(x1, y)

r <- r[c(-1, -5, -13, -14, -15), ]

plot(x=x1, y=y,
     main="COVID-19 Positivity Rate and Overall Depressed Proportion",
     xlab="COVID-19 Positivity Rate (Percent)", ylab="Depressed Proportion (Percent)",
     col="black",
     xlim=c(4, 12),
     ylim=c(6, 10))

# this is to plot regression line on the plot
overall.depressed.positivity.reg_model<-lm(y~x1, data=r)
abline(overall.depressed.positivity.reg_model, col="blue")

# this is the smoother function (just used one for description)
# lines(lowess(cbind(x1, y)), col="red")

# Analyze the summary of the liner regression model
summary(overall.depressed.positivity.reg_model)

# Analyze residuals
plot(overall.depressed.positivity.reg_model)

# correlation test
cor(y, x1, method = c("pearson"))
cor.test(y, x1, method=c("pearson"))



#
# 2. Overall relationship between COVID-19 Deaths and Depression
#

rm(list = ls())

r <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\Association_CaseDeathDepression_All_csv.csv", header=TRUE)

# r <- na.omit(r) 
# View (r)

head(r)
str(r)

# Depressed - All
x1<-r$AvgPositivityRatePeriod
x2<-r$AvgDeathRatePeriod
y<-r$Total*100

summary(x2)
summary(y)

plot(x=x2, y=y,
     main="COVID-19 Death Rate and Overall Depressed Proportion",
     xlab="COVID-19 Death Rate (Percent)", ylab="Depressed Proportion (Percent)",
     col="black",
     xlim=c(0, 7),
     ylim=c(0, 10))
text(x2, y)

r <- r[c(-1, -5, -13, -14, -15), ]

plot(x=x2, y=y,
     main="COVID-19 Death Rate and Overall Depressed Proportion",
     xlab="COVID-19 Death Rate (Percent)", ylab="Depressed Proportion (Percent)",
     col="black",
     xlim=c(0, 7),
     ylim=c(7, 10))

# this is to plot regression line on the plot
overall.depressed.death.reg_model<-lm(y~x2, data=r)
abline(overall.depressed.death.reg_model, col="blue")

# this is the smoother function (just used one for description)
# lines(lowess(cbind(x2, y)), col="red")

# Analyze the summary of the liner regression model
summary(overall.depressed.death.reg_model)

# Analyze residuals
plot(overall.depressed.death.reg_model)

cor(y, x2, method = c("pearson"))
cor.test(y, x2, method=c("pearson"))



#
# 3. Create a multivariate model with COVID-19 Positivity Rate, Death Rate and
# Depressed Proportion

# x1<-r$AvgPositivityRatePeriod
# x2<-r$AvgDeathRatePeriod
# y<-r$Total*100

overall.depressed.positivity.death.reg_model<-lm(y~x1+x2, data=r)
abline(overall.depressed.positivity.death.reg_model, col="blue")
summary(overall.depressed.positivity.death.reg_model)

plot(overall.depressed.positivity.death.reg_model)



#
# 3. Improve the model via transformation

# Transformation of x variables

library(car)
crPlots(overall.depressed.positivity.death.reg_model)

poly.overall.depressed.positivity.death.reg_model=lm(y~poly(x1,4)+poly(x2,4))
summary(poly.overall.depressed.positivity.death.reg_model)

crPlots(poly.overall.depressed.positivity.death.reg_model)

# Check outliers
lrplot= function(lm.object,cex.lab=0.8)
{
        # draws a leverage-residual plot
        # modified 8/8/06
        
        e = residuals(lm.object)
        S = summary(lm.object)$sigma
        df = lm.object$df.residual
        h = hatvalues(lm.object)
        r = e/(S*sqrt(1-h))
        tt = r*sqrt((df-1)/(df-r^2))  # externally studentised residuals
        
        plot(abs(tt), h, xlab= "Absolute value of externally studentised residuals",
             ylab = "influence",type="n")
        text(abs(tt), h, 1:length(h), cex=cex.lab)
        abline(h=3*length(coef(lm.object))/length(residuals(lm.object)),lty=2)
        abline(v=qt(0.975,df), lty=2)
        title("Leverage-residual plot")
        invisible()
}

lrplot(poly.overall.depressed.positivity.death.reg_model)

#

# Transformation of y variable

# Checked the Equality of Variance to see if transformation of y variable is needed


#Checking equality of Variance

funnel=function(reg.object)
{
        #function created by Alan Lee
        # diagnostic plots for detecting non-constant variance
        # plots log sds vs log means
        # and then squared residuals versus fitted values
        # returns a smoothed version of the plot as an
        # estimate of the variance function
        layout(1,2)
        pred<-fitted(reg.object)
        res<-residuals(reg.object)
        cut.points<-quantile(pred,c(0.,.2,.4,.6,.8,1.0)) + c(-0.01,0,0,0,0,1.01)
        group<-cut(pred,cut.points)
        log.means<-log(tapply(pred+res,group,mean))
        log.sds<-log(sqrt(tapply(pred+res,group,var)))
        plot(log.means,log.sds,xlab="Log means",ylab="Log std. errors")
        res.sq<-res^2
        plot(pred,res.sq,ylab="Squared residuals",xlab="Fitted values")
        low.stuff<-loess(res.sq~pred,span=1)
        lines(sort(pred),fitted(low.stuff)[order(pred)],lty=2)
        cat("Slope:",lm(log.sds~log.means)$coef[2],"\n")
        layout(1,1)
        invisible(fitted(low.stuff))
}

funnel(overall.depressed.positivity.death.reg_model)
funnel(poly.overall.depressed.positivity.death.reg_model)


#
boxcoxplot = function (formula,data,p = seq(-2, 3, length = 30), ...)
{
        # Draws a box-Cox plot for transforming to normality
        
        l <- length(p)
        boxcox <- seq(l)
        reg.lm<-lm(formula,data,x=T,y=T)
        y<-reg.lm$y
        X<-reg.lm$x
        n <- length(y)
        sumlog <- sum(log(y))
        for (i in seq(l))
        {
                y.p<- (y^p[i] -1)/p[i]
                trans.res<-residuals(lm(y.p~-1+X))
                ResSS.p<-sum(trans.res^2)
                boxcox[i] <- n * log(ResSS.p)/2 - (p[i] - 1) * sumlog
        }
        plot(p, boxcox, type = "l", ylab = "Profile likelihood",
             main = "Box-Cox plot", ...)
}

# boxcoxplot(y~x1, data=r)
# boxcoxplot(y~x2, data=r)

?boxcoxplot

boxcoxplot(y~x1+x2, data=r, p=seq(3, 8, length=30))
boxcoxplot(overall.depressed.positivity.death.reg_model, data=r, p=seq(3, 8, length=30))

# boxcoxplot(y~poly(x1,4)+poly(x2,4),data=r)

transf.reg=lm((y^6-1)/6~x1+x2)
transf.poly.reg=lm((y^6-1)/2~poly(x1,4)+poly(x2,4))

summary(transf.reg)
summary(transf.poly.reg)

##This seems to have improved the fit. What about the residuals?
plot(transf.reg)
crPlots(transf.reg)

plot(transf.poly.reg)
crPlots(transf.poly.reg)





##

#
# 1. Overall relationship between COVID-19 Positivity Rate and Stock Returns
#

r <- read.csv("C:\\Users\\julia\\Desktop\\BIA652\\Data Sets\\daily_covid_stockreturn_csv.csv", header=TRUE)
nrow(r)
head(r)

#Select only the values we want to analyze

head(r)
names(r)

r.selected <- r[, c(14, 10, 11)]
plot(r.selected) 

cor(r.selected$daily.return, r.selected$daily.positivity.rate, method = c("pearson"))
cor.test(r.selected$daily.return, r.selected$daily.positivity.rate, method=c("pearson"))

cor(r.selected$daily.return, r.selected$daily.death.rate, method = c("pearson"))
cor.test(r.selected$daily.return, r.selected$daily.death.rate, method=c("pearson"))

## Delete rows with missing value
r <- na.omit(r) 

x1<-r$daily.positivity.rate*100
x2<-r$daily.death.rate*100
y<-r$daily.return*100

str(x)
str(y)

summary(x1)
summary(y)

# 

plot(x=x1, y=y,
     main="Daily COVID-19 Positivity Rate and Daily Stock Return",
     xlab="Daily COVID-19 Positivity Rate (Percent)", ylab="Daily Stock Return (Percent)",
     col="black",
     xlim=c(0, 25),
     ylim=c(-7, 6))

model1.reg <- lm(y~x1, data=r)
abline(model1.reg, col='blue') #add regression line
summary(model1.reg)

#
# .2 Overall relationship between COVID-19 Death Rate and Stock Returns
#


x1<-r$daily.positivity.rate*100
x2<-r$daily.death.rate*100
y<-r$daily.return*100

str(x)
str(y)

summary(x2)
summary(y)

plot(x=x2, y=y,
     main="Daily COVID-19 Death Rate and Daily Stock Return",
     xlab="Daily COVID-19 Death Rate (Percent)", ylab="Daily Stock Return (Percent)",
     col="black",
     xlim=c(0, 12),
     ylim=c(-7, 6))

model2.reg <- lm(y~x2, data=r)
abline(model2.reg, col='blue') #add regression line
summary(model2.reg)





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








