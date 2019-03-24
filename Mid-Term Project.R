help(read.csv)
library(readr)
library(dplyr)
LargestIncrease <- max(alldata$changeperday)
View(LargestIncrease)
#Will be looking at the stocks prices of the below ten companies
AAPL <- read_csv("AAPL.csv")
IBM <- read_csv("IBM.csv")
VZ <- read_csv("VZ.csv")
MSFT <- read_csv("MSFT.csv")
CSCO <- read_csv("CSCO.csv")
DIS <- read_csv("DIS.csv")
JPMorgan <- read_csv("JPM.csv")
Coke <- read_csv("KO.csv")
McD <- read_csv("MCD.csv")
Nike <- read_csv("NKE.csv")
library(reshape2)
alldata.na <- rbind(AAPL, IBM, VZ, MSFT, CSCO, DIS,
                              JPMorgan, Coke, McD, Nike)
alldata <- na.omit(alldata.na)
View(alldata)
#Check for n/a values
any(is.na(alldata))
#Replace n/a values with 0
alldata[is.na(alldata)] <- 0
#Replace all n/a values with an avg of the column
alldata$changeperday[is.na(alldata$changeperday)] <- mean(alldata$changeperday)
View(alldata)
#Run the below function when all of the data has been merged
alldata$date <- months(as.Date(alldata$date))
View(alldata)
library(dplyr)
#Basic alldata info
nrow(alldata)
nrow(alldata.na)
str(alldata)
summary(alldata)
colnames(alldata)
#Calculated column
alldata$changeperday <- (alldata$close - alldata$open)
#Could have also used mutate in dplyr
mutate(alldata, changeperday = close - open)
#Rounds Changeperday column to 3 decimal places 
alldata$changeperday <- round(alldata$changeperday, digits = 3)
#View(alldata)
#Graphing Change Per Day column in histogram
library(ggplot2)
histo.prep <- ggplot(alldata, aes(x=changeperday))
origin.graph <- histo.prep + geom_histogram(binwidth = 0.3, 
                                            aes(fill=..count..))
adjustment1 <- origin.graph + ylab('Number of Days')
adjustment2 <- adjustment1 + coord_cartesian(xlim=seq(-7, 7, by=0.3))
print(adjustment2)
#Sorting from biggest change to smallest for all tickers & days
desc.changeperday <- data.frame(order(-alldata['changeperday']))
#View (desc.changeperday)
#Another way to sort
desc.changeperdayalt <- order(-alldata$changeperday)
#View(desc.changeperdayalt)
#Looking at all data
#View(alldata)
#Looking at important columns to this project
View(alldata)
abbreviated.data <- alldata[c('label', 'open', 'close', 'volume', 'changeperday')]
View(abbreviated.data)
#Segmenting out Presidents day info
bPresday14.alldata <- filter(alldata, label == 'Feb 12, 14')
#View(bPresday14.alldata)
aPresday14.alldata <- filter(alldata, label == 'Feb 18, 14')
bPresday15.alldata <- filter(alldata, label == 'Feb 13, 15')
aPresday15.alldata <- filter(alldata, label == 'Feb 17, 15')
bPresday16.alldata <- filter(alldata, label == 'Feb 12, 16')
aPresday16.alldata <- filter(alldata, label == 'Feb 16, 16')
bPresday17.alldata <- filter(alldata, label == 'Feb 17, 17')
aPresday17.alldata <- filter(alldata, label == 'Feb 21, 17')
bPresday18.alldata <- filter(alldata, label == 'Feb 16, 18')
aPresday18.alldata <- filter(alldata, label == 'Feb 20, 18')
# Combining before presidents day
library(reshape2)
b4presday <- rbind(bPresday14.alldata, bPresday15.alldata, bPresday16.alldata,
                   bPresday17.alldata, bPresday18.alldata)
#View(b4presday)
colnames
average.stockchangeb4pres <- mean(b4presday$changeperday)
average.stockchangeb4pres
after.presday <- rbind(aPresday14.alldata, aPresday15.alldata, aPresday16.alldata,
                       aPresday17.alldata, aPresday18.alldata)
average.stockchangeafterpres <- mean(after.presday$changeperday)
average.stockchangeafterpres
#After just testing for Presidents day it increased $0.21 on average before
#and $0.13 after the holiday

#Moving on to MLK Day
library(dplyr)
bMLK14.alldata <- filter(alldata, label == 'Jan 17, 14')
View(bMLK14.alldata)
aMLK14.alldata <- filter(alldata, label == 'Jan 21 14')
View(aMLK14.alldata)
bMLK15.alldata <- filter(alldata, label == 'Jan 16, 15')
View(bMLK15.alldata)
aMLK15.alldata <- filter(alldata, label == 'Jan 20, 15')
bMLK16.alldata <- filter(alldata, label == 'Jan 15, 16')
aMLK16.alldata <- filter(alldata, label == 'Jan 19, 16')
bMLK17.alldata <- filter(alldata, label == 'Jan 13, 17')
aMLK17.alldata <- filter(alldata, label == 'Jan 17, 17')
bMLK18.alldata <- filter(alldata, label == 'Jan 12, 18')
aMLK18.alldata <- filter(alldata, label == 'Jan 16, 18')
b4MLKday <- rbind( bMLK15.alldata, bMLK16.alldata,
                  bMLK17.alldata, bMLK18.alldata)
#View(b4MLKday)
colnames
average.stockchangeb4MLKday <- mean(b4MLKday$changeperday)
average.stockchangeb4MLKday
after.MLKday <- rbind(aMLK14.alldata, aMLK15.alldata, aMLK16.alldata,
                      aMLK17.alldata, aMLK18.alldata)
average.stockchangeafterMLKday <- mean(after.MLKday$changeperday)
average.stockchangeafterMLKday
#Before MLK date stocks rise $0.28 cents but after MLK day they fall -$0.19
summary(alldata)
#As a whole prices increase $0.008 cents per day
Greater.Increase <- data.frame(alldata[alldata$changeperday > 0.008,])
Less.Increase <- data.frame(alldata[alldata$changeperday < 0.008,])
#View(Less.Increase)
#View(Greater.Increase)
#View(alldata)
#Of the 12,580 entries, on 6,089 of the days the price changed less than $0.008 cents and on
#6,491 of the days the price changed more than $0.008 cents
nrow(Greater.Increase)
#Next we are going to calculate memorial day, July 4th, labor day, Thanksgiving,
#Christmas and then finally NYE/Day as (one event)

#Combining before Presidents Day and MLK Day data
b4PresandMLK <- rbind(b4MLKday, b4presday)
#View(b4PresandMLK)

average.stockchangeb4PresandMLK <- mean(b4PresandMLK$changeperday)
average.stockchangeb4PresandMLK
#Before MLK and Presidents day stocks increase $0.24 on average
aPresandMLK <- rbind(after.MLKday, after.presday)
#View(aPresandMLK)
colnames(aPresandMLK)
csvfile.foraMLKandPres <- aPresandMLK[c('date', 'open', 'close', 'changeperday')]
#Writing output into a csv file
write.csv(csvfile.foraMLKandPres, file = 'aPresandMLK.csv')
average.stockchangeaPresandMLK <- mean(aPresandMLK$changeperday)
average.stockchangeaPresandMLK
#After MLK and Presidents day stocks decrease $(0.013) on average


#Memorial Day
bMem14.alldata <- filter(alldata, label == 'May 23, 14')
aMem14.alldata <- filter(alldata, label == 'May 27, 14')
bMem15.alldata <- filter(alldata, label == 'May 22, 15')
aMem15.alldata <- filter(alldata, label == 'May 26, 15')
#The below is the same thing but just a diff way of going about it
bMem16.alldata <- subset(alldata, subset = label == 'May 27, 16')
aMem16.alldata <- subset(alldata, subset = label == 'May 31, 16')
bMem17.alldata <- subset(alldata, subset = label == 'May 26, 17')
aMem17.alldata <- subset(alldata, subset = label == 'May 30, 17')
bMem18.alldata <- subset(alldata, subset = label == 'May 25, 18')
aMem18.alldata <- subset(alldata, subset = label == 'May 29, 18')
#Before Memorial Day
b4Memday <- rbind(bMem14.alldata, bMem15.alldata, bMem16.alldata,
                   bMem17.alldata, bMem18.alldata)
#View(b4Memday)
aMemday <- rbind (aMem14.alldata, aMem15.alldata, aMem16.alldata,
                  aMem17.alldata, aMem18.alldata)
#View(aMemday)
average.stockchangeb4Memday <- mean(b4Memday$changeperday)
average.stockchangeb4Memday
#Before Memorial day prices increase $0.06 on average
average.stockchangeaMemday <- mean(aMemday$changeperday)
average.stockchangeaMemday
#After Memorial day prices decrease $(0.24) on 

#Labor Day
bLabor14.alldata <- subset(alldata, subset = label == 'Aug 29, 14')
View(bLabor14.alldata)
aLabor14.alldata <- subset(alldata, subset = label == 'Sep 2, 14')
bLabor15.alldata <- subset(alldata, subset = label == 'Sep 4, 15')
aLabor15.alldata <- subset(alldata, subset = label == 'Sep 8, 15')
bLabor16.alldata <- subset(alldata, subset = label == 'Sep 2, 16')
aLabor16.alldata <- subset(alldata, subset = label == 'Sep 6, 16')
bLabor17.alldata <- subset(alldata, subset = label == 'Sep 1, 17')
aLabor17.alldata <- subset(alldata, subset = label == 'Sep 5, 17')
bLabor18.alldata <- subset(alldata, subset = label == 'Aug 31, 18')
aLabor18.alldata <- subset(alldata, subset = label == 'Sep 4, 18')
b4LaborDay = rbind(bLabor14.alldata,bLabor15.alldata,bLabor16.alldata,
                   bLabor17.alldata,bLabor18.alldata)
aLaborDay = rbind (aLabor14.alldata, aLabor15.alldata, aLabor16.alldata, 
                   aLabor17.alldata, aLabor18.alldata)
#Before labor day stocks increase $0.03 on average
average.stockchangeb4Laborday <- mean(b4LaborDay$changeperday)
average.stockchangeb4Laborday
#After labor day stocks decrease $0.01 on average
average.stockchangeaLaborDay <- mean(aLaborDay$changeperday)
average.stockchangeaLaborDay


#July 4th (almost thought I was going crazy here b/c no data returned with "July" needed to shorten it to "Jul")
bJuly14.alldata <- filter(alldata, label == 'Jul 3, 14')
View(bJuly14.alldata)
aJuly14.alldata <- filter(alldata, label == 'Jul 6, 14')
#In 2014 it fell on a saturday
bJuly15.alldata <- filter(alldata, label == 'Jul 2, 15')
aJuly15.alldata <- filter(alldata, label == 'Jul 6, 15')
#In 2015 it was observed on Friday but actually fell on a saturday
bJuly16.alldata <- filter(alldata, label == 'Jul 1, 16')
aJuly16.alldata <- filter(alldata, label == 'Jul 5, 16')
#In 2016 it fell on a Monday
bJuly17.alldata <- filter(alldata, label == 'Jul 3, 17')
aJuly17.alldata <- filter(alldata, label == 'Jul 5, 17')
#In 2017 it fell on a Tuesday
bJuly18.alldata <- filter(alldata, label == 'Jul 3, 18')
aJuly18.alldata <- filter(alldata, label == 'Jul 5, 18')
#In 2018 it fell on a Wednesday

bJuly4th.alldata <- rbind(bJuly14.alldata, bJuly15.alldata, bJuly16.alldata,
                          bJuly17.alldata, bJuly18.alldata)

b4July4th.alldata <- na.omit(bJuly4th.alldata)
aJuly4th.alldata <- rbind(aJuly14.alldata, aJuly15.alldata, aJuly16.alldata,
                          aJuly17.alldata, aJuly18.alldata)
#On average stocks increase $0.10 before July 4th holiday
average.stockchangeaJuly4th <- mean(aJuly4th.alldata$changeperday)
average.stockchangeaJuly4th
#On average stocks decrease $0.19 after July 4th Holiday
average.stockchangebJuly4th <- mean(b4July4th.alldata$changeperday)
average.stockchangebJuly4th

#Labor Day
bLabor14.alldata <- filter(alldata, label == 'Aug 29, 14')
View(bJuly14.alldata)
aLabor14.alldata <- filter(alldata, label == 'Sep 2, 14')
bLabor15.alldata <- filter(alldata, label == 'Sep 4, 15')
aLabor15.alldata <- filter(alldata, label == 'Sep 8, 15')
bLabor16.alldata <- filter(alldata, label == 'Sep 2, 16')
aLabor16.alldata <- filter(alldata, label == 'Sep 6, 16')
bLabor17.alldata <- filter(alldata, label == 'Sep 1, 17')
aLabor17.alldata <- filter(alldata, label == 'Sep 5, 17')
bLabor18.alldata <- filter(alldata, label == 'Aug 31, 18')
aLabor18.alldata <- filter(alldata, label == 'Sep 4, 18')

bLabor.alldata <- rbind(bLabor14.alldata, bLabor15.alldata, bLabor16.alldata,
                          bLabor17.alldata, bLabor18.alldata)
aLabor.alldata <- rbind(aLabor14.alldata, aLabor15.alldata, aLabor16.alldata,
                          aLabor17.alldata, aLabor18.alldata)
#On average stocks increase $0.03 before labor day
average.stockchangebLabor <- mean(bLabor.alldata$changeperday)
average.stockchangebLabor
#On average stocks decrease $0.01 after labor day
average.stockchangeaLabor <- mean(aLabor.alldata$changeperday)
average.stockchangeaLabor

#Thanksgiving
bThanks14.alldata <- filter(alldata, label == 'Nov 26, 14')
aThanks14.alldata <- filter(alldata, label == 'Nov 28, 14')
bThanks15.alldata <- filter(alldata, label == 'Nov 25, 15')
aThanks15.alldata <- filter(alldata, label == 'Nov 27, 15')
bThanks16.alldata <- filter(alldata, label == 'Nov 23, 16')
aThanks16.alldata <- filter(alldata, label == 'Nov 25 16')
bThanks17.alldata <- filter(alldata, label == 'Nov 22, 17')
aThanks17.alldata <- filter(alldata, label == 'Nov 24, 17')
bThanks18.alldata <- filter(alldata, label == 'Nov 21, 18')
aThanks18.alldata <- filter(alldata, label == 'Nov 23, 18')

bThanks.alldata <- rbind(bThanks14.alldata, bThanks15.alldata, bThanks16.alldata,
                        bThanks17.alldata, bThanks18.alldata)
aThanks.alldata <- rbind(aThanks14.alldata, aThanks15.alldata, aThanks16.alldata,
                        aThanks17.alldata, aThanks18.alldata)
#On average stocks decrease $(0.05) before Thanksgiving
average.stockchangebThanks <- mean(bThanks.alldata$changeperday)
average.stockchangebThanks
#On average stocks decrease $(0.06) after Thanksgiving
average.stockchangeaThanks <- mean(aThanks.alldata$changeperday)
average.stockchangeaThanks

#Christmas
bChrist14.alldata <- filter(alldata, label == 'Dec 24, 14')
aChrist14.alldata <- filter(alldata, label == 'Dec 26, 14')
#Occured on a Thursday in 2014
bChrist15.alldata <- filter(alldata, label == 'Dec 24, 15')
aChrist15.alldata <- filter(alldata, label == 'Dec 28, 15')
#Occured on a Friday in 2015
bChrist16.alldata <- filter(alldata, label == 'Dec 22, 16')
aChrist16.alldata <- filter(alldata, label == 'Dec 26 16')
#Occured on a Monday in 2016
bChrist17.alldata <- filter(alldata, label == 'Dec 22, 17')
aChrist17.alldata <- filter(alldata, label == 'Dec 26, 17')
#Occured on a Monday in 2017
bChrist18.alldata <- filter(alldata, label == 'Dec 24, 18')
aChrist18.alldata <- filter(alldata, label == 'Dec 26, 18')

bChrist.alldata <- rbind(bChrist14.alldata, bChrist15.alldata, bChrist16.alldata,
                         bChrist17.alldata, bChrist18.alldata)
aChrist.alldata <- rbind(aChrist14.alldata, aChrist15.alldata, aChrist16.alldata,
                         aChrist17.alldata, aChrist18.alldata)
#On average stocks decrease $(0.50) before Christmas
average.stockchangebChrist <- mean(bChrist.alldata$changeperday)
average.stockchangebChrist
#On average stocks increased $(1.07) after Christmas
average.stockchangeaChrist <- mean(aChrist.alldata$changeperday)
average.stockchangeaChrist

#Leaving out NYE/Day because most people don't work that week anyway

#Bringing it all together
b4ImportantData <- rbind(b4MLKday, b4presday, 
                       bChrist.alldata, bThanks.alldata, bLabor.alldata, b4Memday,
                       b4July4th.alldata)
afterImportantData <- rbind(after.MLKday, after.presday, 
                            aChrist.alldata, aThanks.alldata, aLabor.alldata, aMemday,
                            aJuly4th.alldata)
View(b4ImportantData)
abbreviated.b4data <- 
  b4ImportantData[c('date', 'label', 'open', 'close', 'volume', 'changeperday')]
View(abbreviated.b4data)
#The below converts the dates to Holiday months
abbreviated.b4data$date <- ifelse(abbreviated.b4data$date
      %in% c("January"), "MLK Day",
        ifelse(abbreviated.b4data$date
          %in% c("February"), "Presidents Day",
            ifelse(abbreviated.b4data$date
             %in% c("May"), "Memorial Day",
             ifelse(abbreviated.b4data$date
              %in% c("July"), "July 4th",
              ifelse(abbreviated.b4data$date
                %in% c("September", "August"), "Labor Day",
              ifelse(abbreviated.b4data$date
                %in% c("November"), "Thanksgiving",
               ifelse(abbreviated.b4data$date
                 %in% c("December"), "Christmas"," "
                         )))))))
View(abbreviated.b4data)
abbreviated.afterdata <- 
  afterImportantData[c('date', 'label', 'open', 'close', 'volume', 'changeperday')]
View(abbreviated.afterdata)
#The below converts the dates to Holiday months
abbreviated.afterdata$date <- ifelse(abbreviated.afterdata$date
    %in% c("January"), "MLK Day",
    ifelse(abbreviated.afterdata$date
     %in% c("February"), "Presidents Day",
      ifelse(abbreviated.afterdata$date
      %in% c("May"), "Memorial Day",
      ifelse(abbreviated.afterdata$date
      %in% c("July"), "July 4th",
      ifelse(abbreviated.afterdata$date
      %in% c("September", "August"), "Labor Day",
      ifelse(abbreviated.afterdata$date
      %in% c("November"), "Thanksgiving",
      ifelse(abbreviated.afterdata$date
      %in% c("December"), "Christmas"," "
      )))))))
View(abbreviated.afterdata)
summary(abbreviated.afterdata)
#Before a holiday an average of 14,325,843 stocks trade at a decrease $(0.03) on average
avg.abbreviated.b4data <- mean(abbreviated.b4data$changeperday)
avg.abbreviated.b4data
avgTradingBeforeData <- mean(abbreviated.b4data$volume)
avgTradingBeforeData
library(dplyr)
#Renaming date column to holiday
colnames(abbreviated.b4data)[which(names(abbreviated.b4data) 
                                   == "date")] <- "Holiday"
View(abbreviated.b4data)
write.csv(abbreviated.b4data, file = "Before_Holiday_Data.csv")
#After a holiday an average of 15,205,094 stock trade at an increase of $0.09 on average
avg.abbreviated.afterdata <- mean(abbreviated.afterdata$changeperday)
avg.abbreviated.afterdata
avgTradingAfterData <- mean(abbreviated.afterdata$volume)
avgTradingAfterData
colnames(abbreviated.afterdata)[which(names(abbreviated.afterdata) == "date")] <- "Holiday"
write.csv(abbreviated.afterdata, file = "After_Holiday_Data.csv")

#All data average
#On any given day an average of 16,864,693 stocks trade at an increase of $(0.0089) per day on average
#Summary of data below
summary(abbreviated.data)
colnames(abbreviated.data) [which(names(abbreviated.data) == "date")] <- "Holiday"
View(abbreviated.data)
is.na(abbreviated.data)
write.csv(abbreviated.data, file = "all_data.csv")
View(abbreviated.data)
summary(alldata)
colnames(abbreviated.b4data)
colnames(abbreviated.afterdata)
#Graphs in R
library(ggplot2)
boxplot.prep <- ggplot(abbreviated.b4data, 
                       aes(x=open,
                           y=close))
                                               
print(boxplot.prep)
origin.b4graph <- boxplot.prep + geom_bin2d(binwidth
           =c(3,3))
print(origin.b4graph)
adjust.b4graph <- boxplot.prep + facet_grid(.~Holiday)
print(adjust.b4graph)
