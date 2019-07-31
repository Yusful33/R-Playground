ccdata <- read.csv('Consumer_Complaints.csv')
head(ccdata)
View(ccdata)
str(ccdata)
summary(ccdata)
any(is.na(ccdata))
row.has.na <- apply(ccdata, 1, function(x)
{any(is.na(x))
})
#The below remove N/A values and replace it with a blank
ccdata$Consumer.disputed. <- 
  sapply(ccdata$Consumer.disputed., 
         as.character)
ccdata$Consumer.disputed.[is.na(ccdata$Consumer.disputed.)] <- " "
head(ccdata)
nrow(ccdata)
#Looking at the distinct Products contained
library(dplyr)
distinct_Products = ccdata %>% distinct(Product)
print(distinct_Products)
#Creating a new column
colnames(ccdata)
View(ccdata)
ccdata$year <- substring(ccdata$Date.received,6,10)
head(ccdata)
#Looking to create an aggregate column for year and Product
colnames(ccdata)
head(ccdata)
sum.if1 <- NULL
sum.if1 <- as.data.frame(sum.if1)
sum.if1 <- aggregate(data$Products, by = 
                       list(Category = 
                              ccdata$Products, ccdata$Year), FUN = count())
summary(ccdata)
#Repacing na values with blank
ccdata$year[is.na(ccdata$year)] <- " "
ccdata$year <- as.numeric(ccdata$year)
summary(ccdata)
#Fixing the date column
#Applying function
ccdata$year <- sapply(ccdata$year, fix)
summary(ccdata)
View(ccdata)
colnames(ccdata)
#Pulling only the necessary columns 
ccdataClean <- ccdata %>% select(Product, Sub.product, Issue, Sub.issue, Company.public.response,
                                 Company, State, ZIP.code, Consumer.consent.provided., Submitted.via,
                                 Timely.response., year, Date.received)
colnames(ccdataClean)
head(ccdataClean)
View(ccdataClean)
#Updating Timely.Response Column
ccdataClean$Timely.response. <- ifelse(
  ccdataClean$Timely.response. == "Yes", 1,0)
#Updating the Consumer Consent column
ccdataClean$Consumer.consent.provided. <- ifelse(
  ccdataClean$Consumer.consent.provided. == "Consent not provided", 0,1)
head(ccdataClean)
#Changing zipcode to numeric
ccdataClean$ZIP.code <- as.numeric(ccdataClean$ZIP.code)
#Trying as factor for Submitted.via
ccdataClean$Submitted.via<- as.factor(ccdataClean$Submitted.via)
str(ccdataClean)
colnames(ccdataClean)
#Removing NA values
nrow(ccdataClean)
#Removing rows with 0 value
ccdataClean[is.na(ccdataClean)] <- 0
ccdataClean <- subset(ccdataClean, ccdataClean$year != 0)
nrow(ccdataClean)
colnames(ccdataClean)
str(ccdataClean)
#Multiple Regression to distinguish timely reponse
ccdataModel <- lm(Timely.response.~. - ZIP.code - Sub.issue - Date.received
                  -Company -Sub.product, data=ccdataClean)
summary(ccdataModel)
View(ccdataClean$Timely.response.)
#Getting the model into a dataframe
library(broom)
ccdataModel.df <- tidy(ccdataModel)
View(ccdataModel.df)
#Only keeping the pvalue column
ccdataModel.df <- ccdataModel.df[c('term','p.value')]
colnames(ccdataModel.df)
#Finding the avg pvalue
max(ccdataModel.df$p.value)
mean(ccdataModel.df$p.value)
avgVal <- mean(ccdataModel.df$p.value)
#Data for Method of Submission and Timely Response
ccdataModel.submit <- subset(ccdataModel.df, ccdataModel.df$term == "Submitted.viaFax"
        | ccdataModel.df$term == "Submitted.viaPhone" | ccdataModel.df$term == "Submitted.viaPostal mail"
        | ccdataModel.df$term == "Submitted.viaReferral" | ccdataModel.df$term == "Submitted.viaWeb")
View(ccdataModel.submit)
#Subsetting Variables that do affect Timely Response
ccdataModel.Important <- subset(ccdataModel.df, ccdataModel.df$p.value < '0.05')
View(ccdataModel.Important)
#Have a total of 90 variables with a P.Value < 0.05
ccdataModel.df <- subset(ccdataModel.df, ccdataModel.df$p.value < avgVal)
nrow(ccdataModel.df)
#Which variable has the greatest impact 
write.csv(ccdataModel.df, file = "GreatestImpact.csv")
#Fixing Year
ccdata$year <- ifelse(
  ccdata$year == "14", "2014", ccdata$year)
ccdata$year <- ifelse(
  ccdata$year == "15", "2015", ccdata$year)
ccdata$year <- ifelse(
  ccdata$year == "16", "2016", ccdata$year)
ccdata$year <- ifelse(
  ccdata$year == "17", "2017", ccdata$year)
ccdata$year <- ifelse(
  ccdata$year == "18", "2018", ccdata$year)
ccdata$year <- ifelse(
  ccdata$year == "19", "2019", ccdata$year)
colnames(ccdata)


#Graphs
gdata <- ccdata
gdata <- na.omit(gdata)
colnames(gdata)
library(ggplot2)
#Outputting the Data
write.csv(gdata, file = "ConsumerCompliantsUpdated.csv")
str(ccdata)

#Creating the first box plot
print(box)
agg <- aggregate(Product~year,FUN=length,data=gdata)
agg$Complaints <- aggregate(Issue~year, FUN=length, data=gdata)
agg <- as.data.frame(agg)
View(agg)
colnames(agg)
library(plotly)
box <- ggplot(agg, aes(x=year, y=Product)) + geom_boxplot(stat = "boxplot")
box <- ggplotly(box)
print(box)

#Hypothesis Testing
head(gdata)
hdata <- gdata
hdata$Timely.response. <- ifelse(
  hdata$Timely.response. == "Yes", 1,0)
head(hdata)
#H0: Timely.Response of Referral complinats is not greater than phone complaints 
#H1: If the Timely.Response of Referral compliants is greater than phone complaints 

hdata.Referral <- subset(hdata, Submitted.via == 'Referral')
hdata.Phone <- subset(hdata, Submitted.via == 'Phone')
View(hdata.Referral)
hypo <- t.test(hdata.Referral$Timely.response., hdata.Phone$Timely.response., 
                alternative="greater", var.equal=T)
print(hypo)
#We can reject the null hypothesis and support the claim because 
#the P-value (< 2.2e-16) is much lower than the significance level p-value
#of 0.05. So our decision is that the null Hypothesis
#is false and Timely.response. for referrals is greater than the 
#Timely.response. for a phone complaint