library(readr)
dftrain <- read.csv("bikesharetrain.csv")
dftest <- read.csv("bikesharetest.csv")
head(dftrain)
#We are trying to predict the count column which is the numbers of bikes rented each hour
library(ggplot2)
graph1 <- ggplot(dftrain, aes(x=temp, y=count)) + geom_point(alpha=0.5, aes(color=temp)) + theme_bw
print(graph1)
#Converting Date and Time columns
dftrain$datetime <- as.POSIXct(dftrain$datetime)
head(dftrain)
graph2 <- ggplot(dftrain, aes(datetime, count)) + geom_point(alpha=0.5, aes(color=temp))
print(graph2)
#Changing the coloring in graph2
graph3 <- graph2 + scale_color_continuous(low='#55D8CE',high='#FF6E2E')
print(graph3)

#Looking at the correlations between temp and count
cor(dftrain[,c('temp', 'count')])
#Looking at all numbered correlations
num.cols <- sapply(dftrain, is.numeric)
#filter on numeric columns and their correlations
cor.data <- cor(dftrain[,num.cols])
print(cor.data)
library(corrgram)
library(corrplot)
#Below creates a correlation graph with corrplot
print(corrplot(cor.data, method = 'color'))

#Creating a boxplot for count and season
graph4 <- ggplot(dftrain, aes(season, count)) + geom_boxplot(aes(color=factor(season)))
print(graph4)

#Creating a new column that just strips out the hour
dftrain$hour <- sapply(dftrain$datetime, function(x){
  format(x, "%H")
})
head(dftrain)
dftrain <- dftrain[,!grepl("new",names(dftrain))]
head(dftrain)
#Creating a subset of the data that only includes work days
dftrain.work <- subset(dftrain, workingday==1)
head(dftrain.work)
#Creating a new graph that plots out hour and count
graph4.5 <- ggplot(dftrain.work, aes(hour, count)) + geom_point()
print(graph4.5)
graph5 <- ggplot(dftrain.work, aes(hour, count)) 
#Jitter below fills in the space between each hour
graph5.1 <- graph5 + geom_point(aes(color=temp),position=position_jitter(w=1, h=0)) 
graph5.2 <- graph5.1 + scale_color_gradientn(colours=c('dark blue',
            'blue', 'light blue', 'pink', 'red', 'dark red'))
print(graph5.2)
#Creating the same graph above but for non-work days
graph6 <- ggplot(filter(dftrain, workingday==0), aes(hour, count)) 
#Jitter below fills in the space between each hour
graph6.1 <- graph5 + geom_point(aes(color=temp),position=position_jitter(w=1, h=0)) 
graph6.2 <- graph5.1 + scale_color_gradientn(colours=c('dark blue',
                                                       'blue', 'light blue', 'pink', 'red', 'dark red'))
print(graph6.2)
#Creating a model 
library(caTools)
temp.model <- lm(count ~ temp, dftrain)
print(summary(temp.model))

#Trying to predict out count for a 25 degree celcius day
temp.test <- data.frame(temp=c(25))
predict(temp.model, temp.test)
#Our model predicts that we lease our 235 bikes per hour if its 25 degrees out

#Changing the hour column to numeric values
dftrain$hour <- sapply(dftrain$hour, as.numeric)

#Creating a new model with a lot more variables 
#Using . and then minus you are able to start with all columns and then slowly subtract out some
model <- lm(count ~ . -casual - registered - datetime - atemp ,dftrain)
print(summary(model))
#This model does not take into account seasonality (not great for time series data)
