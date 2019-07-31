###------------------
###Regression and Clustering
###------------------

###Students Name: Yusuf Cattaneo
###GNumber: G00983582
rm(list=ls())
install.packages(ggplot2)

library(ggplot2)
library(plotly)
data <- read.csv('EmployeeAttrition.csv')
#Regression
#a. Show the scatter plot with relationship curve between TotalWorkingYears and MonthlyIncome.
#Briefly explain your observation in the plot (Hint: Use scatter.smooth() function in R) (5 points)
grapha <- ggplot(data,aes(x=TotalWorkingYears, 
          y=MonthlyIncome)) + geom_point(size = 0.7) + geom_smooth(method='lm')
grapha <- ggplotly(grapha)
print(grapha)

#Explanation
#Based on the graph, it looks to me that TotalWorkingYears
#and Monthly Income are positively correlated 

#b. Show the scatter plot with relationship curve between Age and DistanceFromHome. Briefly
#explain your observation in the plot (Hint: Use scatter.smooth() function in R) (5 points)
graphb <- ggplot(data, aes(Age, DistanceFromHome)) + geom_point(size = 0.7) + geom_smooth()
graphb <- ggplotly(graphb)
print(graphb)

#Explanation
#Since the geom_smooth line is perfectly horizontal, I would say that these two variables are not correlated at all

#c. Calculate Correlation for (a) and (b) and explain the values to support your answer in (a) and (b)
#(5 points)
print(cor(data$TotalWorkingYears, data$MonthlyIncome))
print(cor(data$Age, data$DistanceFromHome))
#Explanation
#Since the correlation for a is (~0.77) I would say this confirms my 
#original hypthosesis that these two variables are strongly correlated
#Additionally since the correlation for b is (~-0.001) I would say this also
#confirms my original suspicous and these two varaibles are not at all correlated

#d. Using Linear Regression, find details of the relationship between TotalWorkingYears and
#MonthlyIncome. Explain results in terms of p-value at 95% confidence interval and determine 
#whether the relationship is significant or not (Hint: Use lm() to create linear regression model.
#Use print() to show coefficients. Use summary() to show more details) (15 points)
datamodeld <- lm(TotalWorkingYears~ MonthlyIncome, data=data)
summary(datamodeld)
#Explanation
#Based on a CI of 95% I would reject the null hypothesis (that these two variables are not correlated)
#becuase the p-value when running the summary fuction is extremely low at < 2.2e-16.
#As we all know anything less than a p-value of 0.05 would have been sufficient to reject 
#the null hypothesis at a CI of 95%

#Clustering
#a. Use Kmeans Clustering algorithm to find groups between HourlyRate and TotalWorkingYears.
#Use number of clusters as 3. Explain how each group is different from another in terms of
#employees representing those groups.
data2 <- data[c('TotalWorkingYears', 'HourlyRate')]
data2 <- na.omit(data2)
fit <- kmeans(data2,3)
aggregate(data2,by=list(fit$cluster),FUN=mean)
data_clustered <- data.frame(data2, cluster=factor(fit$cluster))
print(ggplot(data_clustered, aes(x=TotalWorkingYears, 
   y=HourlyRate, color=cluster)) + geom_point())
#Explanation
#In looking at graphed clusters its looks like the major designation point
#between each cluster seems to be HourlyRate and not TotalWorkingYears. 
#The three clusters seem to be segmented into three groups of 
#Hourly rate < 55, 78-55 and then > 78 per hour.

#b. Use number of clusters as 5. What did you observe? Did you see any split of groups observed in
#(a)? Observe the splitting groups and explain in terms of employees representing those groups.
#(Hint: Use kmeans() for clustering algorithm. Install ggplot2 library in R and use ggplot()
#function to visualize the clustering results)
fit2 <- kmeans(data2,5)
aggregate(data2,by=list(fit2$cluster),FUN=mean)
library(cluster)
data2_clustered <- data.frame(data2, cluster=factor(fit2$cluster))
print(ggplot(data2_clustered, aes(x=TotalWorkingYears, 
        y=HourlyRate, color=cluster)) + geom_point())
#Explanation 
#In looking at the graph the clusters seem to be divided up much more by 
#hourly rate for clusters 2-5 with only cluster 1 having individuals 
#spread out between the 50 & below rate, 65-50 and 80-65 rate. Which it is 
#important to note are different than the cutoff points when we only had three clusters