#UCI Salary Logistic Regression Project 
#Regression works well if their is a clean relationship
adult <- read.csv("adult_sal.csv")
head(adult)
#Dropping the index column
library(dplyr)
adult <- select(adult, -X)
head(adult)
str(adult)
summary(adult)
#Trying to reduce the numbers of columns being categorized as factors
help(table)
#Converting blank Employer types to unemployed 
table(adult$type_employer)
levels(adult$type_employer)[levels(adult$type_employer) == "?"] <- "Unemployed"
levels(adult$type_employer)
#Creating a funciton to combine Never-Worked and Without-pay into Unemployed
unemp <- function(job) {
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay') {
    return('Unemployed')
  } else {
    return(job)
  }
}
#Applying function
adult$type_employer <- sapply(adult$type_employer, unemp)
#Function to combine State and Local Gov jobs 
gov <- function(job) {
  job <- as.character(job)
  if (job == 'Local-gov' | job=='State-gov') {
    return('SL-Gov')
  } else if (job == 'Self-emp-inc' | job == 'Self-emp-not-inc'){
    return('Self-Emp')
  } else{
    return(job)
  }
}
#Applying function
adult$type_employer <- sapply(adult$type_employer, gov)
table(adult$type_employer)
table(adult$marital)
#Creating a funciton to slim down marital factors
marriage <- function(status) {
  status <- as.character(status)
  if (status == 'Divorced' | status=='Seperated' | status == 'Widowed') {
    return('Not-Married')
  } else if (status == 'Married-spouse-absent' | status == 'Married-civ-spouse' | status  == 'Married-AF-spouse'){
    return('Not-Married')
  } else{
    return(status)
  }
}
#Applying function
adult$marital <-sapply(adult$marital, marriage)
table(adult$marital)
View(adult)
#Moving on to country
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')
North.America <- c('Canada','United-States','Puerto-Rico','Outlying-US(Guam-USVI-etc)')
Europe <-  c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
             'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')
Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')
#Function for countries
group.country <- function(country){
  if (country %in% Asia){
    return('Asia')
  } else if (country %in% North.America){
    return('North.America')
  } else if (country %in% Europe ){
    return('Europe')
  } else if (country %in% Latin.and.South.America){
    return('Latin.and.South.America')
  } else{
    return('Other')
  }
}
#Updating Country values from ? to unknown
levels(adult$country)[levels(adult$country) == "?"] <- "Unknown"
#Applying function
adult$country <-sapply(adult$country, group.country)
table(adult$country)
#Converting all of the grouping above to factors
adult$type_employer <- as.factor(adult$type_employer)
adult$country <- as.factor(adult$country)
adult$marital <- as.factor(adult$marital)
str(adult)
#Dealing with missing data
library(Amelia)
#Replacing missing data with NA
adult[adult == "?"] <- NA
str(adult)
table(adult$occupation)
#Looking at what values might be missing
missmap(adult, main = 'Missing Map', 
        col = c('yellow','red'), legend = FALSE)
#Trimming down the Y values
missmap(adult, y.at=c(1),y.labels =c(''), 
        col=c('yellow', 'black'))
#Removing NA values
na.omit(adult)
#Verifying that their is no longer any missing values
missmap(adult, main = 'Missing Map',
        col = c('Yellow', 'black', legend = TRUE))
colnames(adult)
str(adult)
#Creating visualizations
library(ggplot2)
library(dplyr)
graph1 <- ggplot(adult, aes(age))
graph1.1 <- graph1 + geom_histogram(aes(fill=income),
                                    color='black', 
                                    binwidth=1)
print(graph1.1)
graph2 <- ggplot(adult, aes(hr_per_week)) + geom_histogram(color ='black', binwidth = 20)
print(graph2)
#Renaming the country column
library(dplyr)
names(adult)[names(adult)=="country"] <- "region"
#More graphs
colnames(adult)
str(adult)
#adult$region <- as.factor(adult$region)
#Graph of regions and their income
graph3 <- ggplot(adult, aes(region)) + geom_bar(aes(fill=income),
                                                color='black')
#Below is useful if you cant get a graph to print out

dev.off()
print(graph3)
#Below code makes the text on the x axis easier to read
graph3.1 <- graph3 + theme(axis.text.x = element_text(angle = 90,
                                                      hjust =1))
print(graph3.1)
#Building the model
library(ISLR)
head(adult)
#Splitting the data into a test and train
library(caTools)
set.seed(101)
split <- sample.split(adult$income, SplitRatio =  0.7)
train <- subset(adult, split == TRUE)
test <- subset (adult, split == FALSE)
#Using the glm model
model <- glm(income ~., family=binomial(link = 'logit'), data=train)
#A lot of columns have high siginificants codes so we might want to reduce the number of factors in those columns
summary(model)
#The below reduces variables that are not important to the model
new.step.model <- step(model)
summary(new.step.model)
#Creating a new column to test model
test$predicted.income <- predict(model, newdat = test, type = 'response')
#Creating a confusion matrix
table(test$income, test$predicted.income > 0.5)
#Checking the accuracy
#Accuracy: works best if false positives and 
#false negatives have similar costs
acc <- (6405+1367)/(518+927+6405+1367)
print(acc)
#Recall: ratio of correct predicted positive
#observations in the actual class
rec <- 6405/ (6405+518)
print(rec)
#Precision: Relates to a low false positive rate
pre <- 6405/ (6405+927)
print(pre)
