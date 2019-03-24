#Data Visualization projects
library(ggplot2)
library(data.table)
library(ggthemes)
help(fread)
df <- fread('Economist_Assignment_Data.csv', drop=1)
#Used to get working directory
getwd()
#Change working directory
setwd()
head(df)
#Shape and size should be outside the aes function
pl <- ggplot(df, aes(x=CPI, y=HDI, color = Region)) + 
  geom_point(size = 4, shape = 1)
#This prints out region broken down 
#by color in open circle shapes
print(pl)
#SE = F removes grayed out section of smooth line
pl2 <- pl + geom_smooth(aes(group=1), method='lm',
                        formula = y~log(x),
                        se=F, color = 'red')
#This prints out a line of best fit on the graph
print(pl2)
pl3  <- pl2 + geom_text(aes(label=Country))
#Prints the graph with country names included
print(pl3)
#Only want to label the below list of countries
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")
pl4 <- pl2 + geom_text(aes(label = Country), color = "gray20",
                       data = subset(df, Country %in% pointsToLabel), 
                       check_overlap = T)
#Only prints out the countries in the pointsToLabel list
print(pl4)
pl5 <- pl4 + theme_economist_white()+ scale_x_continuous(
  limits=c(.9,10.5), breaks=1:10) 
print(pl5)
help(geom_smooth)
