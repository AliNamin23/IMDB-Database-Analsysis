#Ali Namin,
#INST314 Project 4
#5/14/2019

#Load in Libraries
install.packages("summarytools")
install.packages("descr")
install.packages("DescTools")
install.packages("pwr")
install.packages("stargazer")
install.packages("PerformanceAnalytics")

library("summarytools")
library("descr")
library("DescTools")
library("pwr")

#Load in data
movieData <- read.csv(file.choose(), header=T)

#make US movies
movieData <- subset(movieData, movieData$country == "USA")

#rename IV's
budget <- movieData$budget 
duration <- movieData$duration
IMDBscore <- movieData$imdb_score
votes <- movieData$num_voted_users

# IV summary stats
#budget
summary(budget)
sd(budget, na.rm=T)
#duration
summary(duration)
sd(duration, na.rm=T)
#IMDBscore
summary(IMDBscore)
sd(IMDBscore, na.rm=T)
#votes
summary(votes)
sd(votes, na.rm=T)

#rename DV
gross<-movieData$gross

#DV summary stats
summary(gross)
sd(gross, na.rm=T)

#create multiple OLS models
budgetModel <- lm(gross ~ budget, data=movieData)
durationModel <- lm(gross ~ duration, data=movieData)
IMDBscoreModel <- lm(gross ~ IMDBscore, data=movieData)
votesModel <- lm(gross ~ votes, data=movieData)
combineModel <- lm(gross ~ budget + duration + IMDBscore + votes, data=movieData)

#stargazer
library("stargazer")
stargazer(budgetModel, durationModel, IMDBscoreModel, votesModel, combineModel,
          type = "html", out = "P4Stargazer(1).html")

summary(budgetModel)
summary(durationModel)
summary(IMDBscoreModel)
summary(votesModel)
summary(combineModel)

#regression assumptions
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(subset(movieData, select=c(budget, duration, IMDBscore, votes)
                         , histogram=TRUE, pch="+"))

#assumptions plots
options(scipen = 4)
plot(combineModel)



