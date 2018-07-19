## Forecasting Numetic Fata - Regression Methods

rm(list = ls())
.rs.restartR()

getwd()
setwd("C:/Users/yogesh.mangela/Documents/R")

#step0<- read.csv("SmokingHabitInUK.csv", as.is = TRUE,Header=TRUE)
Smoking<- read.csv("SmokingHabitInUK.csv", as.is = TRUE)
Smoking

## Examine structure of the dataset. 54 observations /  records. of  7 variables /  columns:
str(Smoking)

## checking summary Min., 1st Qu., Median, Mean, 3rd Qu., Max.

summary(Smoking)


library(tidyverse)
library(tidyr)
library(ggplot2)

str(Smoking)

hist(Smoking$PropSmoke)

##To create a correlation matrix for the four numeric variables in the insurance data
##frame, use the cor() command:
  
cor(Smoking[c("PropSmoke","PropExSmokers","PropNeverSmoked","PropQuitSmoke","AveDailyConsumption")])


## There's correlation between Proportion Quite Skmmoking Vs Proportion Ex Smokers
pairs(Smoking[c("Year","PropSmoke","PropExSmokers","PropNeverSmoked","PropQuitSmoke","AveDailyConsumption")])

##install.packages("psych")

library(psych)
pairs.panels(Smoking[c("PropSmoke","PropExSmokers","PropNeverSmoked","PropQuitSmoke","AveDailyConsumption")])


Smoker_Model<- lm(PropQuitSmoke~PropSmoke+PropExSmokers+PropNeverSmoked+AveDailyConsumption+Year, data = Smoking)

Smoker_Model

summary(Smoker_Model)


## Residuals = True value - the predicted value
## Adjusted R-squared:  0.9892, 0.99

##It is similar to the correlation coefficient, in that
##the closer the value is to 1.0, the better the model perfectly explains the data.
##Since the R-squared value is 0.9892, we know that the model explains nearly
## 99 percent of the variation in the dependent variable. 
##==============================================================================================##
str(Smoking)

Smoke_train<- Smoking[1:37,]
Smoke_test<- Smoking[38:54,]

Smoke_test

Smoke_train

str(Smoking)

## install.packages("rpart")
library(rpart)
m.rpart<- rpart(AveDailyConsumption ~ ., data = Smoke_train)

m.rpart


summary(m.rpart)
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)

rpart.plot(m.rpart, digits = 3, fallen.leaves = TRUE, type = 3 , extra =101)

#evaluating model performance

p.rpart<- predict(m.rpart, Smoke_test)

summary(p.rpart)

summary(Smoke_test$AveDailyConsumption)

cor(p.rpart, Smoke_test$AveDailyConsumption)

#Measuring performance with the mean absolute error Mean absolute Error (MAE)

install.packages("Metrics")
library(Metrics)
##
mae(p.rpart, Smoking$AveDailyConsumption)

mean(Smoking$AveDailyConsumption) #13.18
mae(13.18, Smoking$AveDailyConsumption)












