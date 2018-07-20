## Data source : https://www.kaggle.com/yersever/500-person-gender-height-weight-bodymassindex
## clean data. changed Female:1, male:0
## dataset has Gender,	Height,	Weight and	Index


#Gender : Male / Female
# Height : Number (cm)
# Weight : Number (Kg)
# Index :
# 0 - Extremely Weak
# 1 - Weak
# 2 - Normal
# 3 - Overweight
# 4 - Obesity
# 5 - Extreme Obesity#

## Added BMI column to include all the above index categories  0-5


#Step 1. Clean and Import data
rm(list = ls())
.rs.restartR()

getwd()
setwd("C:/Users/yogesh.mangela/Documents/R")

#step0<- read.csv("500_Person_Gender_Height_Weight_Index.csv", as.is = TRUE,Header=TRUE)
BMI<- read.csv("500_Person_Gender_Height_Weight_Index.csv", as.is = TRUE)
BMI

##Step 2 - exploring and preparing the data

str(BMI)

table(BMI$Gender)

# Female:1, male:0 
#  0   1 
# 245 255 

summary(BMI$Height)

summary(BMI)
#Step 3 - training a model on the data
# kmeans(dataset, k)
#k : specifies the desired number of clusters

library(psych)
str(BMI)
pairs.panels(BMI[c("Gender","Height","Weight","Index","BMI")])

install.packages("mclust")
library(mclust)
#fit_BMI<- Mclust(BMI)


library(ggplot2)
#ggplot(BMI, aes(Gender,Height,Weight,Index,BMI, color=BMI))+ geom_point()

ggplot(BMI, aes(Height,Weight, color=BMI))+ geom_point()

ggplot(BMI, aes(Index,Weight, color=BMI))+ geom_point()


set.seed(250)
BMI_Cluster<- kmeans(BMI[,1:4], 5, nstart = 250)
BMI_Cluster


table(BMI_Cluster$cluster, BMI$BMI)

BMI_Cluster$cluster

##irisCluster$cluster <- as.factor(irisCluster$cluster)
##ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()
## columns c("Gender","Height","Weight","Index","BMI")
BMI_Cluster$cluster<- as.factor(BMI_Cluster$cluster)
ggplot(BMI, aes(Height,Weight, color=BMI_Cluster$cluster))+ geom_point()


