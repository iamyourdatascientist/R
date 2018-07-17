

if (!require("chemometrics")) install.packages("chemometrics")
# elegant way
if (!require("MASS")) install.packages("MASS")
library(chemometrics)
library(MASS)
iris # dataset comes with R
head(iris)
str(iris)

## lets look at the summary function
summary(iris)
plot(iris)## to run Ctrl + return key

cor(iris$Petal.Length,Petal.Width) ## R is case sensitive capital L
cor(iris$Petal.Length,iris$Petal.Width) ## did you see the difference

## missed out reference dataset iris$
fit_iris<- lm(iris$Petal.Length~iris$Petal.Width)
fit_iris

par(mfrow=c(2,2))## this to say plot 2 graph at a time
plot(fit_iris)

residuals(fit_iris)

summary(fit_iris)


iris2<-subset(iris,iris$Species!='setosa')
iris2
cor(iris2$Petal.Length,iris2$Petal.Width)

##let's fit model
fit_iris2<- lm(iris2$Petal.Length ~iris2$Petal.Width)
fit_iris2
summary(fit_iris2)

