
rm(list = ls())
.rs.restartR()

getwd()
setwd("C:/Users/yogesh.mangela/Documents/R/Machine-Learning-with-R-datasets-master")
insurance<- read.csv("insurance.csv", as.is = TRUE)
str(insurance)
head(insurance)
summary(insurance)


summary(insurance$charges)

hist(insurance$charges)


table(insurance$age)
table(insurance$region)

cor(insurance[c("age","bmi","children","charges")])
insurance$age2<- insurance$age^2


pairs(insurance[c("age","bmi","children","charges")])
str(insurance)
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])
## lm() linear regression

ins_model<- lm(charges  ~ age+children+bmi+sex+smoker+region,data=insurance)

ins_model<- lm(charges ~ ., data = insurance)
ins_model


summary(ins_model)
insurance$age2 <- insurance$age^2
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)
ins_model
ins_model2

summary(ins_model2)



















