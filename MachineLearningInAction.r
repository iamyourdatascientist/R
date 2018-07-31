

housing<- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
housing

# adding column names

colnames(housing)<- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PRATIO","B","LSTAT","MDEV")
str(housing)

summary(housing)

plot(housing)

install.packages("corrplot")


library(corrplot)
corrplot(cor(housing),method = "number",t1.cex=0.5)


## partitioning data to test and training data.

housing<- housing[order(housing$MDEV),]
warnings()


install.packages("caret")
library(caret)

set.seed(3277)
trainingIndices<- createDataPartition(housing$MDEV, p=0.75,list = FALSE)

housingTrainig<- housing[trainingIndices,]
housingtest<- housing[-trainingIndices,]
# check number of rows in partitioned data
nrow(housingtest)## 125 rows
nrow(housingTrainig)## 381 rows
str(housingTrainig)


######Model##############
##"CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PRATIO","B","LSTAT",

str(housing)
## R has reserved words and they are case sensetive data=..., not DATA=..
linearModel<- lm(MDEV ~ CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PRATIO+B+LSTAT, data=housingTrainig)

warnings()
## let's summarise it

summary(linearModel)



########## Prediction ############

prediction<- predict(linearModel, newdata = housingtest)
summary(prediction)

summary(housingtest$MDEV)

plot(prediction,housingtest$MDEV)

######### sum of squares ########

sumofsquares<- function(x){
  return(sum(x^2))
}

sumofsquares(1:5)
diff<- prediction - housingtest$MDEV
sumofsquares(diff)

########### Logistic Regression ##############

##lm(MDEV ~ CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PRATIO+B+LSTAT, data=housingTrainig)

LgReg<- glm(MDEV ~ CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PRATIO+B+LSTAT, data=housingTrainig) 
summary(LgReg)

predicted<- predict(LgReg, newdata = housingtest)

summary(predicted)


plot(predicted,housingtest$MDEV)

diff<- predicted - housingtest$MDEV
sumofsquares(diff)


######### Residuals ##########

plot(resid(linearModel))

## Least squares regression ##################

x<- housingtest$MDEV
Y<- predicted

b1<- sum((x-mean(x))*(Y-mean(Y)))/sum((x-mean(x))^2)
b0<- mean(Y)-b1*mean(x)
c(b0,b1)
plot(x,Y)

## b0 + b1 * X, where b0 = Interceptor, b1 = Slop of line
## add a least square regression line to the plot

abline(c(b0,b1),col="blue", lwd=2)


########## Stepwise regression ##############

##install.packages("MASS")
library(MASS)

step<- stepAIC(linearModel, direction = "both")

######### The K-Nearest neighbor classification #####

library(class)
knnModel<- knn(train=housingTrainig, test = housingtest, cl=housingTrainig$MDEV)
summary(knnModel)


## frequency bar graph

plot(knnModel)

plot(housingtest$MDEV)


## Naive Bayes ####

##install.packages("e1071")

##lm(MDEV ~ CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PRATIO+B+LSTAT, data=housingTrainig)

library(e1071)

nb<- naiveBayes(MDEV ~ CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PRATIO+B+LSTAT, data=housingTrainig)
nb$tables$TAX

## apriori
plot(nb$apriori)

##decision tree
library(rpart)
housingFit<- rpart(MDEV ~ CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PRATIO+B+LSTAT, method = "anova" ,data=housingTrainig)

plot(housingFit)
text(housingFit, use.n = TRUE, all = TRUE, cex=.8)

treePredict<- predict(housingFit, newdata = housingtest)

diff<- treePredict - housingtest$MDEV
sumofsquares(diff)

### Neural Network ############
install.packages("neuralnet")
library(neuralnet)
nnet<- neuralnet(MDEV ~ CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PRATIO+B+LSTAT, hidden = 10, threshold = 0.01 ,data=housingTrainig)

nnet<- neuralnet(MDEV ~ CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PRATIO+B+LSTAT,housingTrainig)

plot(nnet,rep = "best")

results<- compute(nnet, housingtest[,-14])
diff<- results$net.result- housingtest$MDEV
sumofsquares(diff)


## Random forest ##

install.packages("randomForest")
library(randomForest)
forestFit<- randomForest(MDEV ~ CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PRATIO+B+LSTAT,data=housingTrainig)
forestPredict<- predict(forestFit, newdata =  housingtest)
diff<- forestPredict - housingtest$MDEV
sumofsquares(diff)


https://iamyourdatascientist.wordpress.com/



