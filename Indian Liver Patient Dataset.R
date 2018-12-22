
## Step 1 : Collect Data
##########################


##https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv


##data<- read.csv(file ="https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv")
data<- read.csv(file = "C:/Users/User/Downloads/Indian Liver Patient DatasetV2.csv")
str(data)

## Step 2 : Preparing and Exploring Data
################################################

## add column names:

colnames(data)<- c("Age","Gender","TotalBilirubin","DirectBilirubin","Alkphos","Sgpt","Sgot","TotalProtiens","Albumin","AlbuminAandGlobulinRatio","liverPatientOrNot")


str(data)
# 
# 'data.frame':	583 obs. of  11 variables:
# $ Age                     : int  65 62 62 58 72 46 26 29 17 55 ...
# $ Gender                  : Factor w/ 2 levels "Female","Male": 1 2 2 2 2 2 1 1 2 2 ...
# $ TotalBilirubin          : num  0.7 10.9 7.3 1 3.9 1.8 0.9 0.9 0.9 0.7 ...
# $ DirectBilirubin         : num  0.1 5.5 4.1 0.4 2 0.7 0.2 0.3 0.3 0.2 ...
# $ Alkphos                 : int  187 699 490 182 195 208 154 202 202 290 ...
# $ Sgpt                    : int  16 64 60 14 27 19 16 14 22 53 ...
# $ Sgot                    : int  18 100 68 20 59 14 12 11 19 58 ...
# $ TotalProtiens           : num  6.8 7.5 7 6.8 7.3 7.6 7 6.7 7.4 6.8 ...
# $ Albumin                 : num  3.3 3.2 3.3 3.4 2.4 4.4 3.5 3.6 4.1 3.4 ...
# $ AlbuminAandGlobulinRatio: num  0.9 0.74 0.89 1 0.4 1.3 1 1.1 1.2 1 ...
# $ liverPatientOrNot       : Factor w/ 2 levels "N","Y": 2 2 2 2 2 2 2 2 1 2 ...

table(data$liverPatientOrNot)

# Y   N 
# 416 167  so there are 416 liver with and 167 with non liver patient tag. as it suggest in a source page;

# Data Set Information:
#   
# This data set contains 583 liver patient records and 167 non liver patient records.
# The data set was collected from north east of Andhra Pradesh, India. Selector is a class label used to divide into groups(liver patient or not). This data set contains 441 male patient records and 142 female patient records.
# Any patient whose age exceeded 89 is listed as being of age "90".


## Step 2.1 : Data preperation - Creating random training and test datasets
###########################################################################


# 582*.20 = 116 20% test sample
# 
# 582* .80 = 466 which is  80 % of taraining sample

set.seed(123)

train_sample<- sample(582,466) ## 80% training sample data

str(train_sample)

liver_train<- data[train_sample, ]
liver_test<-  data[-train_sample, ]

str(liver_test)
str(liver_train)

## If all ok we should have 20% - 30% of liver patient in each of the datasets

prop.table(table(liver_train$liverPatientOrNot))

# Y         N 
# 0.7038627 0.2961373 

prop.table(table(liver_test$liverPatientOrNot))

# Y    N 
# 0.75 0.25



## Step 3 : Traing a model on the data
################################################


## use C5.0 Algoritham -  package
##install.packages("C50")
library("C50")


liver_model<- C5.0(liver_train[-11], liver_train$liverPatientOrNot)


liver_model

summary(liver_model)


# Evaluation on training data (466 cases):
#   
# Decision Tree   
# ----------------  
# Size      Errors  
# 27   69(14.8%)   <<
#   
#   
# (a)   (b)    <-classified as
# ----  ----
# 81    55    (a): class N
# 14   316    (b): class Y

  # TruePositive   FalsePositive 
  # 81                55
  # FalseNegative  TrueNegative
  # 14                316

#                       (TP+TN)
# Accuracy = --------------------------------
#             (total number of Observation)
# 
# (316+81)/583
# 
# [1] 0.6809605 
## AWS machine learning prediction: 0.695



## Step 4 : Evaluating model performance
################################################

liver_pred<- predict(liver_model,liver_test)

library(gmodels)

CrossTable(liver_test$liverPatientOrNot, liver_pred,
           prop.chisq = FALSE,prop.c = FALSE, prop.r=FALSE,
           dnn=c('Actual Liver Patient','Predicted Liver Patient'))

# 
# Cell Contents
#   |-------------------------|
#   |                       N |
#   |         N / Table Total |
#   |-------------------------|
#   
#   
#   Total Observations in Table:  117 
# 
# 
#                          | Predicted Liver Patient 
# 
# Actual Liver Patient      
#   |         N |         Y | Row Total | 
# 
#    -----------------------|-----------|-----------|-----------|
# N |        10 |        21 |        31 | 
#   |     0.085 |     0.179 |           | 
#   ------------------------|-----------|-----------|-----------|
# Y |        16 |        70 |        86 | 
#   |     0.137 |     0.598 |           | 
#   ------------------------|-----------|-----------|-----------|
#   Column Total |       26 |        91 |       117 | 
#   ------------------------|-----------|-----------|-----------|






## Step 5 : Improving Model performance
################################################

## Boosting accuracy decision tree

liver_boost10<- C5.0(liver_train[-17], liver_train$liverPatientOrNot,
                     trails=10)

liver_boost10

summary(liver_boost10)

liver_boost_pred10<- predict(liver_boost10, liver_test)


CrossTable(liver_test$liverPatientOrNot, liver_boost_pred10,
           prop.chisq = FALSE,prop.c = FALSE, prop.r=FALSE,
           dnn=c('Actual Liver Patient','Predicted Liver Patient'))

# 
# Cell Contents
#   |-------------------------|
#   |                       N |
#   |         N / Table Total |
#   |-------------------------|
#   
#   
#   Total Observations in Table:  117 
# 
# 
#                       | Predicted Liver Patient 
# Actual Liver Patient |      N |                     Y | Row Total | 
#      -------------------------|-----------|-----------|-----------|
#     N |        31 |         0 |                    31 | 
#       |     0.265 |     0.000 |           | 
#   ----------------------------|-----------|-----------|-----------|
#     Y |         0 |        86 |                    86 | 
#       |     0.000 |     0.735 |           | 
#   ----------------|-----------|-----------|----------------------|
# Column 
# Total |        31 |        86 |                   117 | 
#   ---------------------|-----------|------------------|-----------|



## Step 6 : Conclusion
################################################
