
rm(list = ls())
.rs.restartR()
##dataset available here:  https://data.london.gov.uk/download/recorded_crime_summary/644a9e0d-75a3-4c3a-91ad-03d2a7cb8f8e/MPS%20LSOA%20Level%20Crime%20%28most%20recent%2024%20months%29.csv
##https://data.london.gov.uk/dataset/recorded_crime_summary 
## go to link: MPS LSOA Level Crime (most recent 24 months)  

data<-read.csv("C:/Users/User/Documents/R/londonBoroughCrimeV2.csv") ## stringsAsFactors = FALSE

str(data)
## Only selected Snapshot of data rather then selecting whole range of data. i.e 2016-Aug,2017-March,2018-March and 2018-July
##Step 0.0
data1<- subset(data,select=c("Borough","Major.Category","Minor.Category","X201609","X201703","X201803","X201806","X201807"))
str(data1)
library(reshape2)
##library(reshape) do not use "reshape", use "reshape2" package sometime it can get confuse and start throwing errors see below
## Warning messages:
# 1: In `[<-.factor`(`*tmp*`, ri, value = c(3L, 3L, 1L, 4L, 1L, 2L, 2L,  :
#   invalid factor level, NAs generated
# 2: In `[<-.factor`(`*tmp*`, ri, value = c(3L, 3L, 1L, 4L, 1L, 2L, 2L,  :
#   invalid factor level, NAs generated

data1
##Calculate average crime for Borough, Major crime and Minor Crime
## step 1 to melt the data in ordet to calculate the avarage in multipale columns n R.
## step 1.1 Borough Average

Borough_ave<- melt(data1, id="Borough")
Borough_ave<-subset(Borough_ave,variable%in%c("X201609","X201703","X201803","X201806","X201807"))
Borough_ave
## each column should be listed vertically (Transpose) under Variable columns. this will help us to do some grouping later to get the average for each variable.
## Step 1.2 not al Brough in our original dataset had all of these values and we also need to make sure all the values are numeric.

Borough_ave<- (na.omit(Borough_ave))
Borough_ave$value<- as.numeric(Borough_ave$value)
Borough_ave

##dim(df.melt.id)
##Step 2 once it's done we can cast the data back to make it wide again. Or Transpose it back again
## This will have a columns for each of the fields we filtered and will now contain the average by Borough. 
##At the sametime will rename columns i.e BAvg2016-Aug,BAvg2017-March,BAvg2018-March and BAvg2018-July
##c("Borough","Major.Category","Minor.Category","X201608","X201703","X201803","X201807")
Borough_ave<- dcast(Borough_ave, Borough~variable, mean)
colnames(Borough_ave)[2:6]<-c("BAvg2016-Sept","BAvg2017-March","BAvg2018-March","BAvg2018-June","BAvg2018-July")
Borough_ave

## Step 3 Let's do exact samething with Major Crime category

Major_crime_avg<- melt(data, id=c("Borough","Major.Category"))
Major_crime_avg<- subset(Major_crime_avg, variable %in% c("X201608","X201703","X201803","X201806","X201807"))
Major_crime_avg<- (na.omit(Major_crime_avg))
Major_crime_avg$value<-as.numeric(Major_crime_avg$value)
Major_crime_avg<- dcast(Major_crime_avg, Borough+Major.Category~variable, mean)
Major_crime_avg<- (na.omit(Major_crime_avg))
colnames(Major_crime_avg)[3:7]<-c("MJCAvg2016-Aug","MJCAvg2017-March","MJCAvg2018-March","MJCAvg2018-June","MJCAvg2018-July")

Major_crime_avg

## step 4 Let's do samething for Minor Crime category !! Just for Fun !!

Minor_crime_ave<- melt(data, id=c("Borough","Major.Category","Minor.Category"))
Minor_crime_ave<- subset(Minor_crime_ave,variable %in% c("X201608","X201703","X201803","X201806","X201807"))
Minor_crime_ave<-(na.omit(Minor_crime_ave))
Minor_crime_ave$value<-as.numeric(Minor_crime_ave$value)
Minor_crime_ave<- dcast(Minor_crime_ave, Borough+Major.Category+Minor.Category~variable, mean)
Minor_crime_ave<- (na.omit(Minor_crime_ave))
colnames(Minor_crime_ave)[4:8]<- c("MNCAvg2016-Aug","MNCAvg2017-March","MNCAvg2018-March","MNCAvg2018-June","MNCAvg2018-July")
Minor_crime_ave
 
 
## Step 4 we will now add the Borough, Major Crime category and Minor Crime Category AVARAGE to our original dataset  Check Step:0.0 {data}

data<- merge(data,Borough_ave,     by.x="Borough", by.y = "Borough")
data<- merge(data,Major_crime_avg, by.x=c("Borough","Major.Category"),by.y=c("Borough","Major.Category"))
data<- merge(data,Minor_crime_ave, by.x=c("Borough","Major.Category","Minor.Category"),by.y=c("Borough","Major.Category","Minor.Category"))
data

## notice that the observations / rows now decreased to 1010, it will remove al the rows which has no AVERAGE

##Step 5 let's put these new fields to use. First we will add 5 placeholders / variables which contailns 0 as default.
## so we can track whether a Borough is Under (Less) crime, or Over Crime based on being lower than the Borough 0r Major Crime Category AVERAGE;

##c("MNCAvg2016-Aug","MNCAvg2017-March","MNCAvg2018-March","MNCAvg2018-June","MNCAvg2018-July")
## Borough
data$B2016AugUnder<- 0
data$B2017MarchUnder<- 0
data$B2018MarchUnder<- 0
data$B2018JuneUnder<- 0
data$B2018JulyUnder<- 0

## Major Crime Category
data$MJC2016AugUnder<- 0
data$MJC2017MarchUnder<- 0
data$MJC2018MarchUnder<- 0
data$MJC2018JuneUnder<- 0
data$MJC2018JulyUnder<- 0

## Minor Crime Category
data$MNC2016AugUnder<- 0
data$MNC2017MarchUnder<- 0
data$MNC2018MarchUnder<- 0
data$MNC2018JuneUnder<- 0
data$MNC2018JulyUnder<- 0



##Step 6 now we will replace the 0's with 1s wherever the respective values for the Crime is less than the AVERAGE
##to indicate that these Crime might be Above AVARAGE based on that metric;

##Borough
data$B2016AugUnder[data$X201608<data$`BAvg2016-Aug`]<- 1
data$B2017MarchUnder[data$X201703<data$`BAvg2017-March`]<- 1
data$B2018MarchUnder[data$X201803<data$`BAvg2018-July`]<- 1
data$B2018JuneUnder[data$X201806<data$`BAvg2018-June`]<- 1
data$B2018JulyUnder[data$X201807<data$`BAvg2018-March`]<- 1

##Major Crime category
# 
# data$MJC2016AugUnder<- 0
# data$MJC2017MarchUnder<- 0
# data$MJC2018MarchUnder<- 0
# data$MJC2018JuneUnder<- 0
# data$MJC2018JulyUnder<- 0

data$MJC2016AugUnder[data$X201608<data$`MJCAvg2016-Aug`]<-1
data$MJC2017MarchUnder[data$X201703<data$`MJCAvg2017-March`]<-1
data$MJC2018MarchUnder[data$X201803<data$`MJCAvg2018-March`]<- 1
data$MJC2018JuneUnder[data$X201806<data$`MJCAvg2018-June`]<-1
data$MJC2018JulyUnder[data$X201807<data$`MJCAvg2018-June`]<-1


## Minor Crime Category   Shift + Ctrl + C to comment a highlighted section
# data$MNC2016AugUnder<- 0
# data$MNC2017MarchUnder<- 0
# data$MNC2018MarchUnder<- 0
# data$MNC2018JuneUnder<- 0
# data$MNC2018JulyUnder<- 0

data$MNC2016AugUnder[data$X201608<data$`MNCAvg2016-Aug`]<- 1
data$MNC2017MarchUnder[data$X201703<data$`MNCAvg2017-March`]<- 1
data$MNC2018MarchUnder[data$X201803<data$`MNCAvg2018-March`]<- 1
data$MNC2018JuneUnder[data$X201806<data$`MNCAvg2018-June`]<- 1
data$MNC2018JulyUnder[data$X201807<data$`MNCAvg2018-July`]<-1


## finally we will sum these 15 culumns to create a new column with the index value stating 1 to 10, 
##how high crimes is based on the differnt dimensions that were considered.

data

str(data) ##data.frame':	1053 obs. of  38 variables: 38 - 15 clumns = 23 columns

data$RealCrimeIndex<- apply(data[23:38],1,sum)

potentially_high_Crime_Borough<- subset(data,RealCrimeIndex>500)
potentially_high_Crime_Borough


#sort by index (ascending) and cyl (descending)
##newdata <- mtcars[order(mpg, -cyl),] 

potentially_high_Crime_BoroughOrder<- potentially_high_Crime_Borough[order(data$RealCrimeIndex),]
potentially_high_Crime_BoroughOrder


potentially_high_Crime<- subset(data,RealCrimeIndex>=1)
potentially_high_Crime

## Download Final Dataset
write.csv(potentially_high_Crime,'LondonBoroughCrimeIndex.csv')

## Some confesions: 
## I admitt that this is an overly simplistic approach to analyse data. However,
## It provides aframework to expand your horizon, Once comfortable with this process you can for i.e:
## Weigh the vlues differently 
## Add or remove criteria,
## Create more accurate index crime rate than just 0s and 1s 
## finally Create tailored criteria to assign a 1 to indicate that the Crime is Highly rated in a Borough.
## The SKY IS  LIMIT HERE but process is the same.









 
 
 
 
 
 
 
