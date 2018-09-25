
rm(list = ls())
.rs.restartR()
##dataset available here:  https://data.london.gov.uk/dataset/recorded_crime_rates
data<-read.csv("C:/Users/User/Documents/R/londonBoroughCrimeV1.csv")

str(data)
#df <- subset(df, select = c(a, c))
#data<- subset(data, select = c(Borough,Major.Category))
#str(data)
hist(data$X201807, breaks=100, main="Crime in London", xlab = "Crime")

hist(data$X201807[data$X201807<100], breaks = 100, main="Crimes in London",xlab = "Crime")
hist(data$X201807[data$X201807<20], breaks = 100, main="Crimes in London",xlab = "Crime")

str(data)
## Borough AVERAGE CRIME as of 2018-July
##aggregate(.~id1+id2, df1, mean)
rm(list = ls())
Borough_avg_Crime<- aggregate((X201608+X201609+X201610+X201611+X201612+X201701+X201702+X201703+X201704+X201705+X201706+X201707+X201708+X201709+X201710+X201711+X201712+X201801+X201802+X201803+X201804+X201805+X201806+X201807)~Borough,data = data,FUN = "mean")
colnames(Borough_avg_Crime)[2]<-"Borough_avg_Crime" 
  
##Borough_avg_Crime<- aggregate(X201807~Borough, data = data,FUN = "mean")
##colnames(Borough_avg_Crime)[2]<-"Borough_avg_Crime"

install.packages("ggplot2")
library(ggplot2)

Borough<-ggplot(Borough_avg_Crime, aes(x=Borough, y=Borough_avg_Crime,fill=Borough))+
  geom_bar(stat="identity")+ggtitle("Borough Average Crime Since Aug-2016 to 2018-July")+
  theme(axis.text.x=element_text(angle=90, hjust=1),legend.position = "none")

Borough

## Major.Category AVERAGE CRIME as of 2018-July

Major_Category_avg_Crime<- aggregate((X201608+X201609+X201610+X201611+X201612+X201701+X201702+X201703+X201704+X201705+X201706+X201707+X201708+X201709+X201710+X201711+X201712+X201801+X201802+X201803+X201804+X201805+X201806+X201807)~Major.Category, data = data,FUN = "mean")
colnames(Major_Category_avg_Crime)[2]<-"Major_Category_avg_Crime"

Major_Category_avg_Crime

Major<-ggplot(Major_Category_avg_Crime, aes(x=Major.Category, y=Major_Category_avg_Crime,fill=Major.Category))+
  geom_bar(stat="identity")+ggtitle("Major Category Average Crime Since Aug-2016 to 2018-July")+
  theme(axis.text.x=element_text(angle=90, hjust=1),legend.position = "none")

Major
## Minor.Category AVERAGE CRIME as of 2018-July

Minor_Category_avg_Crime<- aggregate((X201608+X201609+X201610+X201611+X201612+X201701+X201702+X201703+X201704+X201705+X201706+X201707+X201708+X201709+X201710+X201711+X201712+X201801+X201802+X201803+X201804+X201805+X201806+X201807)~Minor.Category, data = data,FUN = "mean")
colnames(Minor_Category_avg_Crime)[2]<-"Minor_Category_avg_Crime"

Minor_Category_avg_Crime

Minor<-ggplot(Minor_Category_avg_Crime, aes(x=Minor.Category, y=Minor_Category_avg_Crime,fill=Minor.Category))+
  geom_bar(stat="identity")+ggtitle("Minor Category Average Crime Since Aug-2016 to 2018-July")+
  theme(axis.text.x=element_text(angle=90, hjust=1),legend.position = "none")

Minor
##############################################################################################
install.packages("ggpubr")
install.packages("gridExtra")
library(ggpubr)
library("gridExtra")

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE),
       widths=c(3,1), heights=c(1,2))

Major
Borough
Minor

Graph<-grid.arrange(Major,Borough,Minor, 
             ncol = 2, nrow = 2)


