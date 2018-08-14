library(plyr)
library(ggplot2)
library(maptools)
input<-read.table("C:/Users/yogesh.mangela/Documents/R/wu03ew_v1.csv/wu03ew_v1.csv", sep=",", header=T)

## only require three columns from above dataset,

input<- input[,1:3]
names(input)<- c("origin", "destination","total")

centroids<- read.csv("C:/Users/yogesh.mangela/Documents/R/wu03ew_v1.csv/msoa_popweightedcentroids.csv")

## let's preview both datasets

str(input)
str(centroids)


#merge both dataset 
or.xy<- merge(input, centroids, by.x="origin", by.y="Code")
names(or.xy)<- c("origin", "destination", "trips", "o_name", "oX", "oY")

dest.xy<-  merge(or.xy, centroids, by.x="destination", by.y="Code")
names(dest.xy)<- c("origin", "destination", "trips", "o_name", "oX", "oY","d_name", "dX", "dY")

xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

ggplot(dest.xy[which(dest.xy$trips>10),], aes(oX, oY))+
  geom_segment(aes(x=oX, y=oY,xend=dX, yend=dY, alpha=trips), col="white")+ scale_alpha_continuous(range = c(0.03, 0.3))+
  theme(panel.background = element_rect(fill='black',colour='black'))+quiet+coord_equal()







