## How to draw connection graph.

#Step1 Install all the required packages.
install.packages("tidyverse")

## you may use below to check if the package exist, just like SQL table exist drop command

##if (!require(pkg)){  install.packages(pkg)}
if (!require(dplyr)){  install.packages(dplyr)}
install.packages("maps")
install.packages("geosphere")
install.packages("nycflights13")
library(dplyr)
library(nycflights13)
library('tidyverse')
library('maps')
library(geosphere)

## Step 2 Draw empty map canvas 
par(mar=c(0,0,0,0))
map('world',col="#f2f2f2", fill=TRUE, bg="sky blue", lwd=0.05,mar=rep(0,4),border=01, ylim=c(-80,80) )

##belwo are the datasets

airports
airlines
airmiles
airquality
AirPassengers

## view more rows

options(max.print=25000)
print.data.frame(airports)


usairports <- filter(airports, lat < 48.5) # select airport with latitude less than 48.5
usairports <- filter(usairports, lon > -130) ## and select longitute with greater than -130 
usairports <- filter(usairports, faa!="JFK") #select data out jfk jsut like SQL
jfk <- filter(airports, faa=="JFK") #select df for jfk

## now let's design basemap

#create basemap
map("world", regions=c("usa"), fill=T, col="grey8", bg="grey15", ylim=c(21.0,50.0), xlim=c(-130.0,-65.0))
+ points(usairports$lon,usairports$lat, pch=3, cex=0.1, col="chocolate1")

## Flights from JFK airports to rest of the USA airports

for (i in (1:dim(usairports)[1])) { 
  inter <- gcIntermediate(c(jfk$lon[1], jfk$lat[1]), c(usairports$lon[i], usairports$lat[i]), n=200)
  lines(inter, lwd=0.1, col="turquoise2")    
}
