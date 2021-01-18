P2P Reproductive Research - Assignment 2
Kiet Huynh
1/16/2021
##Course Project:

#The goal of the assignment is to analyse the fatallities and injuries from the NOAA Storm Database from 1950 to 2011. There were less comprehensive recorded events in the earlier years; however, the most recent records have been more completed.

##Synopsis:

#During the data analysis, the top 10 most deadly fatalities: Tornado, Excessive Heat, Flash Flood, Heat, Lighting, TSTM Wind, Flood, Rip Current, Hight Wind, and Avalanche from 1950 to 2011.

knitr::opts_chunk$set(echo = TRUE)
##Data Processing:

library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library(ggplot2)
library(gridExtra)
## 
## Attaching package: 'gridExtra'
## The following object is masked from 'package:dplyr':
## 
##     combine
#Load csv file to the report
setwd("~/Desktop/specdata")
report <- read.csv('repdata_data_StormData.csv')
##Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

#Sum each type of fatalities and rearrange the order descending in the top 10

data1 <- aggregate(FATALITIES~EVTYPE,report,sum)
data1 <- arrange(data1,desc(data1[,2]))[1:10,]

#Sum each type of injuries and rearrange the order descending in the top 10
data2 <- aggregate(INJURIES~EVTYPE,report,sum)
data2 <- arrange(data2,desc(data2[,2]))[1:10,]

#Plot the total number of the top 10 fatalities 
g<- ggplot(data1,aes(EVTYPE,FATALITIES,fill=FATALITIES))
g <-g+  geom_bar(stat='identity')+ coord_flip() + ylab("The top 10 fatalities") +
    xlab("Type of Event")

#Plot the total number of the top 10 injuries 
h<- ggplot(data2,aes(EVTYPE,INJURIES,fill=INJURIES))
h <-h+geom_bar(stat='identity')+ coord_flip() + ylab("The top 10 injuries") +
    xlab("Type of Event")
grid.arrange(g,h, ncol=1,nrow=2)


#There are 91346 Injuries and 5633 Fatalities from Tornado between 1950 to 2011 in US.
##Across the United States, which types of events have the greatest economic consequences? #Collect the data from the property and crop damage.

#Substitute character with the numeric data
#Replace special character with 1
report$PROPDMGEXP[report$PROPDMGEXP==''] = 1
report$PROPDMGEXP <- gsub('[[:punct:]]',1,report$PROPDMGEXP)
report$CROPDMGEXP[report$CROPDMGEXP==''] = 1
report$CROPDMGEXP <- gsub('[[:punct:]]',1,report$CROPDMGEXP)

#Replace H or h character with 100
report$PROPDMGEXP <- gsub('[:hH:]',100,report$PROPDMGEXP)
report$CROPDMGEXP <- gsub('[:hH:]',100,report$CROPDMGEXP)
#Replace k or K character with 1000
report$PROPDMGEXP <- gsub('[:Kk:]',1000,report$PROPDMGEXP)
report$CROPDMGEXP <- gsub('[:Kk:]',100,report$CROPDMGEXP)
#Replace b or B character with 1000000
report$PROPDMGEXP <- gsub('[:mM:]',1000000,report$PROPDMGEXP)
report$CROPDMGEXP <- gsub('[:mM:]',1000000,report$CROPDMGEXP)

#Replace b or B character with 1000000000
report$PROPDMGEXP <- gsub('[:bB:]',1000000000,report$PROPDMGEXP)
report$CROPDMGEXP <- gsub('[:bB:]',1000000000,report$CROPDMGEXP)
#Convert characters to be numeric
report$PROPDMGEXP <- as.numeric(report$PROPDMGEXP)
report$CROPDMGEXP <- as.numeric(report$CROPDMGEXP)
#Calculate the total cost of the property and crop damage

#Check for special character
unique(report$PROPDMGEXP)
##  [1] 1e+03 1e+06 1e+00 1e+09 0e+00 5e+00 6e+00 4e+00 2e+00 3e+00 1e+02 7e+00
## [13] 8e+00
unique(report$CROPDMGEXP)
## [1] 1e+00 1e+06 1e+02 1e+09 0e+00 2e+00
#Calculation of the total cost of weather disaster on property
report$PROPDMGCost <- report$PROPDMG*report$PROPDMGEXP
report$CROPDMGCost <- report$CROPDMG*report$CROPDMGEXP

#Sum each type of property damage and rearrange the order descending in the top 10
data11 <- aggregate(PROPDMGCost~EVTYPE,report,sum)
data11 <- arrange(data11,desc(data11[,2]))[1:10,]
data11
##               EVTYPE  PROPDMGCost
## 1              FLOOD 144657709807
## 2  HURRICANE/TYPHOON  69305840000
## 3            TORNADO  56937161054
## 4        STORM SURGE  43323536000
## 5        FLASH FLOOD  16140812294
## 6               HAIL  15732267427
## 7          HURRICANE  11868319010
## 8     TROPICAL STORM   7703890550
## 9       WINTER STORM   6688497250
## 10         HIGH WIND   5270046295
#Sum each type of crop damage and rearrange the order descending in the top 10
data22 <- aggregate(CROPDMGCost~EVTYPE,report,sum)
data22 <- arrange(data22,desc(data22[,2]))[1:10,]
data22
##               EVTYPE CROPDMGCost
## 1            DROUGHT 13953264600
## 2              FLOOD  5515683845
## 3        RIVER FLOOD  5026345900
## 4          ICE STORM  5020616350
## 5          HURRICANE  2739570000
## 6  HURRICANE/TYPHOON  2604540280
## 7               HAIL  2506542448
## 8       EXTREME COLD  1288623300
## 9        FLASH FLOOD  1261155710
## 10      FROST/FREEZE  1088734600
#Create a visulization comprarison for the top 10 disasters 1950-2011

#Plot the total number of the top 10 property damage 
g1 <- ggplot(data11,aes(EVTYPE,PROPDMGCost,fill=PROPDMGCost))
g1 <- g1+  geom_bar(stat='identity')+ coord_flip() + ylab("The top 10 property damage in US Dollar") +
  xlab("Type of Disaster")
#Plot the total number of the top 10 crop damage 
h1 <- ggplot(data22,aes(EVTYPE,CROPDMGCost,fill=CROPDMGCost))
h1 <-h1+geom_bar(stat='identity')+ coord_flip() + ylab("The top 10 crop damage in US Dollar") +
  xlab("Type of Disaster")
grid.arrange(g1,h1, ncol=1,nrow=2)


##Based on the data, the flood disaster is the worst property damage while the drought disaster impacts the crop yield havest between 1950 to 2011
