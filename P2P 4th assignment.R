library(dplyr)
library(ggplot2)
library(gridExtra)
#Load csv file to the report
setwd("~/Desktop/specdata")
report <- read.csv('repdata_data_StormData.csv')

#Sum each type of fatalities and rearrange the order descending in the top 10
data1 <- aggregate(FATALITIES~EVTYPE,report,sum)
data1 <- arrange(data1,desc(data1[,2]))[1:10,]

#Sum each type of injuries and rearrange the order descending in the top 10
data2 <- aggregate(INJURIES~EVTYPE,report,sum)
data2 <- arrange(data2,desc(data2[,2]))[1:10,]

#Plot the total number of the top 10 fatalities 
g<- ggplot(data1,aes(EVTYPE,FATALITIES,fill=FATALITIES))
g <- g+  geom_bar(stat='identity')+ coord_flip + ylab("The top 10 fatalities") +
  xlab("Type of Disaster") 
#Plot the total number of the top 10 injuries 
h<- ggplot(data2,aes(EVTYPE,INJURIES,fill=INJURIES))
h <- h+geom_bar(stat='identity')+ coord_flip() + ylab("The top 10 injuries") +
  xlab("Type of Disaster")
grid.arrange(g,h, ncol=1,nrow=2)

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

report$PROPDMGEXP <- as.numeric(report$PROPDMGEXP)
report$CROPDMGEXP <- as.numeric(report$CROPDMGEXP)
unique(report$PROPDMGEXP)
unique(report$CROPDMGEXP)

#Calculation of the total cost of weather disaster on property
report$PROPDMGCost <- report$PROPDMG*report$PROPDMGEXP
report$CROPDMGCost <- report$CROPDMG*report$CROPDMGEXP

#Sum each type of property damage and rearrange the order descending in the top 10
data11 <- aggregate(PROPDMGCost~EVTYPE,report,sum)
data11 <- arrange(data11,desc(data11[,2]))[1:10,]

#Sum each type of crop damage and rearrange the order descending in the top 10
data22 <- aggregate(CROPDMGCost~EVTYPE,report,sum)
data22 <- arrange(data22,desc(data22[,2]))[1:10,]

#Plot the total number of the top 10 property damage 
g1 <- ggplot(data11,aes(EVTYPE,PROPDMGCost,fill=PROPDMGCost))
g1 <- g1+  geom_bar(stat='identity')+ coord_flip() + ylab("The top 10 property damage in US Dollar") +
  xlab("Type of Disaster")
#Plot the total number of the top 10 crop damage 
h1 <- ggplot(data22,aes(EVTYPE,CROPDMGCost,fill=CROPDMGCost))
h1 <-h1+geom_bar(stat='identity')+ coord_flip() + ylab("The top 10 crop damage in US Dollar") +
  xlab("Type of Disaster")
grid.arrange(g1,h1, ncol=1,nrow=2)
