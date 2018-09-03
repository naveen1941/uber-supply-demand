#Load the required packages
library(dplyr)
library(ggplot2)
library(stringr)

#Load the data - masterData is the UserRequestData given for analysis
masterData <- read.csv("Uber request data.csv",stringsAsFactors = FALSE,na.strings = TRUE)

#Check the summary of the data
summary(masterData)

#View the dataframe for pointing the inconsistent data formats
View(masterData)

#Fix the timestamps seperator use the same common seperator
masterData$RequestDate <- str_replace_all(masterData$Request.timestamp, "[/]", "-")
masterData$DropDate <- str_replace_all(masterData$Drop.timestamp, "[/]", "-")

#Convert Timestamps from string format to data format
masterData$RequestDate <- as.POSIXlt(masterData$RequestDate, format = "%d-%m-%Y %H:%M")
masterData$DropDate <- as.POSIXlt(masterData$DropDate, format = "%d-%m-%Y %H:%M")

#Create calculated metrics - Hour of cab request time and day of the request
masterData$RequestHour <- format(masterData$RequestDate, "%H")
masterData$RequestDay <- format(masterData$RequestDate, "%d")



#Plots for analysis

#Hourly Demand Analysis - From Airport & City
#_____________________________________ PLOT 1 ________________________________________________
hourwiseRequestFreq <- ggplot(masterData,aes(x=factor(RequestHour),fill=factor(Pickup.point)))
hourwiseRequestFreqPlot <- hourwiseRequestFreq+geom_bar(stat='count',position = "dodge")+
  ggtitle("Hourly Demand Analysis")+
  labs(x="Hour of the day", y="No. of Cabs Requested")+
  labs(fill="Pickup Point")
hourwiseRequestFreqPlot
#_____________________________________________________________________________________________


#Create calculated metrics - Timeslots
masterData$RequestHour <- as.numeric(masterData$RequestHour)
masterData$RequestSlot = ifelse(masterData$RequestHour < 5, "EarlyMorning", 
                         ifelse(masterData$RequestHour < 10,"Morning",
                         ifelse(masterData$RequestHour < 17,"Day",
                         ifelse(masterData$RequestHour < 22,"Evening","LateNight"))))

#Identify problem 1
#_____________________________________ PLOT 2 ________________________________________________
morningRequests <- subset(masterData, masterData$RequestSlot=="Morning")
tripStatusCount <- ggplot(morningRequests,aes(x=factor(Status),fill=factor(Pickup.point)))
tripStatusCountBarPlot <- tripStatusCount+geom_bar(stat="count",position = "dodge")+
  ggtitle("Morning Cab Status")+
  labs(x="Trip Status",y="Total count")+
  geom_text(stat='count',aes(label=..count..),vjust=-1)+
  labs(fill="Pickup Point")+scale_x_discrete(limits=c("Trip Completed","Cancelled","No Cars Available"))
tripStatusCountBarPlot
#_____________________________________________________________________________________________

#Identify problem 2
#_____________________________________ PLOT 3 ________________________________________________
eveningRequests <- subset(masterData, masterData$RequestSlot=="Evening")
tripStatusCount <- ggplot(eveningRequests,aes(x=factor(Status),fill=factor(Pickup.point)))
tripStatusCountBarPlot <- tripStatusCount+geom_bar(stat="count",position = "dodge")+
  ggtitle("Evening Cab Status")+
  labs(x="Trip Status",y="Total count")+
  geom_text(stat='count',aes(label=..count..))+
  labs(fill="Pickup Point")+scale_x_discrete(limits=c("Trip Completed","Cancelled","No Cars Available"))
tripStatusCountBarPlot
#_____________________________________________________________________________________________


#Demand & Summpy analysys
#Have drawn charts using Tablue because of the time constaints