##Set default directory
setwd("C:/Git/RepData_PeerAssessment1")

##Extract files from file
unzip(zipfile="activity.zip")

##Read the csv File
Activity <- read.csv("activity.csv")

## Show the columns ad its first values
str(Activity)

Data <- na.omit(Activity)

head(Data, 6)

StepPerDays <- aggregate(steps ~ date, data = Data, FUN = sum)
hist(StepPerDays$steps, xlab="Total number of steps per day", main=NULL, col="blue")
head(StepPerDays, 6)

StepPerInterval <- aggregate(steps ~ interval, data = Activity, FUN = function(x) { mean(x, na.rm = TRUE) })

plot2 <- plot(StepPerInterval$interval, StepPerInterval$steps, type="l", xlab="Intervals", ylab="Mean Steps")

max.StepPerInterval = max(StepPerInterval$steps)
max.Int = StepPerInterval[StepPerInterval$steps==max(max.StepPerInterval),1]

abline(h=max.StepPerInterval, col = c("red"))
abline(v=max.Int, col = c("blue"))

## Calculate mean of Step of days
meanSteps<-mean(StepPerDays$steps)
meanSteps

## Calculate median 
medianSteps<-median(StepPerDays$steps)
medianSteps

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
MaxSteps = max(StepPerInterval$steps)
MaxSteps

# Imputing missing values
missingValues <- is.na(Activity$steps)
table(missingValues)


#Filling na values with interval's steps means
library(plyr)

##Copy of Activity
ActivityM<-Activity

##ActivityM with mean
ActivityM[ , 1:1][is.na(ActivityM[ , 1:1] ) ] =  mean(Data$steps) 

##Cumulative steps with NA
spd <- aggregate(steps ~ date, data = Activity, FUN = sum)

##Cumulative steps with mean
spdm <- aggregate(steps ~ date, data = ActivityM, FUN = sum)

##Paint both histograms 
hist(spdm$steps , col=rgb(1,0,0,0.5), main="Total number of steps per day Mean for NA", ylab="Frequency", 
     xlab="Total number of steps per day")
hist(spd$steps , col=rgb(0,0,1,0.5), add=T)
box()

##Create a new factor variable in the dataset with two levels - "weekday"
##and "weekend" indicating whether a given date is a weekday or weekend day.

#add a column with weekday number and weekdat type
library(lubridate)
## day of week in number
wd <- wday(ymd(Activity$date))

## Classes for weekday and weekend days
wtype<-ifelse(wd %in% c(6, 7), "weekend", "weekday")
attach(Activity)

##Adds columns to dataframe
Activity$wday<- wd
Activity$wdaytype<- wtype
detach(Activity) 

#shows the dataframe with the added columns
head(Activity)

##Make a panel plot containing a time series plot (i.e. type = "l") of the
##5-minute interval (x-axis) and the average number of steps taken, averaged
##across all weekday days or weekend days (y-axis).

library(ggplot2)
options(warn=-1)
weekplot<- ggplot(Activity, aes(x = interval, y = steps)) + ylab("Number of Steps") + geom_line() + facet_grid(wdaytype~.)+ggtitle("Weekday and Weekend TimeSeries panel plot")+scale_x_discrete( limits=c(0,400,800,1200,1600,2000,2355))+theme(axis.text.x = element_text(angle = 40, hjust = 1)) + xlab("Interval")
weekplot


