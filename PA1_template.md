---
title: "Reproducible Research: Peer Assessment 1"
author: "Germán Fajardo G"
date:  "may 3 of 2018"
output: 
  html_document:
    keep_md: true
---

## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

# Loading and preprocessing the data

## Load the data (i.e. read.csv())


```r
##Set default directory
setwd("C:/Git/RepData_PeerAssessment1")

##Extract files from file
unzip(zipfile="activity.zip")

##Read the csv File
Activity <- read.csv("activity.csv")

## Show the columns ad its first values
str(Activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## Process/transform the data (if necessary) into a format suitable for your analysis

##Omit na Values from values

```r
Data <- na.omit(Activity)
```

##Show first six rows without NA 

```r
head(Data, 6)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

# What is mean total number of steps taken per day?


```r
StepPerDays <- aggregate(steps ~ date, data = Data, FUN = sum)
hist(StepPerDays$steps, xlab="Total number of steps per day", main=NULL, col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
head(StepPerDays, 6)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

# What is the average daily activity pattern?


```r
StepPerInterval <- aggregate(steps ~ interval, data = Activity, FUN = function(x) { mean(x, na.rm = TRUE) })
 
plot2 <- plot(StepPerInterval$interval, StepPerInterval$steps, type="l", xlab="Intervals", ylab="Mean Steps")

max.StepPerInterval = max(StepPerInterval$steps)
max.Int = StepPerInterval[StepPerInterval$steps==max(max.StepPerInterval),1]

abline(h=max.StepPerInterval, col = c("red"))
abline(v=max.Int, col = c("blue"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Calculate mean of Step of days

```r
meanSteps<-mean(StepPerDays$steps)
meanSteps
```

```
## [1] 10766.19
```

## Calculate median 

```r
medianSteps<-median(StepPerDays$steps)
medianSteps
```

```
## [1] 10765
```

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
MaxSteps = max(StepPerInterval$steps)
MaxSteps
```

```
## [1] 206.1698
```

# Imputing missing values

```r
missingValues <- is.na(Activity$steps)
table(missingValues)
```

```
## missingValues
## FALSE  TRUE 
## 15264  2304
```

#Filling na values with interval's steps means

```r
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
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

# Are there differences in activity patterns between weekdays and weekends?

##Create a new factor variable in the dataset with two levels - "weekday"
##and "weekend" indicating whether a given date is a weekday or weekend day.

```r
#add a column with weekday number and weekdat type
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:plyr':
## 
##     here
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
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
```

```
##   steps       date interval wday wdaytype
## 1    NA 2012-10-01        0    2  weekday
## 2    NA 2012-10-01        5    2  weekday
## 3    NA 2012-10-01       10    2  weekday
## 4    NA 2012-10-01       15    2  weekday
## 5    NA 2012-10-01       20    2  weekday
## 6    NA 2012-10-01       25    2  weekday
```




```r
##Make a panel plot containing a time series plot (i.e. type = "l") of the
##5-minute interval (x-axis) and the average number of steps taken, averaged
##across all weekday days or weekend days (y-axis).

library(ggplot2)
options(warn=-1)
weekplot<- ggplot(Activity, aes(x = interval, y = steps)) + ylab("Number of Steps") + geom_line() + facet_grid(wdaytype~.)+ggtitle("Weekday and Weekend TimeSeries panel plot")+scale_x_discrete( limits=c(0,400,800,1200,1600,2000,2355))+theme(axis.text.x = element_text(angle = 40, hjust = 1)) + xlab("Interval")
suppressWarnings(weekplot)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
