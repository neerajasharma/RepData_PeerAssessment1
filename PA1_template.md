---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## 1. Loading and preprocessing the data

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
unzip(zipfile = "C:/Users/b002882/Desktop/PRINT/NEERAJA/Reproducible Research/Week2/RepData_PeerAssessment1/repdata_data_activity.zip",exdir = "C:/Users/b002882/Desktop/PRINT/NEERAJA/Reproducible Research/Week2/RepData_PeerAssessment1")
activitydata <- read.csv("activity.csv")
```



## What is mean total number of steps taken per day?
#group by date and calculate sum of steps date wise
# 2.calculate Mean and median number of steps taken each day


```r
datewisesteps <- activitydata %>% select(date,steps) %>% na.omit() %>% group_by(date) %>% summarise(totalsteps=sum(steps))
mean(datewisesteps$totalsteps)
```

```
## [1] 10766.19
```

```r
median(datewisesteps$totalsteps)
```

```
## [1] 10765
```
## 3.Histogram of the total number of steps taken each day

```r
hist(datewisesteps$totalsteps, xlab="Total Steps",main="Total steps taken per day ", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
## 4.What is the average daily activity pattern?


```r
dailyactivityinterval <- activitydata %>% select(interval,steps) %>% na.omit() %>% group_by(interval) %>% summarize(meansteps=mean(steps))
ggplot(dailyactivityinterval,aes(x=interval, y=meansteps))+geom_line()+ggtitle("Average daily activity pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## 5.The 5-minute interval that, on average, contains the maximum number of steps


```r
dailyactivityinterval[which(dailyactivityinterval$meansteps == max(dailyactivityinterval$meansteps)),]
```

```
## # A tibble: 1 x 2
##   interval meansteps
##      <int>     <dbl>
## 1      835      206.
```
## 6.Imputing missing values
#6.1.Calculate and report the total number of missing values in the dataset 


```r
sum(is.na(activitydata$steps))
```

```
## [1] 2304
```
#6.2.strategy:fill  all of the missing values in the dataset by the mean for that 5-minute interval.
# cbind dataframe with missing steps data and meansteps calculated dataframe


```r
mergeActivity <- cbind(activitydata,dailyactivityinterval)
```
#6.3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
mergeActivity$steps[is.na(mergeActivity$steps)]<- mergeActivity$meansteps[is.na(mergeActivity$steps)]
```
#6.4.histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

```r
imputedatewisesteps <- mergeActivity %>% select(date,steps) %>% group_by(date) %>% summarise(totalsteps=sum(steps))

hist(imputedatewisesteps$totalsteps, xlab="Total Steps",main="Total steps taken per day ", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->
#6.4.1.Calculate and report the mean and median total number of steps taken per day

```r
mean(imputedatewisesteps$totalsteps)
```

```
## [1] 10766.19
```

```r
median(imputedatewisesteps$totalsteps)
```

```
## [1] 10766.19
```
## Are there differences in activity patterns between weekdays and weekends?

```r
weekdetails <- subset(mergeActivity[,c("steps","date","interval")])

weekdetails <- transform(weekdetails, date=as.Date(date))

weekdetails$weekday <- weekdays(weekdetails$date)

weekdetails$week  <- ifelse(weekdetails$weekday=="Saturday" |weekdetails$weekday=="Sunday","Weekend","Weekday")

ggplot(weekdetails,aes(x=interval,y=steps,color=week)) + geom_line()+facet_grid(week~.)+xlab("Interval")+ylab("Steps")+ggtitle("Activity patterns in weekdays and weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

