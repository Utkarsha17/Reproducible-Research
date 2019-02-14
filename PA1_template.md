---
title: "Project1"
author: "Utkarsha Patil"
date: "February 13, 2019"
output:
  html_document: 
    fig_caption: yes
    keep_md: yes
    toc: yes
  pdf_document: default
  word_document: default
---

##Introduction
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##Data
The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K] The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as \color{red}{\verb|NA|}NA) date: The date on which the measurement was taken in YYYY-MM-DD format interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

##Loading and preprocessing the data

I have already downloaded and unzipped activity.csv in my local directory.


```r
data <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
1.Calculate the total number of steps taken per day

```r
#aggregating no. of steps per day
daily_steps <- aggregate(steps ~ date, data, sum)
```

2.If you do not understand the difference between a histogram and a bar plot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(daily_steps$steps, main = paste("Total steps per day"), col="blue", xlab="No. of steps")
```
![](https://github.com/Utkarsha17/Reproducible-Research/blob/master/plot1.png)<!-- -->
<!-- -->

3.Calculate and report the mean and median of the total number of steps taken per day

```r
step_mean <- mean(daily_steps$steps)
paste("Mean of total steps taken each day:", step_mean)
```

```
## [1] "Mean of total steps taken each day: 10766.1886792453"
```

```r
step_mean
```

```
## [1] 10766.19
```

```r
step_median <- median(daily_steps$steps)
paste("Median of total steps taken each day:", step_median)
```

```
## [1] "Median of total steps taken each day: 10765"
```

## What is the average daily activity pattern?
1.Make a time series plot (i.e. \color{red} {\verb|type = "l"|} type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
interval_steps <- aggregate(steps ~ interval, data, mean)
plot(interval_steps$interval, interval_steps$steps, type="l", xlab="Interval", ylab="No. of steps", main="Avg no. steps taken per day by interval")
```

[](https://github.com/Utkarsha17/Reproducible-Research/blob/master/plot2.png)<!-- -->

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_interval <- interval_steps[which.max(interval_steps$steps),1]
paste("At 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps:", max_interval)
```

```
## [1] "At 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps: 835"
```
## Imputing missing values
1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
NA_data <- sum(is.na(data$steps))
```

```
## [1] 2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
#Aggregating mean of original data for 5 minute interval 
mean_i <- aggregate(steps ~ interval, data, mean)
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
```r
imputed_data <- transform(data, mean_i$steps == ifelse(is.na(data$steps), mean_i$steps[match(data$interval, mean_i$interval)], data$steps))

imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
```
4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#aggregate steps for new imputed data
imputed_steps <- aggregate(steps ~ date, imputed_data, sum)
daily_steps <- aggregate(steps ~ date, data, sum)

par(mfrow=c(1,2))
hist(imputed_steps$steps, col="red", xlab="No of steps", ylab="Frequency", main="No of steps(aggregating NA values)")
hist(daily_steps$steps, col="blue", xlab="No of steps", ylab="Frequency", main="No of steps(Original data)")
```

[](https://github.com/Utkarsha17/Reproducible-Research/blob/master/plot3.png)<!-- -->

```r
#Calculate the mean and median total number of steps taken per day
new_mean <- mean(imputed_steps$steps)
new_mean
```

```
## [1] 10566.81
```

```r
new_median <- median(imputed_steps$steps)
new_median
```

```
## [1] 10682.5
```

```r
#Calculate the difference between orginal and imputed data
diff_mean <- step_mean - new_mean
paste("difference between between orginal and imputed mean:", diff_mean)
```

```
## [1] "difference between between orginal and imputed mean: 199.373864430467"
```

```r
diff_median <- step_median - new_median
paste("difference between between orginal and imputed median:",diff_median)
```

```
## [1] "difference between between orginal and imputed median: 82.5"
```
## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday")
imputed_data$wk = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))
imputed_steps <- aggregate(steps ~ interval + wk, imputed_data, mean)

library(lattice)

xyplot(imputed_steps$steps ~ imputed_steps$interval|imputed_steps$wk, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

[](https://github.com/Utkarsha17/Reproducible-Research/blob/master/plot4.png)<!-- -->

