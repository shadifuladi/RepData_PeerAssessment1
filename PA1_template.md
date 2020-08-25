---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

This is an R Markdown document on Course Project 1 of Reproducible Research course on Coursera. In this assignment the data from presonal activity monitoring devices, such as number of steps and the corresponding time intervals will be analyzed. 

Here is what the data file looks like, name of variables in data file, and a summary of the data: 


```r
unzip("activity.zip")
data <- read.csv("activity.csv")
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
names(data)
```

```
## [1] "steps"    "date"     "interval"
```

```r
summary (data) 
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

## What is the total number of steps taken per day? 

The missing values (NA) are ignored for this section.

1. Total number of steps taken per day: 


```r
total_steps_per_day <- tapply (data$steps, data$date, FUN=sum)
dates <-unique(data$date)
data_step <- cbind(dates, total_steps_per_day)
summary(total_steps_per_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```

2. A histogram of total number of steps taken each day:


```r
hist(total_steps_per_day, main='Total number of steps taken each day' ,xlab='Steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

3. Mean and median of the total number of steps taken per day, respectively: 


```r
mean(total_steps_per_day, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(total_steps_per_day, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern? 

1. A time series plot of the 5-minutes interval and average of number of steps taken in each, averaged across all days: 


```r
avg_steps_per_time_interval <- tapply(data$steps, data$interval, FUN=mean, na.rm=TRUE)
time_interval <- unique(data$interval)
plot( time_interval, avg_steps_per_time_interval, main = 'Average number of steps in each time interval', xlab='5-minute interval',ylab=' Average number of steps taken', type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
ind_max_step <- which.max(avg_steps_per_time_interval)
max_interval <- time_interval[ind_max_step]
```

The 5-minute interval of **835** contains maximum number of steps per day. 

## Imputing missing values

1. Total number of missing values: 


```r
total_na <- sum(is.na(data$steps))
```

There are, in total, **2304** missing values in the data set.

2. Filling in all the missing values, by substituing the mean value of same intervals over all days. 


```r
update_data <- data
avg_intervals <- cbind(avg_steps_per_time_interval, time_interval)
ind <- which(is.na(update_data$steps))
for (i in ind) {
  new_value <- as.numeric (avg_intervals[ avg_intervals[,2] == update_data$interval[i],1])
 update_data$steps[i] <- new_value  
}
```

3. Here is the new data set: 


```r
head(update_data)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
summary(update_data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 27.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0
```

4. A histogram of the total number of steps taken each day and the **mean** and **median** total number of steps taken per day:


```r
update_total_steps_per_day<- tapply (update_data$steps, update_data$date, FUN=sum)
hist(update_total_steps_per_day, main='Total number of steps taken each day -- missing values are filled' ,xlab='Steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

And here are new mean and median, respectively: 


```r
mean(update_total_steps_per_day, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(update_total_steps_per_day, na.rm = TRUE)
```

```
## [1] 10766.19
```
Replacing the missing values with the corresponding averages over same time intervals does not seem to change the distribution of the data set. 
## Are there differences in activity patterns between weekdays and weekends?

1. Add a colomn to the data file which indicates TRUE for weekdays and FALSE for weeekends: 


```r
added_data <- update_data
days <-  weekdays(as.Date(update_data$date, format = '%Y-%m-%d'))

col_days <- vector()
for (i in 1:nrow(update_data)) {
    if (days[i] == "Saturday") {
        col_days[i] <- "Weekend"
    } else if (days[i] == "Sunday") {
        col_days[i] <- "Weekend"
    } else {
        col_days[i] <- "Weekday"
    }
}

added_data <- cbind(added_data, col_days)
head(added_data)
```

```
##       steps       date interval col_days
## 1 1.7169811 2012-10-01        0  Weekday
## 2 0.3396226 2012-10-01        5  Weekday
## 3 0.1320755 2012-10-01       10  Weekday
## 4 0.1509434 2012-10-01       15  Weekday
## 5 0.0754717 2012-10-01       20  Weekday
## 6 2.0943396 2012-10-01       25  Weekday
```

2. Making a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.


```r
steps_by_day <- aggregate(steps ~ interval + col_days, data = added_data, mean)
names(steps_by_day) <- c("interval", "day", "steps")
library("lattice")
xyplot(steps ~ interval | day, steps_by_day, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
