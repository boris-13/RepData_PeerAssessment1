---
title: "Reproducible Research Course Project 1"
output: 
  html_document: 
    keep_md: yes
---
*Boris Romanciuc*

*25-09-2018*



## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

For project assignment we will have to answer the following questions:

- What is mean total number of steps taken per day?

- What is the average daily activity pattern?

- Imputing missing values

- Are there differences in activity patterns between weekdays and weekends?

## Loading and preprocessing the data

For this assignment, as the first step we will load the data file “activity.csv” by read.csv function and also we will load the ggplot2 library, then will trasnform the date filed format to the correct one and add the extra column 'weekday':


```r
library(ggplot2)

info <- read.csv("activity.csv")

info$date <- as.POSIXct(info$date, "%Y-%m-%d")
weekday <- weekdays(info$date)
info <- cbind(info,weekday)

summary(info)
```

```
##      steps             date               interval           weekday    
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Friday   :2592  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Monday   :2592  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Saturday :2304  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   Sunday   :2304  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   Thursday :2592  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Tuesday  :2592  
##  NA's   :2304                                           Wednesday:2592
```

## 1. What is mean total number of steps taken per day?

Next we will generate a histogram of the daily total number of steps taken, showing the distribution of these totals:


```r
info_all_steps <- with(info, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(info_all_steps) <- c("date", "steps")
hist(info_all_steps$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "darkviolet", ylim = c(0,20), breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

Next we calculate and report the mean and median total number of steps taken per day:

```r
mean(info_all_steps$steps)
```

```
## [1] 9354.23
```


```r
median(info_all_steps$steps)
```

```
## [1] 10395
```


## 2. What is the average daily activity pattern?

For the average daily activity pattern, we will create a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):

```r
avr_daily_info <- aggregate(info$steps, by=list(info$interval), FUN=mean, na.rm=TRUE)
names(avr_daily_info) <- c("interval", "mean")
plot(avr_daily_info$interval, avr_daily_info$mean, type = "l", col="darkviolet", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
```

![](PA1_template_files/figure-html/average_daily-1.png)<!-- -->

Next we will check which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps:

```r
avr_daily_info[which.max(avr_daily_info$mean), ]$interval
```

```
## [1] 835
```

## 3. Imputing missing values

Now we will calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs):

```r
sum(is.na(info$steps))
```

```
## [1] 2304
```

So the original data set has 2304 rows with missing data.

We will use a simple strategy for filling in all of the missing values in the dataset. If a 5-minute interval has missing value, we use the mean for that 5-minute interval:

```r
imputed_steps <- avr_daily_info$mean[match(info$interval, avr_daily_info$interval)]
```

We now are creating a new dataset that is equal to the original dataset but with the missing data filled in (using mean for that interval for imputation):

```r
info_imputed <- transform(info, steps = ifelse(is.na(info$steps), yes = imputed_steps, no = info$steps))
all_steps_imputed <- aggregate(steps ~ date, info_imputed, sum)
names(all_steps_imputed) <- c("date", "daily_steps")
```

Next, we will make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
hist(all_steps_imputed$daily_steps, col = "darkviolet", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day (missing data imputed)", breaks = seq(0,25000,by=2500))
```

![](PA1_template_files/figure-html/hist_imputed-1.png)<!-- -->

Now we calculate again and report the mean and median total number of steps taken per day (with the missing data imputed):

```r
mean(all_steps_imputed$daily_steps)
```

```
## [1] 10766.19
```


```r
median(all_steps_imputed$daily_steps)
```

```
## [1] 10766.19
```

## 4. Are there differences in activity patterns between weekdays and weekends?

For this purpose we will create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
info$date <- as.Date(strptime(info$date, format="%Y-%m-%d"))
info$daytype <- sapply(info$date, function(x) {
        if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })
head(info)
```

```
##   steps       date interval weekday daytype
## 1    NA 2012-10-01        0  Monday Weekday
## 2    NA 2012-10-01        5  Monday Weekday
## 3    NA 2012-10-01       10  Monday Weekday
## 4    NA 2012-10-01       15  Monday Weekday
## 5    NA 2012-10-01       20  Monday Weekday
## 6    NA 2012-10-01       25  Monday Weekday
```


Next we will make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis):

```r
info_by_date <- aggregate(steps~interval + daytype, info, mean, na.rm = TRUE)
plot<- ggplot(info_by_date, aes(x = interval , y = steps, color = daytype)) +
       geom_line() +
       labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
       facet_wrap(~daytype, ncol = 1, nrow=2)
print(plot)
```

![](PA1_template_files/figure-html/plot_day-1.png)<!-- -->

From the panel plot it looks like the weekday activities arise earlier than the weekends - weekday activities arise around 5~6am and weekend activities arise around 8am. We can also observe that from 10am to 5pm, the weekends have higher activity levels than the weekdays.

