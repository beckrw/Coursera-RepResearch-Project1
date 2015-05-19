# Reproducible Research: Peer Assessment 1

Loading dplyr and ggplot2 library


```r
library(dplyr)
library(ggplot2)
```

## Loading and preprocessing the data

Read in activity.csv and display the first 6 rows


```r
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

## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day and plot the histogram


```r
by.date <- group_by(data, date)
summary1 <- summarise(by.date, total.steps=sum(steps, na.rm=TRUE))
ggplot(data=summary1, aes(summary1$total.steps)) + geom_histogram(col="red", fill="green") + labs(title="Total Number of Steps Taken Each Day") + labs(x="Total Number of Steps", y="Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Use summary to report the mean and median of the total number of steps taken per day


```r
summary(summary1$total.steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

## What is the average daily activity pattern?

Calculate the average number of steps taken, averaged across all days per 5-minute interval


```r
by.interval <- group_by(data, interval)
summary2 <- summarise(by.interval, average.steps=mean(steps, na.rm=TRUE))
ggplot(data=summary2, aes(x=interval, y=average.steps, group=1)) + geom_line() + xlab("5-Minute Interval") + ylab("Average Number of Steps Taken Across All Days") + ggtitle("Average Number of Steps Taken Across All Days at 5-Minute Interval")
```

![](Coursera-RepResearch-Project1/figures-html/unnamed-chunk-1.png) 

Use which.max to report Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps


```r
summary2$interval[which.max(summary2$average.steps)]
```

```
## [1] 835
```

## Imputing missing values

Use sum to calculate and report the total number of missing values in the dataset 


```r
sum(is.na(data$step))
```

```
## [1] 2304
```

Fill in missing data with the mean for that 5-minute interval then calculate the total number of steps taken per day with the new dataset and plot the histogram


```r
by.interval <- group_by(data, interval)
data1 <- mutate(by.interval, steps= replace(steps, is.na(steps), mean(steps, na.rm=TRUE)))
head(data1)
```

```
## Source: local data frame [6 x 3]
## Groups: interval
## 
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
by.date <- group_by(data1, date)
summary3 <- summarise(by.date, total.steps=sum(steps, na.rm=TRUE))
ggplot(data=summary3, aes(summary3$total.steps)) + geom_histogram(col="red", fill="green") + labs(title="Total Number of Steps Taken Each Day") + labs(x="Total Number of Steps", y="Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

Use summary to report the mean and median of the total number of steps taken per day on the new dataset


```r
summary(summary3$total.steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```

## Are there differences in activity patterns between weekdays and weekends?

Using the new dataset, assign “weekday” and “weekend” indicating whether a given date is a weekday or weekend day

Next make a panel plot containing the average number of steps taken, averaged across all weekday days or weekend days per 5-minute interval


```r
data2 <- mutate(data1, weekdays= ifelse(weekdays(as.Date(date)) %in% c('Saturday','Sunday'), "weekend", "weekday"))
data2$weekdays<-factor(data2$weekdays)
by.interval <- group_by(data2, interval, weekdays)
summary4 <- summarise(by.interval, average.steps=mean(steps, na.rm=TRUE))
ggplot(summary4, aes(x=interval, y=average.steps, group=1)) + geom_line()  + geom_line() + xlab("5-Minute Interval") + ylab("Average Number of Steps Taken Across All Days") + ggtitle("Average Number of Steps Taken Across All Days at 5-Minute Interval") + facet_wrap(~weekdays, ncol=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 
