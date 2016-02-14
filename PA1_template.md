# Reproducible Research: Peer Assessment 1
Daniel Romero  


## Loading and preprocessing the data

1. Loading data...

```r
setwd("C:/Users/Becario/Dropbox/Cursos/Coursera/Reproducible Research/repdata-data-activity")
activity <- read.csv("activity.csv")
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
ActivityPerDay <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
summary(ActivityPerDay)
```

```
##          date        steps      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day



```r
hist(ActivityPerDay$steps, col = "red", main='Histogram of the Activity per Day')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

**Response:** Total number of steps taken per day is the sum of the total steps corresponding to all the intervals related to a particular day. The histogram in the figure  above shows the frequency of the total steps per day grouped by intervals of 5000 steps.

2. Calculate and report the mean and median total number of steps taken per day


```r
Mean <- mean(ActivityPerDay$steps)
print(Mean)
```

```
## [1] 10766.19
```


```r
Median <- median(ActivityPerDay$steps)
print(Median)
```

```
## [1] 10765
```

**Response:** The *mean* of steps taken per day is 10766.19 steps and the *median* 10765 steps.

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
StepsByInterval <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
plot(steps ~ interval, data = StepsByInterval, type = "l", col ='blue', main=" Averaged number of steps every 5-min intervals", 
     xlab = "No. of Intervals of 5-min",
     ylab = "Averaged steps", lwd = 3)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
StepsByInterval$interval[which.max(StepsByInterval$steps)]
```

```
## [1] 835
```

**Response:** The 5-minute interval with the maximum number of steps is the 835-*th* one.



## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
summary(activity$steps)[7]
```

```
## NA's 
## 2304
```

**Response:** The total missing values in the *steps* variable  is 2304.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
  
  **Response:** For filling all the missing values in the *steps* variable  the  strategy I used consited of replacing the NA's by the median of the same 5-min intervals among all days. 
  
  
 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
MedianPerInterval <- aggregate(steps ~ interval, data = activity, median, na.rm = TRUE)
NewActivityData <- activity

for (n_day in 1:nrow(ActivityPerDay)){
  END <-   n_day*nrow(MedianPerInterval)
  START <- END - nrow(MedianPerInterval) + 1
  con <-0
  for (n_inter in START:END){
    con <- con + 1
    if (is.na(activity$steps[n_inter])){
       NewActivityData$steps[n_inter] <-  MedianPerInterval$steps[con]
    }
  }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
ActivityPerDay2 <- aggregate(steps ~ date, data = NewActivityData, sum)
hist(ActivityPerDay2$steps, col = "red", main='Histogram of the Activity per Day')
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 


```r
Mean <- mean(ActivityPerDay2$steps)
print(Mean)
```

```
## [1] 9643.25
```


```r
Median <- median(ActivityPerDay2$steps)
print(Median)
```

```
## [1] 10417
```

**Response:** The *mean* of steps taken per day is 9643.25 steps and the *median* 10417 steps.

5. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

**Response:** Yes, they do. Both the mean and median values are smaller than the ones of the original dataset after imputing missing data because it was put the median value for each particular 5-min interval. The median value represents the value separating the higher half (50% of the total samples) and the lower half of the sample data being in more robust that the mean in the presence of atypical measures (outliers).

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

**Response:** I labeled the new factor variable as follows:

 + Sundays and Saturdays: "weekend",
 
 + Otherwise: "weekday"


```r
NewActivityData$dayType = weekdays(as.Date(NewActivityData$date))
WEEKEND <- c('sábado', 'domingo')
NewActivityData$dayType <- factor(NewActivityData$dayType %in% WEEKEND, 
                   levels=c(FALSE, TRUE), labels=c('weekday','weekend') )
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:



```r
StepsByIntervalandDay = aggregate(steps ~ interval + dayType, NewActivityData, mean)
library(lattice)
xyplot(steps ~ interval | factor(dayType), data = StepsByIntervalandDay, aspect =1/2,type = "l", col= "red", lwd = 2, xlab= "5-min interval", ylab= "Averaged steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 
