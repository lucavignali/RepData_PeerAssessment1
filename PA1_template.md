# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data
The data to be analyzed is included in the archive activity.zip.The full data set is loaded and a copy of it is created without the missing values = NA. Both datasets, with and without missing values, are needed to answer the following questions.


```r
if(!file.exists("activity.zip")){
        stop("activity.zip file not present")
}
unzip("activity.zip")
All_Steps <- read.csv("activity.csv", colClasses = c("integer", "Date", "integer"))
OK_Steps <- na.omit(All_Steps)
```

As an example the first rows of the cleaned data frame are as follows:


```r
OK_Steps[1:5,]
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
```


## What is mean total number of steps taken per day?

Using the OK_Steps that is not including NA values, the number of steps per day over the two months observation are showed in the following picture



```r
Day_Steps <- with(OK_Steps,aggregate(steps, by = list(date), sum))
names(Day_Steps) <- c("date", "steps")
Mean_Steps <- as.integer(mean(Day_Steps$step))
Median_Steps <- as.integer(median(Day_Steps$step))

library(ggplot2)
g <- ggplot(Day_Steps, aes(date,steps)) + geom_bar(stat="identity")
g <- g + labs(title="Number of Steps per day")
g <- g + geom_hline(yintercept = c(Mean_Steps,Median_Steps), color = "blue", size = 1)
plot(g)
```

![](PA1_template_files/figure-html/Steps_per_day-1.png) 

The picture is also showing in blue the median = 10765 and mean = 10766 number of steps per day over the two months observation period. 




## What is the average daily activity pattern?
Moving to the 5 minutes data granularity, the activity pattern over the 2 months observation period, is reported in the following graph.


```r
Mean_Step_5 <- as.integer(mean(OK_Steps$steps))
iMax_Step_5 <- which.max(OK_Steps$steps)
Max_Step <- OK_Steps[iMax_Step_5,]

g <- ggplot(OK_Steps, aes(date,steps)) + geom_line()
g <- g + labs(title = "Number of Steps per 5 minutes")
g <- g + geom_hline(yintercept = Mean_Step_5, color = "blue", size = 1)
g <- g + geom_point(data=Max_Step, aes(date,steps), color = "red", size = 4)
plot(g)
```

![](PA1_template_files/figure-html/Steps_5_min-1.png) 

The picture is also showing in blue the mean = 37 number of steps every 5 minutes over the two months observation period. 

Finally the maximum number of steps made in 5 minutes period is equal to 806 and occurred on the 2012-11-27 at time interval 615, corresponding to the red point in the previous figure.




## Imputing missing values
Now let's evalute the impact of missing values on previously calculated mean and median per day.


```r
iNA_Steps <- (is.na(All_Steps$steps))
NA_Steps <- sum(iNA_Steps)
Full_Steps <- All_Steps
Full_Steps[iNA_Steps,"steps"] <- Mean_Step_5

Full_Day_Steps <- with(Full_Steps,aggregate(steps, by = list(date), sum))
names(Full_Day_Steps) <- c("date", "steps")
Full_Mean_Steps <- as.integer(mean(Full_Day_Steps$step))
Full_Median_Steps <- as.integer(median(Full_Day_Steps$step))

g <- ggplot(Full_Day_Steps, aes(date,steps)) + geom_bar(stat="identity")
g <- g + labs(title="Number of Steps per day")
g <- g + geom_hline(yintercept = c(Full_Mean_Steps,Full_Median_Steps), color = "blue", size = 1)
plot(g)
```

![](PA1_template_files/figure-html/Missing_Values-1.png) 

The number of NA value in the unnprocessed data set, is equal to 2304.
In order to evaluate the impact of NA on calculated data, we will substitute NA with the previously calulated Mean_Step_5, equal to 37. 

The mean and median steps per day calculated with imputed missing values, are equal to 10751 and 10656 respectively, that are different from previously calcualted mean and median values.

THe impact of the imputed missing values is to decrease both mean value from 10766 to 10751 and median value from 10765 to 10656.





## Are there differences in activity patterns between weekdays and weekends?
