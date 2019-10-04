---
title: "Reproducible Research Course, Project 1"
author: "Nelly Cattaneo"
date: "04/10/2019"
output: html_document
---

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](http://www.fitbit.com/), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up). These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

-	Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- date: The date on which the measurement was taken in YYYY-MM-DD format

- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


## Loading and preprocessing data

Loading the data from “activity.csv”


```r
data <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
```

Trying to understand the data, first looking at dataset summary with “summary” and “str” methods, and then at the first 6 rows


```r
summary(data)
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

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
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

Converting:
- “date” to a Date classe
- “interval” to a factor


```r
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- factor(data$interval)
```

## Q1: What is mean total number of steps taken per day?
### instructions
For this part of the assignment, you can ignore the missing values in the dataset.

1.	Calculate the total number of steps taken per day
2.	If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

3.	Calculate and report the mean and median of the total number of steps taken per day

### answer
Subsetting the dataset to ignore missing values, and looking at the first 6 rows


```r
NAIndex <- is.na(as.character(data$steps))
dataWithoutNA <- data[!NAIndex,]
head(dataWithoutNA)
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

Aggregating the number of steps taken each day:


```r
# data frame with steps taken each day
stepsByDay <- aggregate(steps ~ date, data = dataWithoutNA, sum)
# set column names
colnames(stepsByDay) <- c("date", "steps")
```

Making a histogram of the total number of steps taken each day:


```r
hist(as.numeric(stepsByDay$steps), breaks = 10, col = "green", xlab = "Steps", ylab = "Days", main= "Total number of steps taken each day")
```

![plot of chunk histStepsEachDay](figure/histStepsEachDay-1.png)

Mean total number of steps taken per day:


```r
#Mean
mean(stepsByDay$steps)
```

```
## [1] 10766.19
```

Median total number of steps taken per day:


```r
#Median
median(stepsByDay$steps)
```

```
## [1] 10765
```

## Q2: What is the average daily activity pattern?
### instructions

1.	Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2.	Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

### answer
Calculating the average number of steps, averaged across all days:


```r
#Calculating the average
stepsPerInterval <- aggregate(dataWithoutNA$steps, FUN=mean, by=list(interval=dataWithoutNA$interval), )

#Adding columns names
colnames(stepsPerInterval) <- c("interval", "average_steps")
```

Making a time series plot of the 5-minute interval and the average number of steps taken


```r
#ploting the average daily activity pattern 
plot(as.integer(levels(stepsPerInterval$interval)), stepsPerInterval$average_steps, type="l",
     xlab = "Interval", ylab = "Average Steps", main = "Average Daily Activity pattern",  col ="green")
```

![plot of chunk plotAverageSteps](figure/plotAverageSteps-1.png)

Computing the 5-minute interval that contains the maximum number of steps:


```r
#finding the maximum number of average steps
maxSteps <- max(stepsPerInterval$average_steps)
maxSteps
```

```
## [1] 206.1698
```

The maximum number of steps is 206.17 (206.1698113, to be more precise)


```r
#finding the 5-minute interval that contains the maximum number of steps
#which.max function: it returns the position of the element with the maximal value in a vector
intervalWithMaxSteps <- stepsPerInterval[which.max(stepsPerInterval$average_steps),]$interval
intervalWithMaxSteps
```

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```

- the 5-minute interval that contains 206.17 steps is 835

## Q3: Imputing missing values
### instructions
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.	Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2.	Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3.	Create a new dataset that is equal to the original dataset but with the missing data filled in.

4.	Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

### answer

#### 3.1.	Calculate and report the total number of missing values in the dataset

For the “steps” variable:


```r
numberNASteps <- sum(is.na(as.character(data$steps)))
numberNASteps
```

```
## [1] 2304
```

For the “date” variable:


```r
numberNADate <- sum(is.na(as.character(data$date)))
numberNADate
```

```
## [1] 0
```

For the “interval” variable:


```r
numberNAInterval <- sum(is.na(as.character(data$interval)))
numberNAInterval
```

```
## [1] 0
```

Total number of missing values in the dataset: 


```r
numberNASteps + numberNADate + numberNAInterval
```

```
## [1] 2304
```


#### 3.2 Devise a strategy for filling in all of the missing values in the dataset.

Missing values will be replaced by the mean of that 5-minute interval


```r
#calculating mean by interval
stepsMean <- aggregate(steps ~ interval, data = data, FUN = mean)

#initializing the vector of results
noNASteps <- numeric()

for (i in 1:nrow(data)) {
    #or each observation
    obs <- data[i, ]
  
    if (is.na(obs$steps)) {
      #if observation steps are null, extract the median value for the interval
      steps <- subset(stepsMean, interval == obs$interval)$steps
    } else {
      #otherwise, keep the steps value
      steps <- obs$steps
    }
    
    # building the vector of results, concatenating the partial result computed above
    noNASteps <- c(noNASteps, steps)
}
```

#### 3.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
#copying the dataset
completeData <- data

#Imputing missing values
completeData$steps <- noNASteps
```



```r
#Checking the complete data with the summary and str methods
summary(completeData)
```

```
##      steps             date               interval    
##  Min.   :  0.00   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   5      :   61  
##  Median :  0.00   Median :2012-10-31   10     :   61  
##  Mean   : 37.38   Mean   :2012-10-31   15     :   61  
##  3rd Qu.: 27.00   3rd Qu.:2012-11-15   20     :   61  
##  Max.   :806.00   Max.   :2012-11-30   25     :   61  
##                                        (Other):17202
```

```r
str(completeData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

#### 3.4	Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

**histogram**
creating the histogram


```r
#Creating a data frame with the steps taken for each day
stepsComplete <- aggregate(steps ~ date, data = completeData, sum)

#Adding column names
colnames(stepsComplete) <- c("date", "steps")

#Making the histogram
hist(as.numeric(stepsComplete$steps), breaks = 10, col = "blue", xlab = "Steps", ylab = "Days", main= "Total number of steps taken each day, complete dataset")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

**Calculate and report the mean and median total number of steps taken per day**

Calculating Mean of the complete dataset


```r
rMeanTotal <- mean(stepsComplete$steps)
rMeanTotal
```

```
## [1] 10766.19
```

Calculating Median of the complete dataset


```r
rMedianTotal <- median(stepsComplete$steps)
rMedianTotal
```

```
## [1] 10766.19
```

**Do these values differ from the estimates from the first part of the assignment?**


```r
rMean <- mean(stepsByDay$steps, na.rm=TRUE)
rMeandiff <- rMeanTotal - rMean
rMeandiff
```

```
## [1] 0
```


```r
rMedian <- median(stepsByDay$steps,na.rm=TRUE)
rMediandiff <- rMedianTotal - rMedian
rMediandiff
```

```
## [1] 1.188679
```

The mean is not impacted and the median does have a small variance.

**What is the impact of imputing missing data on the estimates of the total daily number of steps?**

A comparison of the two histograms shows that the impact of imputting missing data is bigger on the 10000 - 12000 step interval.

## Q4: Are there differences in activity patterns between weekdays and weekends?
### instructions
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.	Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

2.	Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

### answer

#### 4.1	Create a new factor variable in the dataset indicating whether a given date is a weekday or weekend day

```r
#Creating a  factor variable "day "to store the day of the week:
completeData$day <- as.factor(weekdays(completeData$date))

#Creating a logical variable "is_weekday" (weekday=TRUE, weekend = FALE) :
completeData$isWeekday <- ifelse(!(completeData$day %in% c("Saturday","Sunday")), TRUE, FALSE) 
#Calculating the average number of steps for weekdays
weekdaysData <- completeData[completeData$isWeekday,]
stepsPerIntervalWeekdays <- aggregate(weekdaysData$steps, by=list(interval=weekdaysData$interval), FUN=mean)

#Calculating the average number of steps for weekends
weekendsData <- completeData[!completeData$isWeekday,]
stepsPerIntervalWeekends <- aggregate(weekendsData$steps, by=list(interval=weekendsData$interval), FUN=mean)

#Adding columns names
colnames(stepsPerIntervalWeekdays) <- c("interval", "average_steps")
colnames(stepsPerIntervalWeekends) <- c("interval", "average_steps")

#Adding a column for the day
stepsPerIntervalWeekdays$day <- "Weekday"
stepsPerIntervalWeekends$day <- "Weekend"

#Merging the two
weekData <- rbind(stepsPerIntervalWeekends, stepsPerIntervalWeekdays)

#Converting the day variable to a factor
weekData$day <- as.factor(weekData$day)
```


#### 4.2	Making the plot


```r
library(lattice)
xyplot(average_steps ~  interval | day, data = weekData, layout = c(1,2), type ="l", ylab="Number of Steps")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)
