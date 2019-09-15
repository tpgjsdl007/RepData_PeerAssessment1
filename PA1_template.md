---
title: "PA1_template.Rmd"
author: "Saehun Kwak"
date: "9/15/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)
```
## Loading and preprocessing the data
```{r, echo=TRUE}
activity <- read.csv("activity.csv")

activity$date <- as.Date(activity$date, "%Y-%m-%d")
activity <- as.data.frame(activity)
```

## What is mean total number of steps taken per day?
# Plots a histogram for step total by day using ggplot2

```{r histogram, fig.height = 4, echo=FALSE}
library(ggplot2)
plot1 <- ggplot(data = na.omit(activity), aes(date, steps)) + stat_summary(fun.y = sum, geom = "bar")
plot1
```

#Calculates mean and median of steps take by day
```{r, echo=TRUE}
steps <- with(activity, tapply(steps, date, sum, na.rm = TRUE))
mean(steps)
median(steps)
```


## What is the average daily activity pattern?
# Using tapply, the means for each interval here are calculated across days.
```{r, echo=TRUE}
daymeans <- with(na.omit(activity), tapply(steps, interval, mean))
head(daymeans)
```

```{r plot, fig.height = 4, echo=FALSE}
plot(daymeans, type = "l", xaxt = "n", xlab = "Minutes in a Day", ylab = "Average Steps")
axis(1, at=seq_along(daymeans), labels = names(daymeans))
```

# This code uses a logical vector to select the maximum value for the mean steps across intervals. The 835th interval gives the maximum value. 
```{r, echo = TRUE}
daymeans[which(daymeans == max(daymeans))]
```

# The following sums the na values and then reports the ratio of NA/total observations.

```{r, echo = TRUE}
library(scales)
sum(is.na(activity))
percent(sum(is.na(activity))/nrow(activity))
```


## Imputing missing values
# My strategy for replacing NAs is to replace each NA value by steps mean per interval. This is accomplished by using a nested for loop to identify the respective interval of the row then replace the NA value with the mean for that interval.

# For demonstration purposes, the head and the tail of the data both contain NAs.
```{r, echo = TRUE}
head(activity)
tail(activity)
```

# The int and len variable are set up to manage the for loop sequences. The NAin and NA steps variables are sections of the data that will be used to replace the NA data after the loop.

```{r, echo = TRUE}
int <- unique(activity$interval)
len <- nrow(activity[is.na(activity),])
NAint <-  activity[is.na(activity),3]
NAsteps <- activity[is.na(activity),1]
for (j in 1:2304) {
  for (i in 1:288){
    if (NAint[j] == int[i])
      NAsteps[j] <- daymeans[i]
    
  }
}
NAindex <- is.na(activity$steps)
activity$steps<- replace(activity$steps,NAindex, NAsteps)
```

# As can be seen here, the NAs have been replaced by the relevant mean for the 5 minute interval.
```{r, echo = TRUE}
head(activity)
tail(activity)
```

# Below is an histogram with the updated data.
```{r hist, fig.height = 4, echo=FALSE}
plot2 <- ggplot(data = activity, aes(date, steps)) + stat_summary(fun.y = sum, geom = "bar")
plot2
```
# Calculates mean and median of steps take by day
```{r, echo=TRUE}
steps <- with(activity, tapply(steps, date, sum, na.rm = TRUE))
mean(steps)
median(steps)
```
# Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
  
#  Yes they are changed, because the NAs  were replaced based on the interval means, the mean and the median for the day now match up.



## Are there differences in activity patterns between weekdays and weekends?
# First, this code creates a new factor variable.
```{R Weekdays}
library(dplyr)
activity <- mutate(activity, day = weekdays(activity$date))
weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
activity$day <- factor((weekdays(activity$date) %in% weekdays), 
                       levels=c(FALSE, TRUE), labels=c('Weekend', 'Weekday'))
```

```{r, echo=TRUE}
weekdays <- subset(activity, day == "Weekday")
weekends <- subset(activity, day == "Weekend")
weekendmeans <- with(weekends, tapply(steps, interval, mean))
weekdaymeans <- with(weekdays, tapply(steps, interval, mean))
```

```{r plot3, fig.height = 6, echo=FALSE}
par(mfrow=c(2,1))
par(mar=c(5,2,2,2))
plot(weekdaymeans, type = "l", xaxt = "n", xlab = "Minutes in a Day", ylab = "Average Steps")
axis(1, at=seq_along(weekdaymeans), labels = names(weekdaymeans))
plot(weekendmeans, type = "l", xaxt = "n", xlab = "Minutes in a Day", ylab = "Average Steps")
axis(1, at=seq_along(weekendmeans), labels = names(weekendmeans))
```
