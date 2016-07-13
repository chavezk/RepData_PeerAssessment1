# Reproducible Research: Peer Assessment 1

# Exercise

##Start by reading the data
```{r}
activity <- read.csv("C:/Users/chavezke/Desktop/Coursera/data/activity.csv")
```

## Convert date field from factor to date
```{r}
activity$date <- as.Date(activity$date)
```

##Load reshape2
```{r}
library(reshape2)
```

##Combine activity by date and steps
```{r}
actMeltDate <- melt(activity, id.vars="date", measure.vars="steps", na.rm=FALSE)
```

##Categorize steps by date
```{r}
actCastDate <- dcast(actMeltDate, date ~ variable, sum)
```
##Plot Histogram
```{r}
plot(actCastDate$date, actCastDate$steps, type="h", main="Histogram of Daily Steps", xlab="Date", ylab="Steps per Day", col="blue", lwd=8)
abline(h=mean(actCastDate$steps, na.rm=TRUE), col="red", lwd=2)
```

##Calculate Mean
```{r}
mean(actCastDate$steps, na.rm = TRUE)
```
##Calculate Median
median(actCastDate$steps, na.rm = TRUE)

##Combine data by Interval
```{r}
actMeltInt <- melt(activity, id.vars="interval", measure.vars="steps", na.rm=TRUE)
```

##Categorize data by Interval
```{r}
actCastInt <- dcast(actMeltInt, interval ~ variable, mean)
```

##Plot data
```{r}
plot(actCastInt$interval, actCastInt$steps, type="l", main="Frequency of Steps Taken at Each Interval", xlab="Interval ID", ylab="Steps", col="orange", lwd=2)
```

##Interval with max steps
```{r}
actCastInt$interval[which(actCastInt$steps == max(actCastInt$steps))]
```

##Max interval mean steps
```{r}
max(actCastInt$steps)
```

##Number of NA values
```{r}
sum(is.na(activity$steps))
```

##Remove NA values
```{r}
stepsPerInt <- actCastInt
actNoNA <- activity
actMerge = merge(actNoNA, stepsPerInt, by="interval", suffixes=c(".act", ".spi"))
naIndex = which(is.na(actNoNA$steps))
actNoNA[naIndex,"steps"] = actMerge[naIndex,"steps.spi"]
```

##Combine data by date
```{r}
actMeltDateNoNA <- melt(actNoNA, id.vars="date", measure.vars="steps", na.rm=FALSE)
```

##Categorize steps by date
```{r}
actCastDateNoNA <- dcast(actMeltDateNoNA, date ~ variable, sum)
```

##Plot Histogram of Daily Steps
```{r, echo=false}
plot(actCastDateNoNA$date, actCastDateNoNA$steps, type="h", main="Histogram of Daily Steps (Removed NA Values)", xlab="Date", ylab="Steps", col="gray", lwd=8)
abline(h=mean(actCastDateNoNA$steps), col="red", lwd=2)
```

##Daily steps mean
```{r}
mean(actCastDateNoNA$steps, na.rm=TRUE)
```

##Daily steps median
```{r}
median(actCastDateNoNA$steps, na.rm=TRUE)
```

##Compare weekdays with weekends
```{r}
for (i in 1:nrow(actNoNA)) {
  if (weekdays(actNoNA$date[i]) == "Saturday" | weekdays(actNoNA$date[i]) == "Sunday") {
    actNoNA$dayOfWeek[i] = "weekend"
  } else {
    actNoNA$dayOfWeek[i] = "weekday"
  }
}

##Combine weekday and Weekend data
actWeekday <- subset(actNoNA, dayOfWeek=="weekday")
actWeekend <- subset(actNoNA, dayOfWeek=="weekend")
actMeltWeekday <- melt(actWeekday, id.vars="interval", measure.vars="steps")
actMeltWeekend <- melt(actWeekend, id.vars="interval", measure.vars="steps")
actCastWeekday <- dcast(actMeltWeekday, interval ~ variable, mean)
actCastWeekend <- dcast(actMeltWeekend, interval ~ variable, mean)
```

##Load plotting packages
```{r}
library(ggplot2)
library(gridExtra)
```

##Plots by interval for weekdays and weekends
```{r}
plot1 <- qplot(actCastWeekday$interval, actCastWeekday$steps, geom="line", data=actCastWeekday, main="Steps by Interval - Weekday", xlab="Interval ID", ylab="Number of Steps")
plot2 <- qplot(actCastWeekend$interval, actCastWeekend$steps, geom="line", data=actCastWeekend, main="Steps by Interval - Weekend", xlab="Interval ID", ylab="Number of Steps")
grid.arrange(plot1, plot2, nrow=2)
```
