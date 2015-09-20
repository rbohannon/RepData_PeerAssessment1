

DayType <- function(x) {
        
        switch(weekdays(x),
               "Saturday" = ,
               "Sunday" = "weekend",
               "weekday")
}

# *****************************************************************************

# load data
activity <- read.csv("./data/activity.csv", 
                     header=TRUE, 
                     sep=",")

# add new attribute - date as Date object
activity$dDate <- as.Date(activity[,'date'],"%Y-%m-%d")


## What is mean total number of steps taken per day?
# calculate daily totals
stepsPerDay <- aggregate(steps ~ date, data=activity, FUN=sum)

# draw histogram
histActivity <- hist(stepsPerDay$steps, 
                     breaks=10, 
                     col="blue", 
                     main="Total Number of Steps Taken Each Day", 
                     xlab="Steps", 
                     ylab="Days", 
                     xlim=range(0:25000), 
                     ylim=range(0:20))

# calculate mean and median values
dailyMean <- mean(stepsPerDay$steps)
dailyMedian <- median(stepsPerDay$steps)




## What is the average daily activity pattern?
stepsMeanPerInterval <- aggregate(steps ~ interval, data=activity, FUN=mean)

# plot time series
plot(stepsMeanPerInterval, type="l")

# determine which interval has the maximum number of steps
infoMax <- stepsMeanPerInterval[which.max(stepsMeanPerInterval$steps), ]
maxInterval <- infoMax$interval


## Imputing missing values

# number of rows with missing values
# only the first column (steps) has missing values
# apply(activity, 2, function(x) any(is.na(x)))


sum(is.na(activity[,1]))

# impute mean for missing values in steps

# duplicate dataset
activityNoNA <- activity

# loop over duplicate dataset and replace NAs with
# mean for the corresponding interval
for (i in 1:nrow(activityNoNA)) {
        
        # if current row is missing value
        if (is.na(activityNoNA[i, 1])) {
                
                # get interval
                currInterval <- activityNoNA[i, 3]
                
                # retrieve the mean for the interval
                currMean <- stepsMeanPerInterval[
                        which(stepsMeanPerInterval[, 1] == currInterval), 
                        2]
                
                # replace NA with mean
                activityNoNA[i, 1] <- currMean
        } # end if
} # end for

# calculate daily totals
stepsPerDayNoNA <- aggregate(steps ~ date, data=activityNoNA, FUN=sum)

# draw histogram
histActivityNoNA <- hist(stepsPerDayNoNA$steps, 
                         breaks=10, 
                         col="green", 
                         main="Total Number of Steps Taken Each Day (No NAs)", 
                         xlab="Steps", 
                         ylab="Days", 
                         xlim=range(0:25000), 
                         ylim=range(0:25))

# calculate mean and median values
dailyMeanNoNA <- mean(stepsPerDayNoNA$steps)
dailyMedianNoNA <- median(stepsPerDayNoNA$steps)

# compare hist$counts and hist$breaks and find orig==16, new==24 for bin 10,000

## Are there differences in activity patterns between weekdays and weekends?

dayType <- c()

# loop over duplicate dataset and determin if each date is a weekday or weekend
for (i in 1:nrow(activityNoNA)) {
        
        dayType <- c(dayType, DayType(activityNoNA[i, 4]))
}

activityNoNA$dayType <- factor(dayType)

stepsMeanPerIntervalPerDayType <- aggregate(steps ~ interval + dayType, data=activityNoNA, FUN=mean)

steps <- stepsMeanPerIntervalPerDayType$steps
interval <- stepsMeanPerIntervalPerDayType$interval
dayType <- stepsMeanPerIntervalPerDayType$dayType

library(lattice)
xyplot(steps ~ interval | dayType,
       layout=c(1, 2), 
       type="l",
       xlab="Interval",
       ylab="Number of Steps")
