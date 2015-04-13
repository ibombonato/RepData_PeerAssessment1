setwd("C:\\Users\\icaro.MINHAVIDA\\Source\\Repos\\RepData_PeerAssessment1")
## Loading and preprocessing the data
zippedFile <- "activity.zip"
unzip(zippedFile)
fileName <- "activity.csv"
data <- read.csv(fileName)

## What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# Make a histogram of the total number of steps taken each day
# Calculate and report the mean and median total number of steps taken per day
library(dplyr)
library(lubridate)

steps_by_day <- data %>% na.omit() %>% group_by(date) %>% summarise(steps = sum(steps))
hist(steps_by_day$steps, main = "Histogram of steps per day", xlab="Steps")

mean(steps_by_day$steps, na.rm = TRUE)
median(steps_by_day$steps, na.rm = TRUE)

# What is the average daily activity pattern?
# 
# Make a time series plot (i.e. type = "l") of the 
# 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all days (y-axis)

steps_by_interval <- data %>% na.omit() %>% group_by(interval) %>% summarise(stepsMean = mean(steps))
plot(x = steps_by_interval$interval, y = steps_by_interval$stepsMean, 
     type = "l" , main = "Average steps by interval(All days)",
     ylab = "Mean of Steps",
     xlab = "Interval")

# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
abline(v = topIntervalValue, col = "red", lty = 3)
topIntervalValue <- filter(steps_by_interval, stepsMean == max(stepsMean)) %>% select(interval)
axis(1, at=836,labels=836, col.axis="red", cex.axis=0.7, tck=-.03)

# Imputing missing values
# 
# Note that there are a number of days/intervals where there are missing 
# values (coded as NA).
# The presence of missing days may introduce bias into some calculations
# or summaries of the data.
# 
# Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with NAs)

totalNA <- sum(!complete.cases(data))

# Devise a strategy for filling in all of the missing values in the dataset.
# The strategy does not need to be sophisticated. 
# For example, you could use the mean/median for that day, or the mean for 
# that 5-minute interval, etc.
missingData <- data[!complete.cases(data),]
for (i in 1:nrow(missingData)){
    missingData[i, 1] <- steps_by_interval %>% 
        filter(interval == missingData[i,3]) %>% 
        select(stepsMean)    
}

# Create a new dataset that is equal to the original dataset but with the missing 
# data filled in.

df <- rbind(data[complete.cases(data),], missingData)

# Make a histogram of the total number of steps taken each day and Calculate and 
# report the mean and median total number of steps taken per day. 

steps_by_day_complete <- df %>% na.omit() %>% group_by(date) %>% summarise(steps = sum(steps))
hist(steps_by_day_complete$steps, main = "Histogram of steps per day", xlab="Steps")

mean(steps_by_day_complete$steps, na.rm = TRUE)
median(steps_by_day_complete$steps, na.rm = TRUE)

# Do these values differ from the estimates from the first part of the assignment?
opar <- par()
par(opar)
par(mfrow = c(1,2))
steps_by_day <- data %>% na.omit() %>% group_by(date) %>% summarise(steps = sum(steps))
hist(steps_by_day$steps, ylim = c(0, 40), xlab = "Steps", main = "Original values")

steps_by_day_complete <- df %>% na.omit() %>% group_by(date) %>% summarise(steps = sum(steps))
hist(steps_by_day_complete$steps, ylim = c(0, 40), xlab = "Steps", main = "Replaced NAs")

mean(steps_by_day$steps, na.rm = TRUE)
median(steps_by_day$steps, na.rm = TRUE)

mean(steps_by_day_complete$steps, na.rm = TRUE)
median(steps_by_day_complete$steps, na.rm = TRUE)

# What is the impact of imputing missing data on the estimates of the total daily 
# number of steps?

#R: None, since the mean and median are the same as in the original data


# Are there differences in activity patterns between weekdays and weekends?
# 
# For this part the weekdays() function may be of some help here. 
# Use the dataset with the filled-in missing values for this part.
# 
# Create a new factor variable in the dataset with two levels 
# -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
# 
# Make a panel plot containing a time series plot (i.e. type = "l") of 
# the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis). 
df$date <- ymd(df$date)

df <- mutate(df, weekday = 
                 ifelse(wday(date)  %in% c(1,2), 
                        yes = "weekend", 
                        no = "weekday"
            ))
str(df)
df$weekday = as.factor(df$weekday)
head(df)
group_by_weekday <- df %>% select(steps, interval, weekday) %>% 
    group_by(weekday, interval) %>% summarise(steps = mean(steps))

dfWeekend <- filter(group_by_weekday, weekday == "weekend")
dfWeekday <- filter(group_by_weekday, weekday == "weekday")

par(mfcol = c(2, 1), mar=c(2, 2, 2, 2) + 0.1)
plot(x = dfWeekend$interval, y = dfWeekend$steps, 
     type = "l" , main = "Average steps by interval(Weekend)",
     ylab = "Number of steps",
     xlab = "Interval")
plot(x = dfWeekday$interval, y = dfWeekday$steps, 
     type = "l" , main = "Average steps by interval(Weekday)",
     ylab = "Number of teps",
     xlab = "Interval")

