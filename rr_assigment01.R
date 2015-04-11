
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
library(pubridate)
str(data)

steps_by_day <- data %>% group_by(date) %>% summarise(steps = sum(steps))
steps_by_day$date <- as.Date(steps_by_day$date)
hist(steps_by_day$steps)

