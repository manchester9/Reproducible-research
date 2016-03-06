# Assignment_1

# Define image sizes
img.width <- 450
img.height <- 300
options(RCHART_HEIGHT = img.height, RCHART_WIDTH = img.width)
opts_chunk$set(fig.width=6, fig.height=4)

library(knitr)
library(ggplot2)
library(scales)
library(Hmisc)
library(dplyr)
library(lubridate)
library(lattice)

# *******************
# Stage 1: Loading and preprocessing the data
if (!file.exists("data")) {dir.create("data")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfile <- "./data/activity.zip"

download.file(fileUrl, destfile = destfile)
dateDownloaded <- date()

dataset <- read.csv(unz("./data/activity.zip", "activity.csv"))

dim(dataset) # 17,568 * 3
colnames(dataset) #steps date interval
str(datasets) # int, fact, int

# *******************
# Stage 2: What is the mean total number of steps taken per day
steps_by_day <- tapply(dataset$steps, list(dataset$date), FUN = sum) 

hist(steps_by_day, 
	breaks = 10, 
    xlab = " ",
    ylab = "Frequency",
    main = "Distribution of total steps per day", 
    col = "blue")

# Mean and median steps by day
steps_by_day_mean <- mean(steps_by_day, na.rm = TRUE) # 10,766
steps_by_day_median <- median(steps_by_day, na.rm = TRUE) # 10,765

# *******************
# Stage 3: What is the average daily activity pattern
avg_steps_per_block <-aggregate(data = dataset, 
	                  steps~interval, 
	                  FUN = "mean", 
	                  na.action = na.omit)

ggplot(data = avg_steps_per_block, aes(x = interval, y = steps)) + 
	   geom_line(color = "steelblue", size = 0.8) + 
	   labs(title = "Time Series of 5-minute interval", x = "5-minute intervals", y = "Average number of steps taken")

max_interval <- avg_steps_per_block[avg_steps_per_block$steps==max(avg_steps_per_block$steps),]
max_interval

# *******************
# Stage 4: Inputting missing values

# Number of missing values
number_of_missing_values <- length(which(is.na(dataset$steps))) # 2,304

# Using the mean to impute
step_values <- data.frame(dataset$steps)
step_values[is.na(step_values),] <- ceiling(tapply(X = dataset$steps,
	                                               INDEX = dataset$interval,
	                                               FUN = "mean",
	                                               na.rm = TRUE))
new_dataset <- cbind(step_values, dataset[,2:3])
colnames(new_dataset) <- c("steps", "date", "interval")

steps_by_day_new_dataset <-aggregate(data = new_dataset, 
                    steps~date, 
                    FUN = "sum", 
                    na.action = na.omit)
colnames(steps_by_day_new_dataset)

ggplot(steps_by_day_new_dataset, aes(date, steps)) + 
	   geom_bar(stat = "identity", colour = "blue", fill = "blue", width = 0.7) + 
       labs(title = "Steps taken each day without missing values", x = "Date", y = "Total number of steps")

steps_by_day_new_dataset_mean <- mean(steps_by_day_new_dataset$steps) # 10,785
steps_by_day_new_dataset_median <- median(steps_by_day_new_dataset$steps) # 10,909

# *******************
# Stage 5: Are there differences in activity patterns between weekdays and weekends?
new_dataset$Weekday<-wday(new_dataset$date, label = TRUE, abbr = FALSE)
new_dataset <- new_dataset %>%
mutate(day_of_week = ifelse(Weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

# table(new_dataset$Weekend, new_dataset$Weekday)

avg_new_dataset <- aggregate(steps ~ interval + 
							 day_of_week, 
							 data=new_dataset, FUN = "mean")

# Creating a panel plot with the lattice library
xyplot(avg_new_dataset$steps ~ avg_new_dataset$interval | avg_new_dataset$day_of_week, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")

