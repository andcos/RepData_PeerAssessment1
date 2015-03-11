
##================================
## Reproducible Research
## PA 1: R Markdown files
## Date:10.03.20
## Author:AC
##================================


library(data.table)
library(knitr)
library(plyr)
library(dplyr)
library(lubridate)
library(zoo)
library(lattice)

## Load the data (i.e. read.csv())
setwd("C:/DATA/COURSERA/05_REPRODUCIBLE/PA1")
data<-read.csv('activity.csv')

##Process/transform the data (if necessary) 
##into a format suitable for your analysis

##=============
## PART 1
##=============


## Calculate the total number of steps taken per day
totalSteps_perDay<-aggregate(steps~day(data$date),data,sum,na.rm = TRUE)
colnames(totalSteps_perDay) <- c("day", "Total_steps_per_day")

## Make a histogram of the total number of steps taken each day
hist(totalSteps_perDay$Total_steps_per_day,
     col = "red", xlab="day",
     main="Total number of steps per day")

## What is mean total number of steps taken per day?
meanStepsPerDay<-aggregate(Total_steps_per_day~day,totalSteps_perDay,mean)
colnames(meanStepsPerDay) <- c("day", "mean_steps_per_day")
medianStepsPerDay<-aggregate(Total_steps_per_day~day,totalSteps_perDay,median)
colnames(medianStepsPerDay) <- c("day", "median_steps_per_day")

print("mean value of the steps on all days")
mean(data$steps,na.rm=TRUE)
print("median value of the steps on all days")
median(data$steps,na.rm=TRUE)


##=================
## PART 2
##=================
## What is the average daily activity pattern?

## Make a time series plot (i.e. type = "l") 
##of the 5-minute interval (x-axis) and the average number of
##steps taken, averaged across all days (y-axis)
ts<-aggregate(steps~interval,data,mean,na.rm=TRUE)

plot(ts$interval,ts$steps,type="l",
     main="Avarage daily activity pattern",
     xlab="5-minute interval",
     ylab="no of steps taken",
     col="blue")

## Which 5-minute interval, on average across all the days in 
## the dataset, contains the maximum number of steps?

max(ts$steps)
print(value coresponding--)
which.max(ts$steps)


##=================
## PART 3
##================= 
  
## Imputing missing values

## Calculate and report the total number of missing 
##values in the dataset (i.e. the total number of rows with NAs)

print(missing values in data)
data_na<-sum(is.na(data))
cdata_na<-which(is.na(data))



##Devise a strategy for filling in all of the missing values
##in the dataset. The strategy does not need to be sophisticated.
##For example, you could use the mean/median for that day, or the
##mean for that 5-minute interval, etc.

## Create a new dataset that is equal to the original dataset
##but with the missing data filled in.

## for this calculation I use the zoo library

## data1 is the new data set
data1<-zoo(data)
data1<-na.locf(data1,from.LAST=TRUE)
sum(is.na(data1))
data1<-na.fill(data1, c("extend", NA))
sum(is.na(data1))
class(data1)
## transforming the data1 from zoo class to data.frame
data1<-data.frame(data1)
data1<-data.table(data1)

##Make a histogram of the total number of steps taken each day 
##and 

## this is working but it is not great
totalSteps_perDay1<-as.data.frame(tapply(as.numeric(data1$steps), as.numeric(day(data1$date)), sum))
## write the days in the same data.frame
totalSteps_perDay1[[2]]<-unique(day(data1$date))

colnames(totalSteps_perDay1) <- c( "Total_steps_per_day","day")
hist(totalSteps_perDay1$Total_steps_per_day,
     col = "red", xlab="day",
     main="Total number of steps per day")


## Calculate and report the mean and median total number of 
## steps taken per day. 

meanStepsPerDay1<-aggregate(Total_steps_per_day~day,totalSteps_perDay1,mean)
colnames(meanStepsPerDay1) <- c("day", "mean_steps_per_day")
medianStepsPerDay1<-aggregate(Total_steps_per_day~day,totalSteps_perDay1,median)
colnames(medianStepsPerDay1) <- c("day", "median_steps_per_day")


##Do these values differ from the estimates
##from the first part of the assignment? 

plot( meanStepsPerDay1$day,
    meanStepsPerDay1$mean_steps_per_day,
     col="red",
     type="l",
     main="Mean number of Steps per day",
     ylab="number of steps per day",
     xlab="days")
lines(meanStepsPerDay$day,
      meanStepsPerDay$mean_steps_per_day,
      col="green")
legend("topleft", 
       legend = c("new", "old"),
       lty=c(1,1),
       lwd=c(2.5,2.5),col=c("red","green"))


## What is the impact of 
##imputing missing data on the estimates of the total daily
##number of steps?

## The result will be more accurate
## accuracy measured in mean number of steps

accuracy<-meanStepsPerDay$Total_steps_per_day[1:30]-meanStepsPerDay1$Total_steps_per_day[1:30]


##=================
## PART 4
##=================

## Are there differences in 
## activity patterns between weekdays and weekends?

## Use the dataset with 
## the filled-in missing values for this part.
data1[[4]]<-weekdays(day(data1$date))
data1[[5]]<-"empty"

data1$V5[data1$V4 == "Mon"] <- "Weekday"
data1$V5[data1$V4 == "Tue"] <- "Weekday"
data1$V5[data1$V4 == "Wed"] <- "Weekday"
data1$V5[data1$V4 == "Thu"] <- "Weekday"
data1$V5[data1$V4 == "Fri"] <- "Weekday"
data1$V5[data1$V4 == "Sat"] <- "Weekend"
data1$V5[data1$V4 == "Sun"] <- "Weekend"

##Make a panel plot containing a time series plot 
##(i.e. type = "l") of the 5-minute interval (x-axis)
##and the average number of steps taken, averaged across 
##all weekday days or weekend days (y-axis). See the README 
##file in the GitHub repository to see an example of what this
##plot should look like using simulated data.

meansteps <- aggregate(as.numeric(data1$steps), by = list(data1$interval, data1$V5), mean)
names(meansteps) <- c("interval", "dtype", "steps")

x.tick.number <- 15
at <- seq(1, nrow(meansteps), length.out=x.tick.number)
labels <- round(seq(0, 2000, length.out=x.tick.number))

xyplot(steps~interval|dtype, 
       data=meansteps, type='l', layout=c(1, 2),
       scales=list(y=list(tick.number=10), 
                   x=list(at=at, labels=labels)))




