
## Loading and preprocessing the data  

data=read.csv("activity.csv")
data$date=as.Date(as.character(data$date))
summary(data)
head(data)

## What is mean total number of steps taken per day?
#Total steps taken on each day**

byStep=aggregate(data$steps,by=list(data$date),sum)
names(byStep)=c("Date","TotalSteps")
head(byStep)

#Histogram of the total number of steps taken each day**

hist(byStep$TotalSteps,breaks = 25,col = "steelblue",border = "white",
     main = "Total number of Steps per Day", xlab = "Total Steps",ylim = c(0,20))


#Mean and median of the total number of steps taken per day**

mean=round(mean(byStep$TotalSteps,na.rm=TRUE),2)
median=round(median(byStep$TotalSteps,na.rm = TRUE),2)

## What is the average daily activity pattern?

#Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

byInterval=aggregate(na.omit(data)$steps,by=list(na.omit(data)$interval),mean)
names(byInterval)=c("Interval","Mean.Steps")

plot(x=byInterval$Interval,y=byInterval$Mean.Steps,type = "l",col =10,lwd=2,main ="Average of Steps accross all Days",xlab="Interval",ylab = "Step")


#The 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps**

byInterval[byInterval$Mean.Steps==max(byInterval$Mean.Steps),]

## Imputing missing values
#Total number of missing values in the dataset (i.e. the total number of rows with NAs)**

sapply(X=data,FUN = function(x) sum(is.na(x)))

#New dataset that is equal to the original dataset but with the missing data filled in.**

 complete.data=data
for (i in c(which(is.na(data$steps)))) {
  complete.data$steps[i]=byInterval[which(byInterval$Interval==data$interval[i]),2]
}
 sapply(X=complete.data,FUN = function(x) sum(is.na(x)))


#Histogram of the total number of steps taken each day**

byStep.new=aggregate(complete.data$steps,by=list(complete.data$date),sum)
names(byStep.new)=c("Date","TotalSteps")
hist(byStep.new$TotalSteps,breaks = 25,col = "steelblue",border = "white",
     main = "Total number of Steps per Day", xlab = "Total Steps",
     ylim = c(0,20))
```

# Any differences in mean and median between original data and data with NAs replaced?

newmean=round(mean(byStep.new$TotalSteps),2)
newmedian=round(median(byStep.new$TotalSteps),2)
par(mfrow=c(1,2))
hist(byStep$TotalSteps,breaks = 25,col = "steelblue",border = "white",
     main=NULL,xlab = "Total Steps per day before fill NAs",ylim = c(0,20))
hist(byStep.new$TotalSteps,breaks = 25,col = "steelblue",border = "white",
     main=NULL,xlab = "Total Steps per day after fill NAs",
     ylim = c(0,20))

## Are there differences in activity patterns between weekdays and weekends?

#New factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day**

new.data=complete.data
new.data$day.of.week=ifelse(weekdays(as.Date(new.data$date)) %in% c("Saturday","Sunday"),
                     "weekend","weekday")
new.data=aggregate(new.data$steps,by=list(new.data$interval,new.data$day.of.week),mean)
names(new.data)=c("Interval","day.of.week","Mean.Steps")
head(new.data)


# Panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)**

library(ggplot2)
ggplot(new.data,aes(x=Interval,y=Mean.Steps,color=day.of.week))+
  geom_line()+
  facet_grid(day.of.week~.)+
  labs(title="Means of Steps by Interval", x="Interval",y="Mean Steps")
