activity<-read.csv("activity.csv")

stepsPerDay<- aggregate(activity$steps,by=list(activity$date),FUN=sum,na.rm = TRUE)
colnames(stepsPerDay)<-c("date","steps")
hist(stepsPerDay$steps)

stepsPerDayMean<-mean(stepsPerDay$steps)
stepsPerDayMean
stepsPerDayMedian<-median(stepsPerDay$steps)
stepsPerDayMedian

stepsPerInterval<-aggregate(activity$steps,by=list(activity$interval), FUN=mean,na.rm = TRUE)
colnames(stepsPerInterval)<-c("interval","steps")
plot(stepsPerInterval$interval,stepsPerInterval$steps,type = "l")

MaxstepsPerInterval<-stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
MaxstepsPerInterval

missingValue<-sum(is.na(activity$steps))
missingValue
Mean<-mean(activity$steps,na.rm=TRUE)
Mean
for(i in nrow(activity)){
if(is.na(activity[i,]$steps)==TRUE){activity[i,]$steps<-Mean}
}

stepsPerDayWithNA<- aggregate(activity$steps,by=list(activity$date),FUN=sum)
colnames(stepsPerDayWithNA)<-c("date","steps")
hist(stepsPerDayWithNA$steps)

activity$date <- as.Date(activity$date, format="%Y-%m-%d")
activity$day<-weekdays(activity$date)
for (i in 1:nrow(activity)) {
  if (activity[i,]$day=="ÐÇÆÚÁù"|activity[i,]$day=="ÐÇÆÚÈÕ") {
    activity[i,]$day<-"weekend"
  }
  else  {
    activity[i,]$day<-"weekday"
  }
}
Averagesteps<- aggregate(activity$steps,by=list(activity$day,activity$interval), FUN=mean,na.rm = TRUE)
colnames(Averagesteps)<-c("day","interval","steps")
install.packages("lattice")
library(lattice)
xyplot(steps ~ interval | day, Averagesteps, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Steps")

