Loading and preprocessing the data
==================================

```{r, echo=TRUE}
new_data<-read.csv('activity.csv')

new_data[,2]<-as.Date(as.character(new_data[,2]))

new_dataclean<-new_data[with(new_data,!is.na(new_data$steps)),]
```

What is mean total number of steps taken per day?
=================================================

```{r, echo=TRUE}
daily_steps<-tapply(new_dataclean$steps,new_dataclean$date,sum,na.rm=TRUE)

barplot(daily_steps)

mean(daily_steps)
median(daily_steps)
```

What is the average daily activity pattern?
===========================================

```{r, echo=TRUE}
interval_steps<-tapply(new_dataclean$steps,new_dataclean$interval,mean,na.rm=TRUE)
intervals<-new_data$interval[1:288]
plot(intervals,interval_steps,type='l', xlab = '5-minutes interval',
     ylab = 'average steps across all days')

joined_data<-data.frame(intervals,interval_steps)
joined_data[with(new_dataclean, interval_steps==max(interval_steps)),]
```

Imputing missing values
=======================

```{r,echo=TRUE}
new_data2<-new_data
sum(is.na(new_data2$steps))

interval_steps2<-tapply(new_dataclean$steps,new_dataclean$interval,mean,na.rm=TRUE)

means<-as.numeric(rep(interval_steps2,288))

for(i in 1:17568){
  if(is.na(new_data2[i,1])){
    new_data2[i,1]<-means[i]
  }
  
}

daily_steps2<-tapply(new_data2$steps,new_data2$date,sum,na.rm=TRUE)

barplot(daily_steps2)

mean(daily_steps2)
median(daily_steps2)
```

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

```{r,echo=TRUE}
new_data2$days<-weekdays(new_data2$date)

for(i in 1:17568){
  if(new_data2[i,4]=='Saturday'){
    new_data2[i,4]='Weekend'
  }
  else if(new_data2[i,4]=='Sunday'){
    new_data2[i,4]='Weekend'
  }
  else new_data2[i,4]='Weekday'
}
new_data2$days=as.factor(new_data2$days)

head(new_data2)

new_data2<-aggregate(data=new_data2, steps~interval+days, mean)
library(lattice)
xyplot(steps~interval | days, data=new_data2,type = 'l', layout=c(1,2), xlab = 'Intervals', ylab = 'Number of steps')
```

