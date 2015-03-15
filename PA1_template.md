---
title: 'Reproducible Research: Peer Assessment 1'
author: "Matias"
date: "Sunday, March 15, 2015"
output:
  html_document:
    keep_md: yes
    theme: spacelab
---

##Loading and preprocessing the data.


```r
#Load the data
activity<-read.table("C:/Users/Usuario/Downloads/Reproducible Research/activity.csv", 
           header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
```


##Exploring the data.

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
#Number of NA's
table(is.na(activity))
```

```
## 
## FALSE  TRUE 
## 50400  2304
```

##What is mean total number of steps taken per day?

```r
steps = rep(0,nlevels(activity$date))
for( i in 1:nlevels(activity$date)) {
        tot = activity[activity$date == levels(activity$date)[i],] 
        steps[i] = sum(tot$steps)
}
steps =cbind(steps, levels(activity$date))
num_steps=as.numeric(steps[,1])


mn<-mean(na.omit(num_steps))
md<-median(na.omit(num_steps))

steps_per_day <- aggregate(activity[,c("steps"), drop=FALSE],
                           by=list(date=activity$date), FUN=sum)
hist(steps_per_day$steps)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 
The mean total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup> and the median is
1.0765 &times; 10<sup>4</sup>

##What is the average daily activity pattern?


```r
#Filling the NA`s values with the mean for that interval.
library(dplyr)
#Creating a new data set
activity1=activity
for(i in 1:length(activity1$steps)) if (is.na(activity$steps[i])){  
   activity1$steps[i] = mean(filter(tm1, interval==activity1$interval[i])$steps)
}
hist(activity1$steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
mn2<-mean((activity1$steps))
md2<-median((activity$steps))
```
The mean total number of steps taken per day is 37.3825996 and the median is
NA

```r
#Para ver cantidad de pasos por intervalo


#Grouping by interval
by_interval <- tm1 %>% group_by(interval)
#Takes the mean of steps by interval
plot_by_interval <- by_interval %>% summarise_each(funs(mean),steps)
#Plot
plot(plot_by_interval$interval, plot_by_interval$steps, type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
plot_by_interval[plot_by_interval$steps==max(plot_by_interval$steps),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
## 1      835 206.1698
```


```r
#Removes NA's from activity

tm1<-activity[!is.na(activity$steps),]

summary(tm1)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-02:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-03:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-04:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-05:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-06:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-07:  288   Max.   :2355.0  
##                   (Other)   :13536
```


```r
#Para calcular el número de pasos por día
s = rep(0,nlevels(tm1$date))
for( i in 1:nlevels(tm1$date)) {
        tv = tm1[tm1$date == levels(tm1$date)[i],] 
        s[i] = sum(tv$steps)
}
s =cbind(s, levels(tm1$date))
```


The number of steps taken each day is

```r
tab
```

```
##    steps       date
## 1      0 2012-10-01
## 2    126 2012-10-02
## 3  11352 2012-10-03
## 4  12116 2012-10-04
## 5  13294 2012-10-05
## 6  15420 2012-10-06
## 7  11015 2012-10-07
## 8      0 2012-10-08
## 9  12811 2012-10-09
## 10  9900 2012-10-10
## 11 10304 2012-10-11
## 12 17382 2012-10-12
## 13 12426 2012-10-13
## 14 15098 2012-10-14
## 15 10139 2012-10-15
## 16 15084 2012-10-16
## 17 13452 2012-10-17
## 18 10056 2012-10-18
## 19 11829 2012-10-19
## 20 10395 2012-10-20
## 21  8821 2012-10-21
## 22 13460 2012-10-22
## 23  8918 2012-10-23
## 24  8355 2012-10-24
## 25  2492 2012-10-25
## 26  6778 2012-10-26
## 27 10119 2012-10-27
## 28 11458 2012-10-28
## 29  5018 2012-10-29
## 30  9819 2012-10-30
## 31 15414 2012-10-31
## 32     0 2012-11-01
## 33 10600 2012-11-02
## 34 10571 2012-11-03
## 35     0 2012-11-04
## 36 10439 2012-11-05
## 37  8334 2012-11-06
## 38 12883 2012-11-07
## 39  3219 2012-11-08
## 40     0 2012-11-09
## 41     0 2012-11-10
## 42 12608 2012-11-11
## 43 10765 2012-11-12
## 44  7336 2012-11-13
## 45     0 2012-11-14
## 46    41 2012-11-15
## 47  5441 2012-11-16
## 48 14339 2012-11-17
## 49 15110 2012-11-18
## 50  8841 2012-11-19
## 51  4472 2012-11-20
## 52 12787 2012-11-21
## 53 20427 2012-11-22
## 54 21194 2012-11-23
## 55 14478 2012-11-24
## 56 11834 2012-11-25
## 57 11162 2012-11-26
## 58 13646 2012-11-27
## 59 10183 2012-11-28
## 60  7047 2012-11-29
## 61     0 2012-11-30
```


###Histogram of the total number of steps taken each day

```r
hist(as.numeric(s[,1]))
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
The number of NA's in the data set is 2304 

```r
table(is.na(activity))  
```

```
## 
## FALSE  TRUE 
## 50400  2304
```




```r
#Transform date
activity1[,4]<-as.Date(activity1$date)
activity1[,4]<-weekdays(activity1[,4])

#Creates variable days
for (i in 1:nrow(activity1)) {if(activity1[i,4] == "sábado" 
                                | activity1[i,4] == "domingo") 
                        activity1$days[i] = "weekend"
                       
                           if(activity1[i,4] == "lunes" 
                              | activity1[i,4] == "martes"
                              | activity1[i,4] == "miércoles" 
                              | activity1[i,4] == "jueves" 
                              | activity1[i,4] == "viernes")
                         activity1$days[i] = "weekday"
                        }
#Transform variable days to factor
activity1$days = factor(activity1$days)
library("ggplot2")
qplot(interval, steps, data=activity1, facets=days~., geom="path")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 
