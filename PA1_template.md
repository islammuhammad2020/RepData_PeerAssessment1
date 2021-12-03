---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

```r
suppressPackageStartupMessages(library(tidyverse))
library(tidyverse)
activity <- read_csv("activity.zip") 
```

```
## 
## -- Column specification ---------------------------------------------------------------------------------------------
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```
## What is mean total number of steps taken per day?

```r
activity_total <- activity %>% filter(steps != "NA") %>% 
               group_by(date) %>% summarise(total_steps = sum(steps))
ggplot(activity_total,aes(x=date,weight = total_steps)) + stat_count()
```

![](PA1_template_files/figure-html/mean_total_day-1.png)<!-- -->

```r
activity_mean <- activity %>% filter(steps != "NA") %>% 
               group_by(date) %>% summarise(total_mean = mean(steps), total_median = median(steps))
```

## What is the average daily activity pattern?

```r
activity_interval <- activity %>% filter(steps != "NA") %>% 
               group_by(interval) %>% summarise(interval_mean = mean(steps))
ggplot(activity_interval, aes(x = interval, y = interval_mean)) + geom_line()
```

![](PA1_template_files/figure-html/average_daily_activity-1.png)<!-- -->

```r
slice_max(activity_interval,interval_mean, n=1)
```

```
## # A tibble: 1 x 2
##   interval interval_mean
##      <dbl>         <dbl>
## 1      835          206.
```



## Imputing missing values

```r
sum_NA <- sum(is.na(activity$steps))
activity_NA <- filter(activity, is.na(steps))
for(i in activity_interval$interval) {
    activity_NA$steps[activity_NA$interval == i] <- activity_interval$interval_mean[activity_interval$interval == i]
}
activity$steps[is.na(activity$steps)] <- activity_NA$steps
activity_total_imputed <- activity %>% group_by(date) %>% summarise(total_steps = sum(steps))
ggplot(activity_total_imputed,aes(x=date,weight = total_steps)) + stat_count()
```

![](PA1_template_files/figure-html/activity_total_imputed-1.png)<!-- -->

```r
activity_mean_imputed <- activity %>% group_by(date) %>% summarise(total_mean = mean(steps), total_median = median(steps))
```

## Are there differences in activity patterns between weekdays and weekends?

```r
day_type <- as.factor(ifelse(weekdays(activity$date) %in% c("Saturday", "Sunday"),"weekend","weekday"))
activity_day <- activity %>% mutate(day_type = as.factor(ifelse(weekdays(activity$date) %in% 
                    c("Saturday", "Sunday"),"weekend","weekday")))
activity_total_day <- activity_day %>% group_by(day_type,interval) %>% 
                     summarise(mean_steps = mean(steps))
```

```
## `summarise()` has grouped output by 'day_type'. You can override using the `.groups` argument.
```

```r
ggplot(activity_total_day,aes(x=interval, y=mean_steps)) + geom_line() +facet_grid(day_type~.)
```

![](PA1_template_files/figure-html/activity_pattern-1.png)<!-- -->
