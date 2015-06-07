library(dplyr)
library(sqldf)

Sys.setlocale("LC_TIME", "English")

## Loading and preprocessing the data

file <- "./activity.csv"

if (!file.exists(file)){
     unzip("activity.zip", unzip = "internal") 
}

data <- read.csv(file, header=TRUE, sep=",", stringsAsFactors = FALSE)

## What is mean total number of steps taken per day?

q1 <- select(data[!is.na(data$steps),],steps,date) %>% 
          group_by(date) %>% 
          summarize(TotalSteps=sum(steps))

plot(as.Date(q1$date,format="%Y-%m-%d"),q1$Total.Steps,type="h",xlab="Date",ylab="Total Steps",main="Total Steps Per Date",col="blue")

mean(q1$TotalSteps)

median(q1$TotalSteps)

## What is the average daily activity pattern?

q2 <- select(data[!is.na(data$steps),],steps,interval) %>% 
    group_by(interval)  %>% 
    summarize(AverageSteps=mean(steps))

plot(q2,type="l",xlab="Interval",ylab="Average Steps",main="Average Steps per Interval",col="blue")

q2[order(-q2$AverageSteps),][1,1]

## Imputing missing values

sum(is.na(data))

q3<- sqldf("select 
              case 
                when data.steps is null then AverageSteps 
                else steps 
              end as steps,
              data.date,
              data.interval
            from 
              data 
              inner join q2 
              on data.interval=q2.interval")

q3 <- select(q3,steps,date) %>% 
             group_by(date) %>% 
             summarize(TotalSteps=sum(steps))

plot(as.Date(q3$date,format="%Y-%m-%d"),q3$TotalSteps,type="h",xlab="Date",ylab="Total Steps",main="Total Steps Per Date",col="blue")

mean(q3$TotalSteps)

median(q3$TotalSteps)