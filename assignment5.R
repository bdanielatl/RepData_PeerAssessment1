library(dplyr)
library(ggplot2)
source("multiplot.R")
par()              # view current settings
opar <- par()      # make a copy of current settings
activity.df <- read.csv("activity.csv",stringsAsFactors = FALSE)
activity.df.cc <- activity.df %>% filter(complete.cases(.))
activity.df.cc <- activity.df.cc %>% mutate(date=as.Date(date , "%Y-%m-%d"))

agg.df <- activity.df.cc %>% 
        group_by(date)%>%
        summarize(sum_steps = sum(steps), 
                  mean_steps=mean(steps), 
                  median_steps = median(steps))
#number of steps per day (each bar is bucket range on the number of steps,
#the count are the number for days that bucket was taken )
hist(agg.df$sum_steps, 
     ylab="number of steps per day", 
     xlab="bins of number of steps", 
     main ="Histogram of Number of Daily Steps Taken")

#average daily pattern
adp.df <- activity.df.cc %>% 
          group_by(interval) %>%
          summarise(mean_steps =mean(steps))

qplot(interval,mean_steps,data=adp.df,geom="line")


#time when the maximum number of steps are taken
adp.df %>% arrange(desc(mean_steps)) %>% head(n=1) %>% select (interval)        


#count the number of rows that have NAs
num_rows_na<-activity.df %>% filter(is.na(steps)) %>% count()

#fill those rows with NAs with the average number of steps

activity.df.fc <- merge(adp.df,activity.df, by="interval") %>% 
                mutate(nsteps=ifelse(is.na(steps),mean_steps,steps))


#rollup to the daily level
activity.day <- activity.df.fc  %>% group_by(date) %>%
                                summarize(total_n_steps=sum(nsteps))

par(mfrow=c(1,2))


#plot the total steps after
hist(activity.day$total_n_steps , ylab="number of steps per day", 
     xlab="bins of number of steps", 
     main ="Daily Steps Taken \nwith NA's filled by Avg Day")

hist(agg.df$sum_steps, 
     ylab="number of steps per day", 
     xlab="bins of number of steps", 
     main ="Number of Daily \nSteps Taken")


total_daily_steps<-sum(activity.day$total_n_steps)
mean_daily_steps<- mean(activity.day$total_n_steps)

# Do these values differ from the estimates from the first part of the assignment? 
        #they do differ, the number of steps per day is boosted, 
        #especially in the 10-15K bucket
# What is the impact of imputing missing data on the estimates of the total 
# daily number of steps?

########
activity.df.cc.wd <- activity.df.cc %>% 
        mutate(isweekday=ifelse(!(weekdays(date,TRUE) %in% c("Sat","Sun")),1,0)) %>%
        group_by (interval,isweekday) %>%
        summarise(total_steps = sum(steps))

# p1<- ggplot(data=activity.df.cc.wd%>%
#                     filter(isweekday==1),interval, total_steps,geom="line")
# 
# p2<- ggplot(data=activity.df.cc.wd%>%
#                     filter(isweekday==0),interval, total_steps,geom="line")
# multiplot(p1,p2,cols=1,rows=2)


p1<-ggplot(
        data=activity.df.cc.wd%>%filter(isweekday==1), 
        aes(x=interval,y=total_steps)) + geom_line()+ ylim(0,10000) +
        ggtitle("Total Steps Over a Day on Weekdays")

p2<-ggplot(
        data=activity.df.cc.wd%>%filter(isweekday==0), 
        aes(x=interval,y=total_steps)) + geom_line() + ylim(0,10000) +
        ggtitle("Total Steps Over a Day on Weekends")

multiplot(p1,p2)



par(opar)          # restore original settings