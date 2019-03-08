***Loading & processing data***
===============================

    ## Reading of the activity monitoring data set
    ## Remember to put the data set in your R working directory!
    ## Loading and preprocessing the data

    library(ggplot2)
    library(plyr)
    library(dplyr)

    ## Warning: package 'dplyr' was built under R version 3.5.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    AMD <- read.csv("activity.csv")
    AMD$day <- weekdays(as.Date(AMD$date))

    ## Removing the NA values from data set

    AMDF <- AMD[complete.cases(AMD),]

***What is mean total number of steps taken per day?***
=======================================================

### *Calculate the total number of steps taken per day*

    ## Summarizing of total steps per day

    TotSteps <- aggregate (steps ~ date, AMDF, sum)

### *Make a histogram of the total number of steps taken each day*

    ## Creating the histogram of total steps per day

    hist(TotSteps$steps,
         col="green",
         xlab="Steps",
         main = "Total Steps per Day",
         breaks=5)

![](PA1_template_files/figure-markdown_strict/histogram-1.png)

### *Calculate and report the mean and median of the total number of steps taken per day*

    ## Mean number of steps taken per day

    MeanSteps <- as.integer(mean(TotSteps$steps))
    MeanSteps

    ## [1] 10766

    ## Median of steps taken per day

    MedSteps <- as.integer(median(TotSteps$steps))
    MedSteps

    ## [1] 10765

**The average number of steps taken each day was 10766 steps.**  
**The median number of steps taken each day was 10765 steps.**

***What is the average daily activity pattern?***
=================================================

### *Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*

    ## creation of table with average number of steps per interval

    int5mins <- aggregate(steps ~ interval, AMDF, mean)

    ## Creation of line plot of average number of steps per interval

    g <- ggplot(int5mins, aes(x=interval, y=steps),
                xlab = "Interval",
                ylab="Average Number of Steps")
    g + geom_line(color="orange")+
        xlab("Interval")+
        ylab("Average Number of Steps")+
        ggtitle("Average Number of Steps per Interval")+
        theme(plot.title = element_text(hjust = 0.5))

![](PA1_template_files/figure-markdown_strict/average%20daily%20pattern-1.png)

### *Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*

    ## Checking the maximum number of steps

    maxsteps <- max(int5mins$steps)
    maxsteps

    ## [1] 206.1698

    ## Checking which interval contains maximum number of steps

    maxstepsint <- int5mins$interval[which.max(int5mins$steps)]
    maxstepsint

    ## [1] 835

**The maximum number of steps for a 5-minute interval was 206 steps.**  
**The 5-minute interval which had the maximum number of steps was the
835th interval.**

***Imputing missing values***
=============================

### *Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)*

### *Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*

**My strategy will be replacing the NAs with random number of steps.
This number will be in range 0 ... tripled square root of max number of
steps in the dataset.**

    ## Definition of replacement value

    steplim <- 3*sqrt(max(AMDF$steps))
    steplim

    ## [1] 85.17042

    ## Create a new dataset that is equal to the original dataset but with the
    ## missing data filled in.

    AMD2 <- AMD
    naInd = which(is.na(AMD2$steps))
    AMD2[naInd,"steps"]<-sample(0:steplim,length(naInd),replace = TRUE)

    ## Check for NAs in the new set

    noNA2 <- nrow(AMD2[is.na(AMD2$steps),])
    noNA2

    ## [1] 0

### *Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*

    ## Summarizing of total steps per day for new dataset

    TotSteps2 <- aggregate (steps ~ date, AMD2, sum)

### *Make a histogram of the total number of steps taken each day. Histogram shows change between original (no NAs) and updated data.*

    ## Creation of comparative histogram

    hist(TotSteps2$steps,
         col="green",
         xlab="Steps",
         main = "Total Steps per Day with fixed NAs",
         breaks=5)
    hist(TotSteps$steps,
         col="yellow",
         breaks=5,
         add=TRUE)
    legend("topright", c("Fixed Data", "NA Removed Data"),
           fill=c("green", "yellow"))

![](PA1_template_files/figure-markdown_strict/comparison-1.png)

### *Calculate and report the mean and median of the total number of steps taken per day*

    ## Mean number of steps taken per day for new dataset

    MeanSteps2 <- as.integer(mean(TotSteps2$steps))
    MeanSteps2

    ## [1] 10943

    ## Median of steps taken per day for new dataset

    MedSteps2 <- as.integer(median(TotSteps2$steps))
    MedSteps2

    ## [1] 11458

**Both values are different from values obtained for dataset with
removed NAs. Due to used replacement method (with sampling), results may
be different every time. However, the general shape of distribution is
similar to the original one.**

***Are there differences in activity patterns between weekdays and weekends?***
===============================================================================

### *Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.*

    ## Create new category based on the days of the week

    AMD3 <- AMD2
    AMD3$daycat <- ifelse(AMD3$day %in% c("sobota", "niedziela"), "Weekend", "Weekday")

### *Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).*

#### See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

    ## Summarize data by interval and type of day
    TotSteps3 <- aggregate (steps ~ interval+daycat, AMD3, mean)

    ##Plot data in a panel plot
    g2<-ggplot(TotSteps3,aes(x=interval,y=steps,color=daycat))
    g2+geom_line(stat ="identity")+
        facet_grid(daycat~.,scales = "free")+
        xlab('Interval') +
        ylab('Average number of steps') +
        ggtitle('Average number of steps per day type')+
        theme(plot.title = element_text(hjust = 0.5))

![](PA1_template_files/figure-markdown_strict/final%20comparison-1.png)

**By looking on the plot it can be said that the step activity trends
are different, taking into consideration day of week. At weekends
average number of steps per interval is higher than during week. This
may be due to fact that people have more free time tha during the
week.**
