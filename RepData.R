#set working directory.
setwd("C:/Users/bradyhsu.PHALANX/RepData_PeerAssessment1")

##Loading and preprocessing the data
#Load the data (i.e. read.csv())

        #Process/transform the data (if necessary) into a format suitable for your analysis
        activitydata <- read.csv(file = "./activity.csv", header = TRUE, sep=",", na.strings = "NA")
        
        #remove na value by steps
        ad_no_na <- activitydata[!is.na(activitydata$steps),]
        
        #convert date type to numeric
        ad_no_na$date <- as.numeric(format(as.Date(ad_no_na$date, "%Y-%m-%d"), "%Y%m%d"))
        
        #ad1 <- ad_no_na[which(ad_no_na$date == '20121002'),]
        #ad2 <- ad_no_na[which(ad_no_na$date == '20121003'),]



##What is mean total number of steps taken per day?

        #For this part of the assignment, you can ignore the missing values in the dataset.
        #Make a histogram of the total number of steps taken each day
        library(plyr)
        #ad_summarized_data <- ddply(ad_no_na, .(date), summarise, "total number of steps"=nrow(date))
        ad_no_na_summarized_data <- ddply(ad_no_na, .(date), summarise, "total number of steps"=sum(steps), "mean"=mean(steps), "median"=median(steps))
        hist(ad_no_na_summarized_data$"total number of steps", xlab="total number of steps")
        
        #Calculate and report the mean and median total number of steps taken per day
        ad_no_na_summarized_data$mean
        ad_no_na_summarized_data$median



##What is the average daily activity pattern?

        ad_no_na_summarized_data2 <- ddply(ad_no_na, .(interval), summarise, "mean"=mean(steps), "max"=max(steps))
        #Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken,
        #averaged across all days (y-axis)
        #Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
        plot(x=ad_no_na_summarized_data2$interval, y=ad_no_na_summarized_data2$mean, type="l", xlab="the 5-minute interval", ylab="the average number of steps taken")


##Imputing missing values

        #Note that there are a number of days/intervals where there are missing values (coded as NA).
        #The presence of missing days may introduce bias into some calculations or summaries of the data.
        
        #Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
        ad_na <- activitydata[is.na(activitydata$steps),]
        nrow(ad_na)
        #nrow(activitydata)
        
        #Devise a strategy for filling in all of the missing values in the dataset.
        #The strategy does not need to be sophisticated. For example, you could use the mean/median for that day,
        #or the mean for that 5-minute interval, etc.
        
        #produce a imputed value by medain from activity data without NA value.
        imputedval <- median(ad_no_na_summarized_data$mean)
        
        #Create a new dataset that is equal to the original dataset but with the missing data filled in.
        #convert date type to numeric
        #ad <- activitydata
        #ad$date <- as.numeric(format(as.Date(ad$date, "%Y-%m-%d"), "%Y%m%d"))
        ad_na$date <- as.numeric(format(as.Date(ad_na$date, "%Y-%m-%d"), "%Y%m%d"))
        
        #assign a imputed value into column 'steps'
        ad_na$steps = imputedval
        #ad_na_summarized_data <- ddply(ad_na, .(date), summarise, "total number of steps"=sum(steps), "mean"=mean(steps), "median"=median(steps))
        
        #merge two datasets
        ad_all<- rbind(ad_no_na, ad_na)

        #order by column steps
        ad_all <- ad_all[with(ad_all, order(date)),]

        #summarising datasets
        ad_all_summarized_data <- ddply(ad_all, .(date), summarise, "total number of steps"=sum(steps), "mean"=mean(steps), "median"=median(steps))
                
        #Make a histogram of the total number of steps taken each day and Calculate and report the mean 
        #and median total number of steps taken per day.
        hist(ad_all_summarized_data$"total number of steps", xlab="total number of steps")
        
        #Do these values differ from the estimates from the first part of the assignment?
        #par(mfrow = c(2, 1))
        #hist(ad_no_na_summarized_data$"total number of steps", xlab="total number of steps without NA")
        #hist(ad_summarized_data$"total number of steps", xlab="total number of steps with NA")
        
        
        ad_all_summarized_data2 <- ddply(ad_all, .(interval), summarise, "mean"=mean(steps), "max"=max(steps))
        par(mfrow = c(2, 1))
        plot(x=ad_all_summarized_data2$interval, y=ad_all_summarized_data2$mean, type="l", xlab="the 5-minute interval with NA", ylab="the average number of steps taken")
        plot(x=ad_no_na_summarized_data2$interval, y=ad_no_na_summarized_data2$mean, type="l", xlab="the 5-minute interval without NA", ylab="the average number of steps taken")

        #What is the impact of imputing missing data 
        #on the estimates of the total daily number of steps?
        fit1 <- lm(ad_no_na_summarized_data2$interval~ ad_no_na_summarized_data2$mean)
        fit2 <- lm(ad_all_summarized_data2$interval~ ad_all_summarized_data2$mean)


##Are there differences in activity patterns between weekdays and weekends?

        #For this part the weekdays() function may be of some help here. Use the dataset 
        #with the filled-in missing values for this part.
        
        #Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
        #indicating whether a given date is a weekday or weekend day.
        chk_weekend <- function(x) 
        { 
                if(((as.POSIXlt(as.Date(as.character(x), "%Y%m%d"))$wday) == 6 ) | 
                           ((as.POSIXlt(as.Date(as.character(x), "%Y%m%d"))$wday) == 0 )){
                                return("weekend")
                } else {
                                return("weekday")
                }
        }
        
        ad_all$weekdays <- chk_weekend(ad_all$date)

        #Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
        #and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

        ad_all_summarized_data2 <- ddply(ad_all, .(interval), summarise, "mean"=mean(steps), "max"=max(steps))
        par(mfrow = c(1, 1))
        plot(x=ad_all_summarized_data2$interval, y=ad_all_summarized_data2$mean, type="l", xlab="Interval", ylab="Number of steps")
        
        #See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
