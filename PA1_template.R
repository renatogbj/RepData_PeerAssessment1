library(dplyr)

setwd('/Users/renato/Documents/Coursera/Coursera-Data-Science/Reproducible Research/')

# Loading and preprocessing the data

activity <- read.csv('activity.csv')

summary(activity)
str(activity)


# What is mean total number of steps taken per day?

df <- tbl_df(activity)
total_steps_per_day <- df %>% group_by(date) %>% 
        summarize(total_steps = sum(steps))

hist(total_steps_per_day$total_steps, 
     main = 'Histogram of Total Number of Steps per Day',
     xlab = 'Total number of steps taken each day')

mean(total_steps_per_day$total_steps, na.rm = TRUE)
median(total_steps_per_day$total_steps, na.rm = TRUE)


# What is the average daily activity pattern?

avg <- df %>% group_by(interval) %>% 
        summarize(avg_steps = mean(steps, na.rm = TRUE))

plot(x = avg$interval, y = avg$avg_steps,
     xlab = 'Interval', ylab = 'Average steps per day')


avg[avg$avg_steps == max(avg$avg_steps), ]


# Imputing missing values

sum(is.na(activity))

df_no_na <- merge(activity, avg, by = 'interval')
df_no_na <- df_no_na[order(df_no_na$date), ]
df_no_na$steps[is.na(df_no_na$steps)] <- 
        df_no_na$avg_steps[is.na(df_no_na$steps)]
df_no_na <- df_no_na %>% select(steps, date, interval)

df_no_na <- tbl_df(df_no_na)
total_steps_per_day_no_na <- df_no_na %>% group_by(date) %>% 
        summarize(total_steps = sum(steps))

hist(total_steps_per_day_no_na$total_steps, 
     main = 'Histogram of Total Number of Steps per Day',
     xlab = 'Total number of steps taken each day')

mean(total_steps_per_day_no_na$total_steps)
median(total_steps_per_day_no_na$total_steps)

# Are there differences in activity patterns between weekdays and weekends?

weekday_vector <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
df_weekdays <- df_no_na %>% mutate(weekday_name = weekdays(as.POSIXct(date)))
df_weekdays <- df_weekdays %>% 
        mutate(day_of_week = ifelse(weekday_name %in% weekday_vector, 
                                    'weekday', 'weekend')) %>%
        select(-weekday_name)


par(mfrow = c(2, 1))
plot(x = df_weekdays$interval[df_weekdays$day_of_week == 'weekday'],
     y = df_weekdays$steps[df_weekdays$day_of_week == 'weekday'],
     main = 'Num. of steps avg. in weekday',
     xlab = 'Interval', ylab = 'Num. Steps', type = 'l')
plot(x = df_weekdays$interval[df_weekdays$day_of_week == 'weekend'], 
     y = df_weekdays$steps[df_weekdays$day_of_week == 'weekend'],
     main = 'Num. of steps avg. in weekend',
     xlab = 'Interval', ylab = 'Num. Steps', type = 'l')
