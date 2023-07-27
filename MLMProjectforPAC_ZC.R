# read in files
filename <- "20230321_EMAData.csv"
EMAdata <- read_csv(filename)

filename <- "20230327_betweenday.csv"
betweenday <- read_csv(filename)

EMAdata$id <- as.factor(EMAdata$id)
EMAdata$event <- as.factor(EMAdata$event) 
levels(EMAdata$event) <- c("Baseline", "Session 4", "Post")
EMAdata$enrollmentgroup <- as.factor(EMAdata$enrollmentgroup) 
levels(EMAdata$enrollmentgroup) <- c("Waitlist Control", "Immediate Enrollment", "Group")

#remove group 2
EMAdata <- filter(EMAdata, enrollmentgroup != "Group")

## Affect Dynamic Variables
withinday <- EMAdata[,c('id', 'event', 'day', 'instance', 'enrollmentgroup', 'time_interval', 'positive_score', 'negative_score', "positive_SD","negative_SD")]

#convert time_interval from h:m:s to minutes
withinday$time_interval_min <- hour(withinday$time_interval)*60 + minute(withinday$time_interval)

withinday <- withinday %>%
  relocate("time_interval_min", .after="time_interval")

# scatter plot of ASD x time interval (hour)
ggplot(withinday, aes(x = time_interval, y=positive_SD)) + geom_point(size = 0.5) +
  ggtitle("Successive Difference - Positive")

ggplot(withinday, aes(x = time_interval, y=negative_SD)) + geom_point(size = 0.5) +
  ggtitle("Successive Difference - Negative")

#create between day variable
betweenday <- filter(betweenday, betweenday$id != "12046")

#split betweenday df by event
betweenday_baseline <- filter(betweenday, betweenday$event == "1")
betweenday_session4 <- filter(betweenday, betweenday$event == "2")
betweenday_post <- filter(betweenday, betweenday$event == "3")

#remove group 2 & 12046 (no between data)
id_names <- c("12015", "12016", "12017", "12018", "12019", "12020","12021", "12022", "12023", "12024","12025", "12026", "12027", "12028", "12029", "12030", "12031", "12032", "12033", "12034", "12035", "12036", "12037", "12038", "12039", "12040", "12041","12043", "12044", "12045", "12050", "12057")

length(id_names)

id_group <- c("1", "1", "0", "1", "0", "1", "0", "0", "1", "1", "0", "1", "0", "0", "0", "0", "1", "1", "1", "0", "1", "0", "0", "1", "1", "0", "0", "1", "0", "1","1", "1")

length(id_group)

# calculate affect dynamic variables
```{r}
# initialize MEAN matrix
ema_mean <- matrix(0, 32, 7)

colnames(ema_mean) <- c("group", "posbaseline", "possession4", "pospost", "negbaseline", "negsession4", "negpost")
rownames(ema_mean) <- id_names

for(i in 1:32){
  ema_mean[i,1] <- id_group[i]
}
```

```{r}
#fill positive mean
## baseline

for(i in 1:32){
  name <- id_names[i]
  index <- which(betweenday_baseline$id == name)
  
  vec <- c()
  len <- length(index)
  
  for(x in 1:len){
    n <- betweenday_baseline$positive_mean[index[x]]
    vec <- c(vec, n)
  }
  
  ema_mean[i,2] <- mean(vec)
}

## session4

for(i in 1:32){
  name <- id_names[i]
  index <- which(betweenday_session4$id == name)
  
  vec <- c()
  len <- length(index)
  
  for(x in 1:len){
    n <- betweenday_session4$positive_mean[index[x]]
    vec <- c(vec, n)
  }
  
  ema_mean[i,3] <- mean(vec)
}

## post

for(i in 1:32){
  name <- id_names[i]
  index <- which(betweenday_post$id == name)
  
  vec <- c()
  len <- length(index)
  
  for(x in 1:len){
    n <- betweenday_post$positive_mean[index[x]]
    vec <- c(vec, n)
  }
  
  ema_mean[i,4] <- mean(vec)
}
```

```{r}
#fill negative mean
## baseline

for(i in 1:32){
  name <- id_names[i]
  index <- which(betweenday_baseline$id == name)
  
  vec <- c()
  len <- length(index)
  
  for(x in 1:len){
    n <- betweenday_baseline$negative_mean[index[x]]
    vec <- c(vec, n)
  }
  
  ema_mean[i,5] <- mean(vec)
}

## session4

for(i in 1:32){
  name <- id_names[i]
  index <- which(betweenday_session4$id == name)
  
  vec <- c()
  len <- length(index)
  
  for(x in 1:len){
    n <- betweenday_session4$negative_mean[index[x]]
    vec <- c(vec, n)
  }
  
  ema_mean[i,6] <- mean(vec)
}

## post

for(i in 1:32){
  name <- id_names[i]
  index <- which(betweenday_post$id == name)
  
  vec <- c()
  len <- length(index)
  
  for(x in 1:len){
    n <- betweenday_post$negative_mean[index[x]]
    vec <- c(vec, n)
  }
  
  ema_mean[i,7] <- mean(vec)
}
```

```{r}
ema_mean <- as.data.frame(ema_mean)
ema_mean$posbaseline <- as.numeric(ema_mean$posbaseline)
ema_mean$possession4 <- as.numeric(ema_mean$possession4)
ema_mean$pospost <- as.numeric(ema_mean$pospost)
ema_mean$negbaseline <- as.numeric(ema_mean$negbaseline)
ema_mean$negsession4 <- as.numeric(ema_mean$negsession4)
ema_mean$negpost <- as.numeric(ema_mean$negpost)
```

```{r}
#group by group
ema_mean%>%
  group_by(group) %>%
  summarise_at(vars(posbaseline, possession4, pospost, negbaseline, negsession4, negpost, ), list(mean=mean, sd=sd), na.rm=TRUE)
```