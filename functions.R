plotFunction <- function(df,user,week){
  # Extract frequency per type for a given week
  df <- df[df$User==user,c("Type","WeekNumber")]
  df <- df[df$WeekNumber==week,c("Type")]
 return (renderPlot({
    barplot(table(df),main="Number of cigarettes per type")
  }) )
}



userEngagement <- function(df,user){
  # Extract the number of records for each week
  df <- df[df$User==user,c("Type","WeekNumber")]
  df <- data.frame(table(df))
  behavior <- (sum(df[df$Type=="Behaviour","Freq"]))
  df <- df[df$Type %in% c("Cheated","On time"),c("WeekNumber","Freq")]
  df <- aggregate(df$Freq, by = list(df$WeekNumber), sum)
  
  # Replace first weeks value with the behaviour week value
  df$x <- as.character(df$x)
  df$x[df$x==0] <- behavior
  
  # Offset the values to compare the week n value with the value at week n-1
  dfbis <- df[-c(1),]
  df <- df[-nrow(df),]
  y <- (as.numeric(dfbis$x)-as.numeric(df$x))/(as.numeric(dfbis$x))*100
  z <- 1:length(y)
  return (renderPlot({
    qplot(z, y, geom=c("point","smooth"),main="Engagement variation compared to the previous week",xlab = "Week Number",ylab = "Engagement (%)")
  }))
  
}

progPerAgeBin <- function(df1, df2){
  
  #Extract age from the survey and merge it with the logs 
  temp1 <- df1[c("User","Type","WeekNumber")]
  df2 <- plyr::rename(df2,c("Name"="User"))
  temp2 <- df2[c("Age","User")]
  temp3 <- unique(df1[c("User")])
  temp2 <- merge(x = temp2, y = temp3, by = "User")
  
  # Create Age bins
  interv <- pretty(temp2$Age)
  temp <- cut(temp2$Age, breaks=interv, right = FALSE)
  
  temp2$Age <- temp
  
  # Give an age bin for each user
  freqByBin <- temp2
  
  # Extract the frequency per bins if there's any records
  freqByBin[freqByBin==0] <- NA
  freqByBin <- na.omit(freqByBin)
  freqByBin <- data.frame(table(freqByBin$Age))
  
  # Extract the number of records per age bin per type 
  df3 <- merge(x = temp1, y = temp2, by = "User")
  df5 <- df3[c("Type","Age")]
  df3 <- df3[c("User","Type","Age", "WeekNumber")]
  df4 <- aggregate(df3$WeekNumber, by = list(df3$Age,df3$User), max)
  df4 <- aggregate(df4$x,by = list(df4$Group.1), mean)
  df5 <- data.frame(table(df5))
  
  # Only consider "cheated" and "on time" records
  y <-df5[df5$Type=="Cheated","Freq"]+df5[df5$Type=="On time","Freq"]
  
  # Calculate frequency for those types in each age bin
  y <- y[y!=0]
  y <- y/df4$x
  freqByBin <- freqByBin[freqByBin$Freq!=0,c("Var1","Freq")]
  y <- y/freqByBin$Freq
  
  
  # Calculate the number of records for the behavior week as a comparison 
  behavior <- df5[df5$Type=="Behaviour","Freq"]
  behavior <- behavior[behavior!=0]
  
  # Calculate the progress rate
  y <- (behavior-y)/behavior*100
  y[is.infinite(y)]<-0
  names(y)=freqByBin$Var1
  

  return (renderPlot({
    barplot(y,main="Progress per age bin compared to the behavior week",xlab = "Age bin",ylab = "Progress rate (%)")
    
  }))
}



progRate <- function(df,user){
  tryCatch(
    {
      # Extract "cheated" and "on time" frequencies and compare them to the behaviour week
      x <- df[c("User","Type","WeekNumber")]
      x <- data.frame(table(x))
      x <- x[x$User==user,c("User","Type","WeekNumber","Freq")]
      y <-x[x$Type=="Cheated","Freq"]+x[x$Type=="On time","Freq"]
      y <- y[y!=0]
      behavior <- (sum(x[x$Type=="Behaviour","Freq"]))
      y <- (behavior-y)/behavior*100
      y <- y[!is.infinite(y)]
     z <- 1:length(y)
   
  return (renderPlot({
    qplot(z,y, geom=c("point", "smooth"), group=1, main = "Average progress rate per week (compared to the behaviour week)",  xlab = "Week number",ylab = "Progress rate (%)")
    }))
    },
  error = function(e) {})
  
}

getInfos <- function(df,user){
  
  # Extract information from the survey
  df1 <- df[df$Name==user,c("Gender","Age","BMI","CigPerDay")]
  return(renderPrint({
      gender <- paste("Gender: ",df1$Gender)
      age <- paste("Age: ",df1$Age)
      bmi <- paste("BMI: ",round(df1$BMI))
      cig <- paste("Cigarettes/Day: ",df1$CigPerDay)
      HTML(paste(gender,age,bmi,cig, sep = '<br/>'))
    }))
}

lastWeekCons <- function(df,user,week){
  tryCatch({
    # Extract consumption depending on the user and the weeknumber
    df <- df[df$User==user,c("User","Day","WeekNumber","Type")]
    df <- df[df$Type %in% c("Behaviour","On time","Cheated"),]
    df <- df[df$WeekNumber==week,c("Day")]
    df <- data.frame(table(df))
    df$df <- factor(df$df, levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    df <- df[order(df$df), ]
    plot = qplot(df$df,df$Freq, geom=c("point", "smooth"), group=1, main = "Number of cigarettes smoked (on time/cheated or behaviour)", xlab = "Day of the week", ylab = "Number of cigarettes")
    return(renderPlot(plot))
  },
  error = function(e) {})
  
  
}

getmaxWeek <- function(df,user){
  # Extract the total number of week for a user
  df <- df[df$User==user,c("TotalWeek")]
  return(df[1])
}

dailyCons <- function(df){
  # Extract consumption per weekday for all the users
  df1 <- df[c("User","Day")]
  df2 <- data.frame(table(df1))
  df2$Day <- factor(df2$Day, levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df2 <- df2[order(df2$Day), ]
  return (renderPlot({qplot(Day, Freq, data = df2, geom=c("boxplot"),main="Mean and Std of Cigarette Consumption per weekday")}))
}

weeklyTypes <- function(df,week,type){
  df3 <- df[df$WeekNumber==week,c("User","Time","Type")]
  df3 <- df3[df3$Type %in% type,c("User","Time","Type")]
  df3$Hour <- strftime(df3$Time,format="%H")
  interv <- c(0,6,10,14,18,22,24)
  temp <- cut(as.numeric(df3$Hour), breaks=interv, right = FALSE)
  df3$Hour <- temp
  
  y <- data.frame(table(df3[c("Hour","Type")]))
  plot = ggplot(data = y, aes( x = Hour, y = Freq, fill = Type ) )+geom_bar( stat = 'identity',position = 'dodge'  )+ggtitle("Mode usage per week/per time slot")
  
  return(renderPlot(plot))
}