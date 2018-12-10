# CIGARETTES SAVED
totCigSaved <- function(dfLogs){
  df <- dfLogs[dfLogs$Type %in% c("Behaviour","Cheated","On time"),c("User","WeekNumber","Type")]
  df <- table(df)
  df <- data.frame(df)  
  df <- df[df$Freq!=0,]
  ref <- df[df$Type=="Behaviour","Freq"]
  df <- df[df$WeekNumber!=0,]
  
  saved <- max(0,df[df$Type=="Behaviour","Freq"]) - max(0,df[df$Type=="Cheated","Freq"]) - max(0,df[df$Type=="On time","Freq"])  
  df <- aggregate(df$Freq,by = list(WeekNumber=df$WeekNumber), sum)
  
  return(renderValueBox({
    valueBox(
      paste0(sum(df$x),"€")
      ,"Savings"
      ,icon = icon("euro",lib='glyphicon')
      ,color = "yellow")  
  }))
  
}

avgCig <- function(dfLogs){
  df <- dfLogs[dfLogs$Type %in% c("Behaviour","Cheated","On time"),c("User","WeekNumber","Type")]
  df <- table(df)
  df <- data.frame(df)  
  df <- df[df$Freq!=0,]
  ref <- df[df$Type=="Behaviour","Freq"]
  df <- df[df$WeekNumber!=0,]
  
  saved <- max(0,df[df$Type=="Behaviour","Freq"]) - max(0,df[df$Type=="Cheated","Freq"]) - max(0,df[df$Type=="On time","Freq"])
  df <- aggregate(df$Freq,by = list(WeekNumber=df$WeekNumber), sum)
  tot <- sum(df$x)
  avg <- tot/length(unique(dfLogs$User))
  
  return(renderValueBox({
    valueBox(
      round(avg,2)
      ,"Cigarettes saved in average"
      ,icon = icon("star",lib='glyphicon')
      ,color = "blue")  
  }))
  
  
}

totAvgProg <- function(dfLogs){
  users <- unique(dfLogs$User)
  df <- progress(dfLogs,users[1])
  for(user in users){
    df2 <- progress(dfLogs,user)
    df <- rbind.data.frame(df,df2)
  }
  df <- aggregate(list(Progress=df$Progress),by = list(WeekNumber=df$WeekNumber),mean)
  return(renderPlot({qplot(df$WeekNumber,df$Progress, geom=c("point", "smooth"), group=1, xlab = "Week Number", ylab = "Average progress") + scale_y_continuous(labels=scales::percent)}))
}

totCigCons <- function(dfLogs){
  df <- dfLogs[c("WeekNumber","Day","Type")]
  df <- data.frame(table(df))
  df <- df[df$Type %in% c("Cheated","On time","Behaviour"),c("WeekNumber","Day","Freq")]
  df <- df[df$Freq!=0,c("WeekNumber","Day","Freq")]
  df$Day <- factor(df$Day, levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df <- df[order(df$Day), ]
  return(renderPlot({qplot(Day, Freq, data = df, geom=c("boxplot"))}))
}

totCigSlot <- function(dfLogs){
  df <- dfLogs[c("Time","Day","Type")]
  df$Hour <- strftime(df$Time,format="%H")
  interv <- c(0,2,4,6,8,10,12,14,16,18,20,22,24)
  temp <- cut(as.numeric(df$Hour), breaks=interv, right = FALSE)
  df$Hour <- temp
  df <- df[df$Type %in% c("Cheated","On time","Behaviour"),c("Hour","Day")]
  df <- data.frame(table(df))
  df <- aggregate(list(Freq=df$Freq),by = list(Hour=df$Hour,Day=df$Day), sum)
  df$Day <- factor(df$Day, levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df <- df[order(df$Day), ]
  return(renderPlot({ggplot(data = df, aes( x = Day, y = Freq , fill=Hour) )+geom_bar( stat = 'identity',position = 'dodge')}))
  
}

totProgRate <- function(dfLogs){
  users <- unique(dfLogs$User)
  df <- progress(dfLogs,users[1])
  for(user in users){
    df2 <- progress(dfLogs,user)
    df <- rbind.data.frame(df,df2)
  }
  df <- aggregate(list(Progress=df$Progress),by = list(WeekNumber=df$WeekNumber),mean)
  rate <- c()
  rate[1] <- 0
  rate[2] <- 0
  prog <- df$Progress
  for(i in 3:length(prog)){
    if(prog[i-1]>=-0.01 && prog[i-1]<=0.01){
      rate[i] <-  (prog[i] - prog[i-2])/prog[i-2]
    } else {
      rate[i] <-  (prog[i] - prog[i-1])/prog[i-1]
    }
  }
  week <- 1:length(prog)
  return(renderPlot({qplot(week,rate, geom=c("point", "smooth"), group=1, xlab = "Week Number", ylab = "Progress rate") + scale_y_continuous(labels=scales::percent)}))
}


totEng <- function(dfLogs){
  users <- unique(dfLogs$User)
  df <- weeklyEngagement(dfLogs,users[1])
  for(user in users){
    df2 <- weeklyEngagement(dfLogs,user)
    df <- rbind.data.frame(df,df2)
  }
  df <- df[!is.na(df$engagementWeek),]
  df <- aggregate(list(engagement=df$engagementWeek),by = list(WeekNumber=df$week),mean)
  return(renderPlot({qplot(df$WeekNumber,df$engagement, geom=c("point", "smooth"), group=1, xlab = "Week Number", ylab = "Average engagement") + scale_y_continuous(labels=scales::percent)}))
}