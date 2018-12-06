# AGE CATEGORY
ageCat <- function(dfLogs,dfSurvey,user){
  temp1 <- dfLogs[dfLogs$User==user,c("User")]
  df3 <- plyr::rename(dfSurvey,c("Name"="User"))
  temp2 <- df3[df3$User==user,c("Age","User")]
  temp3 <- unique(dfLogs[c("User")])
  temp2 <- merge(x = temp2, y = temp3, by = "User")
  setDT(temp2)
  temp2[Age >0 & Age <30, Category := "Young"]
  temp2[Age >=30 & Age <50, Category := "Middle"]
  temp2[Age >=50, Category := "Old"]
  age <- temp2$Age
  category <- temp2$Category
  if(nrow(temp2)==0){
    age <- "Unknown"
    category <- "Unknown"
  }
  return(renderValueBox({
    valueBox(
      paste0("Age: ",age)
      ,paste('Category: ',category)
      ,icon = icon("user",lib='glyphicon')
      ,color = "purple")  
  }))
  
}

# CIGARETTES SAVED
cigSaved <- function(dfLogs,user){
  df <- dfLogs[dfLogs$User==user & dfLogs$Type %in% c("Behaviour","Cheated","On time"),c("WeekNumber","Type")]
  df <- table(df)
  df <- data.frame(df)  
  df <- df[df$Freq!=0,]
  ref <- df[df$Type=="Behaviour","Freq"]
  df <- df[df$WeekNumber!=0,]
  
  saved <- df[df$Type=="Behaviour","Freq"] - df[df$Type=="Cheated","Freq"] - df[df$Type=="On time","Freq"]  
  df <- aggregate(df$Freq,by = list(df$WeekNumber), sum)
  df <- plyr::rename(df,c("Group.1"="WeekNumber"))
  
  return(renderValueBox({
    valueBox(
      paste0(sum(df$x),"€")
      ,"Savings"
      ,icon = icon("euro",lib='glyphicon')
      ,color = "yellow")  
  }))

}

# OVERALL PROGRESS
progress <- function(dfLogs,user){
  # Extract frequences per type per week
  df <- dfLogs[dfLogs$User==user,c("Type","WeekNumber")]
  df <- table(df)
  df <- data.frame(df)  
  
  # Calculate the progress for week 1 and 2
  y <-df[df$Type=="Cheated","Freq"]+df[df$Type=="On time","Freq"]
  y <- y[y!=0]
  behavior <- (sum(df[df$Type=="Behaviour","Freq"]))
  y <- (behavior-y)/behavior
  y <- y[!is.infinite(y)]
  
  # Calculate the progress for week ≥ 3
  cons <- df[df$Type=="Cheated","Freq"]+df[df$Type=="On time","Freq"]
  cons[1] <- df[df$Type=="Behaviour","Freq"][1]
  avg <- c(0)
  
  for (i in 4:length(cons)){
    if(!isEngaged(dfLogs,i,user)){
      for(j in (i-1):1){
        if(isEngaged(dfLogs,j,user)){
          cons[i] <- cons[j]
          break;
        }
      }
    }
    averageCons <- (cons[i-1]+cons[i-2]+cons[i-3])/3
    avg[i] <- (averageCons - cons[i])/averageCons
  }
  
  # Concatenate the progress in a unique array
  prog <- c(0,y[2:3],avg[4:length(cons)])
  df <- unique(df[c("WeekNumber")])
  df$Progress <- prog
  
  return(df)
  
}

# AVERAGE PROGRESS
averageProgress <- function(dfLogs,user){
  print(user)
  df <- progress(dfLogs,user)
  avProg <- round(mean(df$Progress[2:length(df$Progress)])*100,2)
  return(renderValueBox({
    valueBox(
      paste0(avProg,"%")
      ,"Average Progress"
      ,icon = icon("export",lib='glyphicon')
      ,color = "green")  
  }))
}

# PROGRESS CATEGORY
progressCat <- function(dfLogs,user){
  avProg <- averageProgress(dfLogs,user)
  if(avProg <= 0.2){
    progCat = "low"
  }else if(avProg >= 0.5){
    progCat = "high"
  }else progCat = "medium"
  
  return(progCat)
}

weeklyEngagement <- function(dfLogs,user){
  df <- dfLogs[dfLogs$User==user & dfLogs$WeekNumber!=0,c("WeekNumber","Type")]
  df <- data.frame(table(df))
  week <- unique(df$WeekNumber)
  engagementWeek <- 1 - df[df$Type=="Auto skipped","Freq"]/(df[df$Type=="Auto skipped","Freq"]+ df[df$Type=="Skipped","Freq"] + df[df$Type=="On time","Freq"] + df[df$Type=="Snoozed","Freq"] )
  engWeek <- cbind.data.frame(week,engagementWeek)
 # plot = qplot(eng$date,eng$engagement, geom=c("point", "smooth"), group=1, main = "Engagement per week", xlab = "Week Number", ylab = "Engagement rate") + scale_y_continuous(labels=scales::percent)
  return(engWeek)
}

averageEngagement <- function(dfLogs,user){
  engagement <- weeklyEngagement(dfLogs,user)
  avEng <- mean(engagement$engagementWeek)
  return(avEng)
}

progressRate <- function(dfLogs,user){
  rate <- c()
  rate[1] <- 0
  rate[2] <- 0
  for(i in 3:length(prog)){
    if(prog[i-1]==0){
      rate[i] <-  (prog[i] - prog[i-2])/prog[i-2]
    } else {
      rate[i] <-  (prog[i] - prog[i-1])/prog[i-1]
    }
  }
}

bestRate <- function(dfLogs,user){
  rate <- progressRate(dfLogs,user)
  bestWeek <- which.max(rate)
  return(bestWeek)
}

averageCons <- function(dfLogs,user,period){
  
  # average number of cig per day
  df <- dfLogs[dfLogs$User==user,c("Day","Type","WeekNumber")]
  df <- table(df)
  df <- data.frame(df)  
  
  if(period=="week"){
    # average number of cig per day in the week
    df2 <- df[df$Day!="Sunday" & df$Day!="Saturday", c("Day","WeekNumber","Freq")]
    meanCigWeek <- round(mean(df2$Freq), 2)
    return(meanCigWeek)
  } else if(period=="weekend"){
    # average number of cig per day in the week
    df3 <- df[df$Day=="Sunday" | df$Day=="Saturday", c("Day","WeekNumber","Freq")]
    meanCigWeekend <- round(mean(df3$Freq), 2)
    return(meanCigWeekend)
  } else {
    df <- df[df$Type %in% c("Cheated","On time","Behaviour"),c("Day","WeekNumber","Freq")]
    df <- df[df$Freq!=0,c("Day","WeekNumber","Freq")]
    meanCig <- round(mean(df$Freq), 2)
    return(meanCig)
  }
  
}

mostSmoked <- function(dfLogs,user){
  df <- dfLogs[dfLogs$User==user,c("Time","Type")]
  df <- df[df$Type %in% c("Cheated","On time","Behaviour"),c("Time","Type")]
  df$Hour <- strftime(df$Time,format="%H")
  interv <- c(0,6,10,14,18,22,24)
  temp <- cut(as.numeric(df$Hour), breaks=interv, right = FALSE)
  df$Hour <- temp
  df <- table(df)
  df <- data.frame(df) 
  df <- df[df$Freq!=0,c("Hour","Freq")]
  df <- aggregate(df$Freq,by = list(df$Hour), sum)
  maxCons <- max(df$x)
  whichMaxCons <- which.max(df$x)
  slot <- df[df$x==maxCons,]$Group.1
  slot <- levels(slot)[whichMaxCons]
  
  df1 <- dfLogs[dfLogs$User==user,c("Time","Type","Day","WeekNumber")]
  df1 <- df1[df1$Type %in% c("Cheated","On time","Behaviour"),c("Time","Type","Day","WeekNumber")]
  df1$Hour <- strftime(df1$Time,format="%H")
  interv <- c(0,6,10,14,18,22,24)
  temp <- cut(as.numeric(df1$Hour), breaks=interv, right = FALSE)
  df1$Hour <- temp
  df1 <- df1[df1$Hour==slot,c("Hour","WeekNumber")]
  df1 <- table(df1)
  df1 <- data.frame(df1) 
  df1[df1$Freq!=0,]
  df1 <- aggregate(list(Freq=df1$Freq),by = list(Hour=df1$Hour),mean)
  return(sum(df1$Freq))
}

isEngaged <- function(dfLogs,week,user){
  week <- week - 1
  if(week==0) return(FALSE)
  df17 <- dfLogs[dfLogs$User==user & dfLogs$WeekNumber==week,c("Type")]
  df17 <- data.frame(table(df17))
  engagement <- 1 - max(df17[df17$df17=="Auto skipped","Freq"],0)/(max(df17[df17$df17=="Auto skipped","Freq"],0)+ max(df17[df17$df17=="Skipped","Freq"],0)+ max(df17[df17$df17=="On time","Freq"],0) + max(df17[df17$df17=="Snoozed","Freq"],0)  )
  if(is.na(engagement)) return(FALSE)
  if(engagement > 0.4){
    return (TRUE)
  } else return(FALSE)
}



