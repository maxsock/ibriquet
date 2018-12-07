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
  
  dfWeek <- dfLogs[dfLogs$User==user,c("TotalWeek")]
  total <- dfWeek[1]
  
  
  # Calculate the progress for week 1 and 2
  y <-df[df$Type=="Cheated","Freq"]+df[df$Type=="On time","Freq"]
  y <- y[y!=0]
  behavior <- (sum(df[df$Type=="Behaviour","Freq"]))
  y <- (behavior-y)/behavior
  y <- y[!is.infinite(y)]
  
  if(total>2){
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
    
  } else {
    prog <- c(0,y[1:2])
    df <- unique(df[c("WeekNumber")])
    df$Progress <- prog
    return(df)
  }
  
}

# AVERAGE PROGRESS
averageProgress <- function(dfLogs,user){
  df <- progress(dfLogs,user)
  avProg <- round(mean(df$Progress[2:length(df$Progress)])*100,2)
  progCat <- progressCat(dfLogs,user,avProg/100)
  return(renderValueBox({
    valueBox(
      paste0(avProg,"%")
      ,paste0("Average Progress: ",progCat)
      ,icon = icon("export",lib='glyphicon')
      ,color = "green")  
  }))
}

# PROGRESS CATEGORY
progressCat <- function(dfLogs,user,avProg){
  if(avProg <= 0.2){
    progCat = "Low"
  }else if(avProg >= 0.5){
    progCat = "High"
  }else progCat = "Medium"
  
  return(progCat)
}

weeklyEngagement <- function(dfLogs,user){
  df <- dfLogs[dfLogs$User==user & dfLogs$WeekNumber!=0,c("WeekNumber","Type")]
  df <- data.frame(table(df))
  week <- unique(df$WeekNumber)
  for (i in 1:length(week)){
    engagementWeek[i] <- 1 - max(0,df[df$Type=="Auto skipped" & df$WeekNumber==i,"Freq"])/(max(0,df[df$Type=="Auto skipped" & df$WeekNumber==i,"Freq"])+ max(0,df[df$Type=="Skipped" & df$WeekNumber==i,"Freq"]) + max(0,df[df$Type=="On time" & df$WeekNumber==i,"Freq"]) + max(0,df[df$Type=="Snoozed" & df$WeekNumber==i,"Freq"]) )
  }
  engWeek <- cbind.data.frame(week,engagementWeek)
  return(engWeek)
}

averageEngagement <- function(dfLogs,user){
  engagement <- weeklyEngagement(dfLogs,user)
  avEng <- round(mean(engagement$engagementWeek),2)*100
  return(renderValueBox({
    valueBox(
      paste0(avEng,"%")
      ,"Average Engagement"
      ,icon = icon("export",lib='glyphicon')
      ,color = "blue")  
  }))
}

progressRate <- function(dfLogs,user){
  rate <- c()
  rate[1] <- 0
  rate[2] <- 0
  prog <- progress(dfLogs,user)$Progress
  for(i in 3:length(prog)){
    if(prog[i-1]==0){
      rate[i] <-  (prog[i] - prog[i-2])/prog[i-2]
    } else {
      rate[i] <-  (prog[i] - prog[i-1])/prog[i-1]
    }
  }
  return(rate)
}

bestRate <- function(dfLogs,user){
  rate <- progressRate(dfLogs,user)
  bRate <- round(max(rate,na.rm = TRUE),2)*100
  bestWeek <- which.max(rate)
  return(renderValueBox({
    valueBox(
      paste0(bRate,"%")
      ,paste0("Best Progress Rate on week ",bestWeek)
      ,icon = icon("star",lib='glyphicon')
      ,color = "orange")  
  }))
}

averageCons <- function(dfLogs,user,period){
  
  # average number of cig per day
  df <- dfLogs[dfLogs$User==user,c("Day","Type","WeekNumber")]
  df <- table(df)
  df <- data.frame(df)  
  df <- df[df$Type %in% c("Cheated","On time","Behaviour"),c("Day","WeekNumber","Freq")]
  df <- df[df$Freq!=0,c("Day","WeekNumber","Freq")]
  
  
  if(period=="week"){
    # average number of cig per day in the week
    df2 <- df[df$Day!="Sunday" & df$Day!="Saturday", c("Day","WeekNumber","Freq")]
    meanCigWeek <- round(mean(df2$Freq), 2)
    return(renderValueBox({
      valueBox(
        meanCigWeek
        ,"Cigarette smoked per week day "
        ,icon = icon("stats",lib='glyphicon')
        ,color = "aqua")  
    }))
  } else if(period=="weekend"){
    # average number of cig per day in the week
    df3 <- df[df$Day=="Sunday" | df$Day=="Saturday", c("Day","WeekNumber","Freq")]
    meanCigWeekend <- round(mean(df3$Freq), 2)
    return(renderValueBox({
      valueBox(
        meanCigWeekend
        ,"Cigarette smoked per weekend day "
        ,icon = icon("stats",lib='glyphicon')
        ,color = "teal")  
    }))
  } else {
    meanCig <- round(mean(df$Freq), 2)
    return(renderValueBox({
      valueBox(
        meanCig
        ,"Cigarette smoked per day "
        ,icon = icon("stats",lib='glyphicon')
        ,color = "lime")  
    }))
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
  return(renderValueBox({
    valueBox(
      round(mean(df1$Freq),2)
      ,paste0("Cigarette smoked in the slot ",slot)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "maroon")  
  }))
  
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

cigCons <- function(dfLogs,user){
  df11 <- dfLogs[dfLogs$User==user,c("WeekNumber","Day","Type")]
  df11 <- table(df11)
  df11 <- data.frame(df11)  
  df11 <- df11[df11$Type %in% c("Cheated","On time","Behaviour"),c("WeekNumber","Day","Freq")]
  df11 <- aggregate(df11$Freq,by = list(df11$Day), sum)
  df11$Group.1 <- factor(df11$Group.1, levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df11 <- df11[order(df11$Group.1), ]
  return(renderPlot({qplot(Group.1, x, data = df11, geom=c("point", "smooth"), group=1,xlab = "day",ylab = "quantity")}))
  
}

getmaxWeek <- function(df,user){
  # Extract the total number of week for a user
  df <- df[df$User==user,c("TotalWeek")]
  return(df[1])
}

meanCigCons <- function(dfLogs,user){
  df <- dfLogs[dfLogs$User==user,c("WeekNumber","Day","Type")]
  df <- data.frame(table(df))
  df <- df[df$Type %in% c("Cheated","On time","Behaviour"),c("WeekNumber","Day","Freq")]
  df <- df[df$Freq!=0,c("Day","Freq")]
  df$Day <- factor(df$Day, levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df <- df[order(df$Day), ]
  
  return(renderPlot({qplot(Day, Freq, data = df, geom=c("boxplot"))}))
}

overallProgress <- function(dfLogs,user){
  prog <- progress(dfLogs,user)
  return(renderPlot({qplot(WeekNumber, Progress*100, data = prog, geom=c("point", "smooth"), group=1,xlab = "Week Number",ylab = "Progress (%)")}))
}

progRate <- function(dfLogs,user){
  rate <- progressRate(dfLogs,user)
  z <- 1:length(rate)
  
  return(renderPlot({qplot(z, rate, geom=c("point","smooth"),xlab = "Week Number",ylab = "Rate (%)")}))
}

cigSlot <- function(dfLogs,week,user){
  df <- dfLogs[dfLogs$User==user & dfLogs$WeekNumber==week,c("Time","Day","Type")]
  if(nrow(df)==0){
    return(NULL)
  }
  df <- data.frame(table(df))
  df$Hour <- strftime(df$Time,format="%H")
  interv <- c(0,2,4,6,8,10,12,14,16,18,20,22,24)
  temp <- cut(as.numeric(df$Hour), breaks=interv, right = FALSE)
  df$Hour <- temp
  df <- df[df$Type %in% c("Cheated","On time","Behaviour"),c("Hour","Day","Freq")]
  df <- aggregate(list(Freq=df$Freq),by = list(Hour=df$Hour,Day=df$Day), sum)
  df$Day <- factor(df$Day, levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df <- df[order(df$Day), ]
  return(renderPlot({ggplot(data = df, aes( x = Day, y = Freq , fill=Hour) )+geom_bar( stat = 'identity',position = 'dodge')}))
  
}

cigWeek <- function(dfLogs,week,user){
  df <- dfLogs[dfLogs$User==user & dfLogs$WeekNumber==week,c("Day","Type")]
  if(nrow(df)==0){
    return(NULL)
  }
  df <- data.frame(table(df))
  df <- df[df$Type %in% c("Cheated","On time","Behaviour"),c("Day","Freq")]
  df <- df[df$Freq!=0,c("Day","Freq")]
  df <- aggregate(list(Freq=df$Freq),by = list(Day=df$Day), sum)
  df$Day <- factor(df$Day, levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df <- df[order(df$Day), ]
  return(renderPlot({qplot(df$Day, df$Freq, geom=c("point","smooth"),group=1,xlab = "Day",ylab = "Consumption")}))
  
}

modeWeek <- function(dfLogs,week,user){
  df <- dfLogs[dfLogs$User==user & dfLogs$WeekNumber==week,c("Type")]
  if(length(df)==0){
    return(NULL)
  }
  df <- data.frame(table(df))
  return(renderPlot({ggplot(data = df, aes( x = df, y = Freq, fill=df) )+geom_bar( stat = 'identity',position = 'dodge'  )}))
  
}

overallEngagement <- function(dfLogs,user){
  eng <- weeklyEngagement(dfLogs,user)
  return(renderPlot({qplot(eng$week,eng$engagementWeek, geom=c("point", "smooth"), group=1, xlab = "Week Number", ylab = "Engagement rate") + scale_y_continuous(labels=scales::percent)}))
  
}

overallMode <- function(dfLogs,user){
  type=c("Cheated","On time","Behaviour","Skipped","Auto skipped","Friend")
  df <- dfLogs[dfLogs$Type %in% type & dfLogs$User==user,c("Dateday","Type")]
  df <- data.frame(table(df))
  df <- df[df$Freq!=0,]
  return(renderPlot({ggplot(data = df, aes( x = Dateday, y = Freq, fill = Type ) )+geom_bar( stat = 'identity',position = 'dodge'  )+theme(axis.text.x = element_text(angle = 60,hjust=1))}))
  
}