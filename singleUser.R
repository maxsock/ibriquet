ageCat <- function(user){
  temp1 <- dfLogs[dfLogs$User==user,c("User")]
  df3 <- plyr::rename(dfSurvey,c("Name"="User"))
  temp2 <- df3[df3$User==user,c("Age","User")]
  temp3 <- unique(dfLogs[c("User")])
  temp2 <- merge(x = temp2, y = temp3, by = "User")
  setDT(temp2)
  temp2[Age >0 & Age <30, Category := "Young"]
  temp2[Age >=30 & Age <50, Category := "Middle"]
  temp2[Age >=50, Category := "Old"]
  return(temp2$Category)
}

cigSaved <- function(user){
  df <- dfLogs[dfLogs$User==user & dfLogs$Type %in% c("Behaviour","Cheated","On time"),c("WeekNumber","Type")]
  df <- table(df)
  df <- data.frame(df)  
  df <- df[df$Freq!=0,]
  ref <- df[df$Type=="Behaviour","Freq"]
  df <- df[df$WeekNumber!=0,]
  
  saved <- df[df$Type=="Behaviour","Freq"] - df[df$Type=="Cheated","Freq"] - df[df$Type=="On time","Freq"]  
  df <- aggregate(df$Freq,by = list(df$WeekNumber), sum)
  df <- plyr::rename(df,c("Group.1"="WeekNumber"))
  
  return(sum(df$x))

}

progress <- function(user){
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
    if(!isEngaged(i,user)){
      for(j in (i-1):1){
        if(isEngaged(j,user)){
          cons[i] <- cons[j]
          break;
        }
      }
    }
    averageCons <- (cons[i-1]+cons[i-2]+cons[i-3])/3
    avg[i]= (averageCons - cons[i])/averageCons
  }
  
  # Concatenate the progress in a unique array
  prog <- c(0,y[2:3],avg[4:length(cons)])
  df <- unique(df[c("WeekNumber")])
  df$Progress <- prog
  
  return(df)
  
}

averageProgress <- function(user){
  df <- progress(user)
  avProg <- mean(df$Progress[2:length(df$Progress)])
  return(avProg)
}

progressCat <- function(user){
  avProg <- averageProgress(user)
  if(avProg <= 0.2){
    progCat = "low"
  }else if(avProg >= 0.5){
    progCat = "high"
  }else progCat = "medium"
  
  return(progCat)
}







