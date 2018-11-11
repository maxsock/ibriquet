cleanLogs <- function(df){
  
  # Transform the date string to the right format
  df$Time <- strptime(df$Time,format="%d/%m/%Y %H:%M")
  
  # Extract days and weeks
  df$Day <- weekdays(as.Date(df$Time))
  df$Week <- week(df$Time)
  
  # Extract the weeknumber since the user started his behavior week
  tmp1 <- df
  tmp2 <- tmp1[tmp1$Type=="Behaviour",c("User","Time")]
  tmp2 <- tmp2[!duplicated(tmp2[,"User"]),]
  tmp1 <- merge(x=tmp1, y=tmp2, by="User", all = TRUE)
  tmp1$WeekNumber <- time_length(interval(start = tmp1$Time.y, end = tmp1$Time.x), unit = "weeks") 
  tmp1$WeekNumber <- floor(tmp1$WeekNumber)
  tmp1 <- plyr::rename(tmp1,c("Time.x"="Time"))
  tmp1 <- select(tmp1,"User","Time","WeekNumber")
  df <- merge(x=df,y=tmp1, by=c("User","Time"))
  
  
  # Delete user who didn't have good data
  df <- df[df$User!="William Beauregard",]
  df <- df[df$User!="Marc Gaumont",]
  df <- df[df$User!="Joseph Toussaint",]
  df <- df[df$User!="Wilfried Piaget",]
  
  # Delete users who didn't use the lighter for more than 4 weeks
  tempLength <-  aggregate(df$WeekNumber, by = list(df$User), max)
  tempLength <- plyr::rename(tempLength,c("Group.1"="User"))
  df <- merge(x=df,y=tempLength, by=c("User"))
  df <- plyr::rename(df,c("x"="TotalWeek"))
  df <- df[df$TotalWeek>4,]
  
  # Delete users where there's 6 times more "auto skipped" than "on time" (not using the lighter properly)
  skipped <- data.frame(table(df1[df1$Type=="Auto skipped",c("User")]))
  time <- data.frame(table(df1[df1$Type=="On time",c("User")]))
  test <- merge(x=time,y=skipped, by=c("Var1"))
  test <- test[test$Freq.y<6*test$Freq.x,]
  df <- df[df$User %in% test$Var1,]
  
  

  return(df)
}

cleanSurvey <- function(df){
  # Replace the spaces with dots
  names(df)<-str_replace_all(names(df), c(" " = "." , "," = "" ))
  
  # Rename columns and reduce features
  df <- plyr::rename(df,c("How.much.do.you.weigh?.(kg)"="Weight"))
  df <- plyr::rename(df,c("What.is.your.height?.(cm)"="Height"))
  df <- plyr::rename(df,c("How.many.cigarettes.do.you.smoke.per.day"="CigPerDay"))
  df$BMI <- df$Weight/(df$Height/100)**2
  
  return(df)
}
