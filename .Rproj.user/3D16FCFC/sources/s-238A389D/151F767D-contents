cleanData <- function(df){
  df$Time <- strptime(df$Time,format="%d/%m/%Y %H:%M")
  df$Day <- weekdays(as.Date(df$Time))
  df$Week <- week(df$Time)
  
  tmp1 <- df
  tmp2 <- tmp1[tmp1$Type=="Behaviour",c("User","Time")]
  
  tmp2 <- tmp2[!duplicated(tmp2[,"User"]),]
  tmp1 <- merge(x=tmp1, y=tmp2, by="User", all = TRUE)
  tmp1$WeekNumber <- time_length(interval(start = tmp1$Time.y, end = tmp1$Time.x), unit = "weeks") 
  tmp1$WeekNumber <- floor(tmp1$WeekNumber)
  tmp1 <- plyr::rename(tmp1,c("Time.x"="Time"))
  tmp1 <- select(tmp1,"User","Time","WeekNumber")
  df <- merge(x=df,y=tmp1, by=c("User","Time"))

  return(df)
}
