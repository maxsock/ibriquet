cleanData <- function(df){
  df$Time <- strptime(df$Time,format="%d/%m/%Y %H:%M")
  df$Day <- weekdays(as.Date(df$Time))
  df$Week <- week(df$Time)
  
  return(df)
}
