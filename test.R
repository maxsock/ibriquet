plotFunction <- function(df,user){
 return (renderPlot({
    barplot(table(df[df$User==user,"Type"]),main="Number of cigarettes per type")
  }) )
}

engagement <- function(df){
  x <- df[c("User","Week")]
  x <- data.frame(table(x))
  y <-x[x$Freq>0,c("Freq","Week")]
  y <- table(y$Week)
  y <- as.numeric(y)
  y <- y/36*100
  y <- y[!is.infinite(y)]
  z <- 1:length(y)
  lo <- loess(y~z)
  return (renderPlot({
    plot(y, main="Engagement", xlab = "Week number",ylab = "Engagement (%)")
     lines(predict(lo),type="l",col="red")
  }))
  
}

progRate <- function(df,user){
  tryCatch(
    {
  x <- df[c("User","Type","Week")]
  x <- data.frame(table(x))
  x <- x[x$User==user,c("User","Type","Week","Freq")]
  y <-x[x$Type=="Cheated","Freq"]+x[x$Type=="On time","Freq"]
  
  
  y <- (mean(na.exclude(x[x$Type=="Behaviour","Freq"])))/y*100
  y <- y[!is.infinite(y)]
  z <- 1:length(y)
  lo <- loess(y~z)
  return (renderPlot({
    plot(y, main="Average progress rate per week", xlab = "Week number",ylab = "Progress rate (%)")
    lines(predict(lo),type="l",col="red")}))
    },
  error = function(e) {})
  
}

dailyCons <- function(df){
  df1 <- df[c("User","Day")]
  df2 <- data.frame(table(df1))
  df2$Day <- factor(df2$Day, levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df2 <- df2[order(df2$Day), ]
  return (renderPlot({boxplot(Freq ~ Day,df2,main="Mean and Std of Cigarette Consumption per weekday")}))
}