library(shiny)
library(data.table)
setwd("/Users/maximiliensock/ibriquet/")

source("cleaning.R")


df1 <- read.csv("/Users/maximiliensock/ibriquet/logs.csv",
                header = TRUE,
                sep = ";",fileEncoding = "MACROMAN")
                
                
df2 <- read_excel("/Users/maximiliensock/ibriquet/surveydataece.xlsx")

write.csv(df2, file = "test.csv")
df1$Time <- strptime(df1$Time,format="%d/%m/%Y %H:%M")
#df1$Time <- substring(df1$Time,0,7)
df1$Day <- weekdays(as.Date(df1$Time))
df1$Month <- substring(df1$Time,0,7)
y <- table(df1[df1$User=="Rémi Dubost","Day"])
x <- table(df1[df1$User!="Rémi Dubost","Day"])
z <- df1[c("User","Day")]
zz <- data.frame(table(z))
#zzz <- aggregate(Freq ~ Day,zz,mean)
zz$Day <- factor(zz$Day, levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
zz <- zz[order(zz$Day), ]
#setcolorder(zz,c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
boxplot(Freq ~ Day,zz)
df3 <- data.frame(zz) 
zz <- df2[c("Start Date","Name")]
zz$Week <- week(zz$`Start Date`)
zz <- zz[zz$Name=="Rémi Dubost","Week"]
df1$Week <- week(df1$Time)

user="Rémi Dubost"
x <- df1[c("User","Week")]
x <- data.frame(table(x))
y <-x[x$Freq>0,c("Freq","Week")]
y <- table(y$Week)
y <- as.numeric(y)
y <- y/36*100
y <- y[!is.infinite(y)]
z <- 1:length(y)
lo <- loess(y~z)
plot(y, main="Engagement", xlab = "Week number",ylab = "Engagement (%)")
lines(predict(lo),type="l",col="red")
