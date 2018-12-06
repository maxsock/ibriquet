library(shiny)
library(shinydashboard)
library(readxl)
library(shinyjs)
library(lubridate)
library(data.table)
library(dplyr)
library(plyr)
library(bda)
library(tools)
library(stringr)
library(ggplot2)
#setwd("C:/Users/Lisa/Documents/GitHub/ibriquet")
setwd("/Users/maximiliensock/ibriquet")

source("cleaning.R")


################################################ LOADING DATA  #####################################################################
df1 <- read.csv("/Users/maximiliensock/ibriquet/logs.csv",
                header = TRUE,
                sep = ";",fileEncoding = "MACROMAN")


df2 <- read_excel("/Users/maximiliensock/ibriquet/surveydataece.xlsx")


################################################    CLEANING   ###########################################################
dfLogs <- cleanLogs(df1);
dfSurvey <- cleanSurvey(df2)


user="Hervé Dupuis"
########################## AGE CATEGORY ###########################################

temp1 <- dfLogs[dfLogs$User==user,c("User")]
df3 <- plyr::rename(dfSurvey,c("Name"="User"))
temp2 <- df3[df3$User==user,c("Age","User")]
temp3 <- unique(dfLogs[c("User")])
temp2 <- merge(x = temp2, y = temp3, by = "User")
setDT(temp2)
temp2[Age >0 & Age <30, Category := "Young"]
temp2[Age >=30 & Age <50, Category := "Middle"]
temp2[Age >=50, Category := "Old"]
temp2$Category
nrow(temp2)
#returns dataframe with age and category

########################## CIGARETTES SAVED ################################################

df4 <- dfLogs[dfLogs$User==user & dfLogs$Type %in% c("Behaviour","Cheated","On time"),c("WeekNumber","Type")]
df4 <- table(df4)
df4 <- data.frame(df4)  
df4 <- df4[df4$Freq!=0,]
ref <- df4[df4$Type=="Behaviour","Freq"]
df4 <- df4[df4$WeekNumber!=0,]

saved <- df4[df4$Type=="Behaviour","Freq"] - df4[df4$Type=="Cheated","Freq"] - df4[df4$Type=="On time","Freq"]  
df4 <- aggregate(df4$Freq,by = list(df4$WeekNumber), sum)
df4 <- plyr::rename(df4,c("Group.1"="WeekNumber"))

saved <- sum(df4$x)
#returns number of saved cigarettes


########################## OVERALL PROGRESS ################################################
user = "Étienne Toussaint"

# Extract frequences per type per week
df5 <- dfLogs[dfLogs$User==user,c("Type","WeekNumber")]
df5 <- table(df5)
df5 <- data.frame(df5)  

# Calculate the progress for week 1 and 2
y <-df5[df5$Type=="Cheated","Freq"]+df5[df5$Type=="On time","Freq"]
y <- y[y!=0]
behavior <- (sum(df5[df5$Type=="Behaviour","Freq"]))
y <- (behavior-y)/behavior
y <- y[!is.infinite(y)]

# Calculate the progress for week ≥ 3
cons <- df5[df5$Type=="Cheated","Freq"]+df5[df5$Type=="On time","Freq"]
cons[1] <- df5[df5$Type=="Behaviour","Freq"][1]
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
df5 <- unique(df5[c("WeekNumber")])
df5$Progress <- prog

# return progress rate per week

###### Average progress ######
avProg <- mean(df5$Progress[2:length(df5$Progress)])

###### Progress Category #######
if(avProg <= 0.2){
  progCat = "low"
}else if(avProg >= 0.5){
  progCat = "high"
}else progCat = "medium"




########################## RATE OF PROGRESS #############################################

# ATTENTION utilisation de prog de la fonction précédente 
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
z <- 1:length(rate)

qplot(z, rate, geom=c("point","smooth"),main="Rate of progress",xlab = "Week Number",ylab = "Rate (%)")
bestWeek <- which.max(rate)
# returns the week with the best rate



########################## AVERAGE CIGARETTE CONSUMPTION ######################################

df6 <- dfLogs[dfLogs$User==user,c("Day","Type","WeekNumber")]
df6 <- table(df6)
df6 <- data.frame(df6)  
df6 <- df6[df6$Type %in% c("Cheated","On time","Behaviour"),c("Day","WeekNumber","Freq")]
df6 <- df6[df6$Freq!=0,c("Day","WeekNumber","Freq")]
meanCig <- round(mean(df6$Freq), 2)

# returns the average number of cig per day

df7 <- df6[df6$Day!="Sunday" & df6$Day!="Saturday", c("Day","WeekNumber","Freq")]
meanCigWeek <- round(mean(df7$Freq), 2)

# returns the average number of cig per day in the week

df8 <- df6[df6$Day=="Sunday" | df6$Day=="Saturday", c("Day","WeekNumber","Freq")]
meanCigWeekend <- round(mean(df8$Freq), 2)

# returns the average number of cig per day in the week



########################## MOST SMOKING INTENSITY SLOT #################################



df9 <- dfLogs[dfLogs$User==user,c("Time","Type")]
df9 <- df9[df9$Type %in% c("Cheated","On time","Behaviour"),c("Time","Type")]
df9$Hour <- strftime(df9$Time,format="%H")
interv <- c(0,6,10,14,18,22,24)
temp <- cut(as.numeric(df9$Hour), breaks=interv, right = FALSE)
df9$Hour <- temp
df9 <- table(df9)
df9 <- data.frame(df9) 
df9 <- df9[df9$Freq!=0,c("Hour","Freq")]
df9 <- aggregate(df9$Freq,by = list(df9$Hour), sum)
maxCons <- max(df9$x)
whichMaxCons <- which.max(df9$x)
slot <- df9[df9$x==maxCons,]$Group.1
slot <- levels(slot)[whichMaxCons]

df10 <- dfLogs[dfLogs$User==user,c("Time","Type","Day","WeekNumber")]
df10 <- df10[df10$Type %in% c("Cheated","On time","Behaviour"),c("Time","Type","Day","WeekNumber")]
df10$Hour <- strftime(df10$Time,format="%H")
interv <- c(0,6,10,14,18,22,24)
temp <- cut(as.numeric(df10$Hour), breaks=interv, right = FALSE)
df10$Hour <- temp
df10 <- df10[df10$Hour==slot,c("Hour","WeekNumber")]
df10 <- table(df10)
df10 <- data.frame(df10) 
df10[df10$Freq!=0,]
df10 <- aggregate(list(Freq=df10$Freq),by = list(Hour=df10$Hour),mean)
sum(df10$Freq)


########################## CIGARETTE CONSUMPTION PER WEEKDAY #################################
week = 1

df11 <- dfLogs[dfLogs$User==user & dfLogs$WeekNumber==week,c("Day","Type")]
df11 <- table(df11)
df11 <- data.frame(df11)  
df11 <- df11[df11$Type %in% c("Cheated","On time","Behaviour"),c("Day","Freq")]
df11 <- aggregate(df11$Freq,by = list(df11$Day), sum)
df11$Group.1 <- factor(df11$Group.1, levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
df11 <- df11[order(df11$Group.1), ]

# plot the consumption per day per week


########################## MEAN AND STD OF CIG CONS PER WEEKDAY #################################

df12 <- dfLogs[dfLogs$User==user,c("WeekNumber","Day","Type")]
df12 <- data.frame(table(df12))
df12 <- df12[df12$Type %in% c("Cheated","On time","Behaviour"),c("WeekNumber","Day","Freq")]
df12 <- df12[df12$Freq!=0,c("Day","Freq")]
df12$Day <- factor(df12$Day, levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
df12 <- df12[order(df12$Day), ]

qplot(Day, Freq, data = df12, geom=c("boxplot"),main="Mean and Std of Cigarette Consumption per weekday")

########################## CIGARETTES PER WEEKDAY PER TIME SLOTS #################################


df13 <- dfLogs[dfLogs$User==user & dfLogs$WeekNumber==week,c("Time","Day","Type")]
df13 <- data.frame(table(df13))
df13$Hour <- strftime(df13$Time,format="%H")
interv <- c(0,2,4,6,8,10,12,14,16,18,20,22,24)
temp <- cut(as.numeric(df13$Hour), breaks=interv, right = FALSE)
df13$Hour <- temp
df13 <- df13[df13$Type %in% c("Cheated","On time","Behaviour"),c("Hour","Day","Freq")]
df13 <- aggregate(df13$Freq,by = list(df13$Hour,df13$Day), sum)
ggplot(data = df13, aes( x = Group.2, y = x , fill=Group.1) )+geom_bar( stat = 'identity',position = 'dodge'  )+ggtitle("Cigarettes per weekday per time slot")


########################## MODE USAGE PER WEEK #################################


df14 <- dfLogs[dfLogs$User==user & dfLogs$WeekNumber==week,c("Type")]
df14 <- data.frame(table(df14))
ggplot(data = df14, aes( x = df14, y = Freq ) )+geom_bar( stat = 'identity',position = 'dodge'  )+ggtitle("Mode usage per week")


########################## CIGARETTES PER WEEKDAY PER TIME SLOTS #################################

df15 <- dfLogs[dfLogs$User==user & dfLogs$WeekNumber==week,c("Day","Type")]
df15 <- data.frame(table(df15))
df15 <- df15[df15$Type %in% c("Cheated","On time","Behaviour"),c("Day","Freq")]
df15 <- aggregate(df15$Freq,by = list(df15$Day), sum)
df15$Group.1 <- factor(df15$Group.1, levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
df15 <- df15[order(df15$Group.1), ]
ggplot(data = df15, aes( x = Group.1, y = x ) )+geom_bar( stat = 'identity',position = 'dodge'  )+ggtitle("Cigarettes per weekday")


########################## ENGAGEMENT PER DAY #################################

df16 <- dfLogs[dfLogs$User==user & dfLogs$WeekNumber!=0,c("Dateday","Type")]
df16 <- data.frame(table(df16))
date <- unique(df16$Dateday)
engagementDay <- 1 - df16[df16$Type=="Auto skipped","Freq"]/(df16[df16$Type=="Auto skipped","Freq"]+ df16[df16$Type=="Skipped","Freq"] + df16[df16$Type=="On time","Freq"] + df16[df16$Type=="Snoozed","Freq"] )
engDay <- cbind.data.frame(date,engagementDay)

########################## ENGAGEMENT PER WEEK #################################

df17 <- dfLogs[dfLogs$User==user & dfLogs$WeekNumber!=0,c("WeekNumber","Type")]
df17 <- data.frame(table(df17))
week <- unique(df17$WeekNumber)
engagementWeek <- 1 - df17[df17$Type=="Auto skipped","Freq"]/(df17[df17$Type=="Auto skipped","Freq"]+ df17[df17$Type=="Skipped","Freq"] + df17[df17$Type=="On time","Freq"] + df17[df17$Type=="Snoozed","Freq"] )
engWeek <- cbind.data.frame(week,engagementWeek)
#qplot(eng$date,eng$engagement, geom=c("point", "smooth"), group=1, main = "Engagement per week", xlab = "Week Number", ylab = "Engagement rate (%)")+ scale_y_continuous(labels=scales::percent)
user = "Étienne Toussaint"
isEngaged(18,user)
isEngaged <- function(week,user){
  week <- week - 1
  if(week==0) return(FALSE)
  df17 <- dfLogs[dfLogs$User==user & dfLogs$WeekNumber==week,c("Type")]
  df17 <- data.frame(table(df17))
  if(week==16) print(df17)
  engagement <- 1 - max(df17[df17$df17=="Auto skipped","Freq"],0)/(max(df17[df17$df17=="Auto skipped","Freq"],0)+ max(df17[df17$df17=="Skipped","Freq"],0)+ max(df17[df17$df17=="On time","Freq"],0) + max(df17[df17$df17=="Snoozed","Freq"],0)  )
  print(engagement)
  if(is.na(engagement)) return(FALSEsa)
   if(engagement > 0.4){
    return (TRUE)
  } else return(FALSE)
}

########################## MODE USAGE OVER ALL PERIOD #################################
type=c("Cheated","On time")
df18 <- dfLogs[dfLogs$Type %in% type & dfLogs$User==user,c("Dateday","Type")]
df18 <- data.frame(table(df18))
df18 <- df18[df18$Freq!=0,]
ggplot(data = df18, aes( x = Dateday, y = Freq, fill = Type ) )+geom_bar( stat = 'identity',position = 'dodge'  )+ggtitle("Mode usage over all period")

