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
source("singleUser.R")


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
user = "Inès Delaunay"

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
  cat("cons :",cons)
  for (i in 4:length(cons)){
    cat("i :",i)
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
  
  print(df)
  
} else {
  prog <- c(0,y[1:2])
  df <- unique(df[c("WeekNumber")])
  df$Progress <- prog
  
}
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


user="Hervé Dupuis"

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
round(mean(df1$Freq),2)

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
user="Abel Sharpe"
week=0
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


df14 <- dfLogs[dfLogs$User==user & dfLogs$WeekNumber==23,c("Type")]
if(length(df14)==0){
  return(NULL)
}
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


########################## MODE USAGE OVER ALL PERIOD #################################
type=c("Cheated","On time")
df18 <- dfLogs[dfLogs$Type %in% type & dfLogs$User==user,c("Dateday","Type")]
df18 <- data.frame(table(df18))
df18 <- df18[df18$Freq!=0,]
ggplot(data = df18, aes( x = Dateday, y = Freq, fill = Type ) )+geom_bar( stat = 'identity',position = 'dodge'  )+ggtitle("Mode usage over all period")+theme(axis.text.x = element_text(angle = 60,hjust=1))







df <- dfLogs[dfLogs$Type %in% c("Behaviour","Cheated","On time"),c("User","WeekNumber","Type")]
df <- table(df)
df <- data.frame(df)  
df <- df[df$Freq!=0,]
ref <- df[df$Type=="Behaviour","Freq"]
df <- df[df$WeekNumber!=0,]

saved <- df[df$Type=="Behaviour","Freq"] - df[df$Type=="Cheated","Freq"] - df[df$Type=="On time","Freq"]  
df <- aggregate(df$Freq,by = list(WeekNumber=df$WeekNumber), sum)

length(unique(dfLogs$User))

dftest <- dfLogs[dfLogs$Type %in% c("Behaviour","Cheated","On time"),"User"]
length(dftest)

