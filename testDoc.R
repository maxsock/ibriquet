library(shiny)
library(data.table)
library(dplyr)
library(plyr)
library(lubridate)
library(bda)
library(readxl)
#setwd("C:/Users/Lisa/Documents/GitHub/ibriquet")
setwd("/Users/maximiliensock/ibriquet")

source("cleaning.R")


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

user="Étienne Toussaint"
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



x <- df[c("User","Type","Week")]
x <- data.frame(table(x))
x <- x[x$User==user,c("User","Type","Week","Freq")]
y <-x[x$Type=="Cheated","Freq"]+x[x$Type=="On time","Freq"]


y <- (mean(na.exclude(x[x$Type=="Behaviour","Freq"])))/y*100
y <- y[!is.infinite(y)]
z <- 1:length(y)
lo <- loess(y~z)

################################################ LOADING DATA  #####################################################################
df1 <- read.csv("/Users/maximiliensock/ibriquet/logs.csv",
                header = TRUE,
                sep = ";",fileEncoding = "MACROMAN")


df2 <- read_excel("/Users/maximiliensock/ibriquet/surveydataece.xlsx")


################################################    CLEANING   ###########################################################
names(df2)<-str_replace_all(names(df2), c(" " = "." , "," = "" ))
df2 <- plyr::rename(df2,c("How.much.do.you.weigh?.(kg)"="Weight"))
df2 <- plyr::rename(df2,c("What.is.your.height?.(cm)"="Height"))
df2 <- plyr::rename(df2,c("How.many.cigarettes.do.you.smoke.per.day"="CigPerDay"))

df2$BMI <- df2$Weight/(df2$Height/100)**2




df1$Time <- strptime(df1$Time,format="%d/%m/%Y %H:%M")
df1$Day <- weekdays(as.Date(df1$Time))


tmp1 <- df1
tmp2 <- tmp1[tmp1$Type=="Behaviour",c("User","Time")]
tmp2 <- tmp2[!duplicated(tmp2[,"User"]),]
tmp1 <- merge(x=tmp1, y=tmp2, by="User", all = TRUE)
tmp1$WeekNumber <- time_length(interval(start = tmp1$Time.y, end = tmp1$Time.x), unit = "weeks") 
tmp1$WeekNumber <- floor(tmp1$WeekNumber)
tmp1 <- plyr::rename(tmp1,c("Time.x"="Time"))
tmp1 <- select(tmp1,"User","Time","WeekNumber")
df1 <- merge(x=df1,y=tmp1, by=c("User","Time"))
df1 <- df1[df1$User!="William Beauregard",]
df1 <- df1[df1$User!="Marc Gaumont",]
df1 <- df1[df1$User!="Joseph Toussaint",]
df1 <- df1[df1$User!="Wilfried Piaget",]





############################################################################################################################

#Extract age from the survey and merge it with the logs 
temp1 <- df1[c("User","Type","WeekNumber")]
df2 <- plyr::rename(df2,c("Name"="User"))
temp2 <- df2[c("Age","User")]
temp3 <- unique(df1[c("User")])
temp2 <- merge(x = temp2, y = temp3, by = "User")

# Create Age bins
interv <- pretty(temp2$Age)
temp <- cut(temp2$Age, breaks=interv, right = FALSE)

temp2$Age <- temp

# Give an age bin for each user
freqByBin <- temp2

# Extract the frequency per bins if there's any records
freqByBin[freqByBin==0] <- NA
freqByBin <- na.omit(freqByBin)
freqByBin <- data.frame(table(freqByBin$Age))

# Extract the number of records per age bin per type 
df3 <- merge(x = temp1, y = temp2, by = "User")
df5 <- df3[c("Type","Age")]
df3 <- df3[c("User","Type","Age", "WeekNumber")]
df4 <- aggregate(df3$WeekNumber, by = list(df3$Age,df3$User), max)
df4 <- aggregate(df4$x,by = list(df4$Group.1), mean)
df5 <- data.frame(table(df5))

# Only consider "cheated" and "on time" records
y <-df5[df5$Type=="Cheated","Freq"]+df5[df5$Type=="On time","Freq"]

# Calculate frequency for those types in each age bin
y <- y[y!=0]
y <- y/df4$x
freqByBin <- freqByBin[freqByBin$Freq!=0,c("Var1","Freq")]
y <- y/freqByBin$Freq


# Calculate the number of records for the behavior week as a comparison 
behavior <- df5[df5$Type=="Behaviour","Freq"]
behavior <- behavior[behavior!=0]

# Calculate the progress rate
y <- (behavior-y)/behavior*100
y[is.infinite(y)]<-0
names(y)=freqByBin$Var1



















user="Rémi Dubost"

skipped <- data.frame(table(df1[df1$Type=="Auto skipped",c("User")]))
time <- data.frame(table(df1[df1$Type=="On time",c("User")]))
test <- merge(x=time,y=skipped, by=c("Var1"))
test <- test[test$Freq.y<6*test$Freq.x,]

df <- df1[df1$User %in% test$Var1,]









df1 <- df1[df$User==user,c("User","Day","WeekNumber","Type")]
df <- df1[df1$Type %in% type,]


tempLength <-  aggregate(df1$WeekNumber, by = list(df1$User), max)
tempLength <- plyr::rename(tempLength,c("Group.1"="User"))
df1 <- merge(x=df1,y=tempLength, by=c("User"))
df1 <- df1[df1$x>4,]

type=c("On time","Behaviour")
week=3

df1 <- df1[df1$User==user,c("User","Day","WeekNumber","Type")]
df1 <- df1[df1$Type==type,c("User","Day","WeekNumber")]
df1 <- df1[df1$WeekNumber==week,c("Day")]
df1 <- data.frame(table(df1))
df1 <- df1[order(df1$df1), ]

x <- df[c("User","Type","Week")]
x <- data.frame(table(x))
x <- x[x$User==user,c("User","Type","Week","Freq")]
y <-x[x$Type=="Cheated","Freq"]+x[x$Type=="On time","Freq"]

behavior <- (mean(na.exclude(x[x$Type=="Behaviour","Freq"])))
y <- (behavior-y)/behavior*100
y <- y[!is.infinite(y)]
z <- 1:length(y)
lo <- loess(y~z)





#PROGRESS PER Health Condition
temp1 <- df1[c("User","Type","WeekNumber")]
df2 <- plyr::rename(df2,c("Name"="User", "How would you describe your general health condition? (select answer that applies the best)" = "HealthCond"))
temp2 <- df2[c("HealthCond","User")]
temp3 <- unique(df1[c("User")])
temp2 <- merge(x = temp2, y = temp3, by = "User")

interv <- pretty(temp2$HealthCond)
temp <- cut(temp2$Age, breaks=interv, right = FALSE)

temp2$Age <- temp
freqByBin <- temp2

freqByBin[freqByBin==0] <- NA
freqByBin <- na.omit(freqByBin)
freqByBin <- data.frame(table(freqByBin$Age))
df3 <- merge(x = temp1, y = temp2, by = "User")
df5 <- df3[c("Type","Age")]
df3 <- df3[c("User","Type","Age", "WeekNumber")]
df4 <- aggregate(df3$WeekNumber, by = list(df3$Age,df3$User), max)
df4 <- aggregate(df4$x,by = list(df4$Group.1), mean)
df5 <- data.frame(table(df5))

y <-df5[df5$Type=="Cheated","Freq"]+df5[df5$Type=="On time","Freq"]
y <- y[y!=0]
y <- y/df4$x
freqByBin <- freqByBin[freqByBin$Freq!=0,c("Var1","Freq")]
#y <- y/36)
y <- y/freqByBin$Freq

behavior <- df5[df5$Type=="Behaviour","Freq"]
behavior <- behavior[behavior!=0]
y <- (behavior-y)/behavior*100
y[is.infinite(y)]<-0
names(y)=freqByBin$Var1
barplot(y,main="Progress per age bin compared to the behavior week",xlab = "Age bin",ylab = "Progress rate (%)")



# PROGRESSION PER USER
user="Étienne Toussaint"
x <- df1[c("User","Type","WeekNumber")]
x <- data.frame(table(x))
x <- x[x$User==user,c("User","Type","WeekNumber","Freq")]
y <-x[x$Type=="Cheated","Freq"]+x[x$Type=="On time","Freq"]
y <- y[y!=0]
behavior <- (sum(x[x$Type=="Behaviour","Freq"]))
y <- (behavior-y)/behavior*100
y <- y[!is.infinite(y)]

z <- 1:length(y)
lo <- loess(y~z)
plot(y, main="Average progress rate per week", xlab = "Week number",ylab = "Progress rate (%)")
lines(predict(lo),type="l",col="red")
















observeEvent(input$uploadButton,{dfLogs <- read.csv("/Users/maximiliensock/ibriquet/logs.csv",
                                                    header = TRUE,
                                                    sep = ";",fileEncoding = "MACROMAN")


dfSurvey <- read_excel("/Users/maximiliensock/ibriquet/surveydataece.xlsx")
readCSV(dfLogs,dfSurvey)
})








checkboxGroupInput("type", "Type of record: ",
                   c("Behaviour","Friend","On time","Skipped","Auto skipped","Cheated"),selected = c("Behaviour","Friend","On time","Skipped","Auto skipped","Cheated")),

