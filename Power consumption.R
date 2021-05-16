
#Attribute Information:
  
#1.date: Date in format dd/mm/yyyy
#2.time: time in format hh:mm:ss
#3.global_active_power: household global minute-averaged active power (in kilowatt)
#4.global_reactive_power: household global minute-averaged reactive power (in kilowatt)
#5.voltage: minute-averaged voltage (in volt)
#6.global_intensity: household global minute-averaged current intensity (in ampere)
#7.sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). 
#It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
#8.sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). 
#It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
#9.sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). 
#It corresponds to an electric water-heater and an air-conditioner.
# https://brianward1428.medium.com/introduction-to-tidyverse-7b3dbf2337d5 

#=================================================================
#load libraries 
#=================================================================

library(RMariaDB) 
library(caret)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(corrplot)
library(GGally)
library(psych)
library(reshape)
library(dplyr)
library(plotly)
library(ggfortify)
library(fastDummies)
library(reshape2)
library(superml)
library(forecast)
#=================================================================
#Connection
#=================================================================
con = dbConnect(MariaDB(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
#list the tables 
dbListTables(con)
#List the features 
dbListFields(con,'iris')
#query database to download data
irisALL <- dbGetQuery(con, "SELECT * FROM iris")
#select features and download data 
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

#Using the dbListFields function learn the attributes associated with the yr_2006 table.
dbListFields(con,'yr_2006')
#select data and downlaod
yr_2006 <-dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2006")
yr_2007 <-dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2007")
yr_2008 <-dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2008")
yr_2009 <-dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2009")
yr_2010 <-dbGetQuery(con, "SELECT Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3 FROM yr_2010")

UD_finder_2006 <- dbGetQuery(con, "SELECT distinct Month(Date) FROM yr_2006")
View(UD_finder_2006)
UD_finder_2007 <- dbGetQuery(con, "SELECT distinct Month(Date) FROM yr_2007")
View(UD_finder_2007)
UD_finder_2008 <- dbGetQuery(con, "SELECT distinct Month(Date) FROM yr_2008")
View(UD_finder_2008)
UD_finder_2009 <- dbGetQuery(con, "SELECT distinct Month(Date) FROM yr_2009")
View(UD_finder_2009)
UD_finder_2010 <- dbGetQuery(con, "SELECT distinct Month(Date) FROM yr_2010")
View(UD_finder_2010)




str(yr_2006) 
summary(yr_2006)
head(yr_2006) 
tail(yr_2006) # covers only December

str(yr_2007)
summary(yr_2007)
head(yr_2007) 
tail(yr_2007) # covers till December
View(yr_2007)
str(yr_2008)
summary(yr_2008)
head(yr_2008) 
tail(yr_2008) # covers till December

str(yr_2009)
summary(yr_2009)
head(yr_2009) 
tail(yr_2009)  # covers till December

str(yr_2010)
summary(yr_2010)
head(yr_2010) 
tail(yr_2010) # does not cover till December 


#=================================================================
#Data selected 
#=================================================================
## Combine tables into one dataframe using dplyr
newDF <- rbind(yr_2007, yr_2008, yr_2009)

View(newDF)

str(newDF)
summary(newDF)
head(newDF) 
tail(newDF)

## Combine Date and Time attribute values in a new attribute column
newDF <-cbind(newDF,paste(newDF$Date,newDF$Time), stringsAsFactors=FALSE)

## Give the new attribute in the 6th column a header name 

colnames(newDF)[6] <-"DateTime"
## Move the DateTime attribute within the dataset
newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF)-1))]
head(newDF)

## Convert DateTime from character to POSIXct 
newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S")
## Add the time zone
#attr(newDF$DateTime, "tzone") <- "Europe/Paris"
attr(newDF$DateTime, "tzone") <- "GMT"

## Inspect the data types
str(newDF)


#=================================================================
#lubridate
#=================================================================

## Create "year" attribute with lubridate
newDF$year <- year(newDF$DateTime)
newDF$month <- month(newDF$DateTime)
newDF$day <- day(newDF$DateTime)
newDF$hour <- hour(newDF$DateTime)
newDF$minute <- minute(newDF$DateTime)
newDF$week <- week(newDF$DateTime)
newDF$weekDay <- weekdays(newDF$DateTime)
## Move the attributes - run 7 times
newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF)-1))]
df <- newDF
print(newDF$weekDay)
label <- LabelEncoder$new()
newDF$weekDay <- label$fit_transform(newDF$weekDay)

df_pc <- newDF
View(newDF)
summary(newDF)
#=================================================================
#Remove DateTime and Date 
#=================================================================
df_pc$DateTime <- NULL
df_pc$Date <- NULL
df_pc$Time <- NULL
View(df_pc)

str(df_pc)
summary(df_pc)
head(df_pc) 
tail(df_pc)
#=================================================================
#Analyzing Summary Statistics
#=================================================================
psych::describe(df_pc)

#=================================================================
#correlation 
#=================================================================
corrData <- cor(df_pc)
corrData
corrplot(corrData)
#observation of the plot - sub meter 1 has positive correlation with hour, sub meter 2 and 3 
#sub meter 2 is negatively correlated to year, positive  with hour, sub meter 1 and 3
# sub meter 3 is positive with year, hour,sub meter 1 and 2.
# hour and year are important 
# Week and Month are highly correlated and week is positive with day. 
#=================================================================
#Checking Outliers Using Boxplots
#=================================================================
meltData <- melt(df_pc)
p <- ggplot(meltData, aes(factor(variable), value))
p + geom_boxplot() + facet_wrap(~variable, scale="free")

plot(df_pc$Sub_metering_1) # has some gap but Kitchen is used most of the days. 
plot(df_pc$Sub_metering_2)# Laundry is used more than kitchen- one appliance uses more power I think its refrigerator 
plot(df_pc$Sub_metering_3) # uses less energy than kitchen and laundry. very few days they energy usage goes up.


#playing with dplyr 

filter(df_pc, month==7, year==2008)

# chaining method
df_pc %>%
  select(Sub_metering_1) 

df_pc %>%
  group_by(year) %>%
  summarise_each(list(mean), Sub_metering_1, Sub_metering_2,Sub_metering_3)


MeanSummary <- df_pc %>%
  group_by(month) %>%
  filter(between(month,3,10)) %>%
summarise_each(list(mean), Sub_metering_1, Sub_metering_2,Sub_metering_3)


par(mfrow=c(3,1))
plot(MeanSummary$month,MeanSummary$Sub_metering_1, ylab="Sub Meter 1 Mean", xlab="Month",type="l", lwd=2,main="submeter1 Mar - Oct Mean",col="orange",ylim=c(0,2))
plot(MeanSummary$month,MeanSummary$Sub_metering_2, ylab="Sub Meter 2 Mean", xlab="Month",type="l", lwd=2,main="submeter2 Mar - Oct Mean",col="blue",ylim=c(0,2))
plot(MeanSummary$month,MeanSummary$Sub_metering_3, ylab="Sub Meter 3 Mean", xlab="Month",type="l", lwd=2,main="submeter3 Mar - Oct Mean",col="red",ylim=c(0,10))


df_pc %>%
group_by(month, year) %>%
  filter(year==2009) %>%
  filter(between(month,3,10)) %>%
  summarise_each(list(mean), Sub_metering_1, Sub_metering_2,Sub_metering_3)

#Subsetting and Meaningful Time Periods

## Subset the second week of 2008 - All Observations
houseWeek <- filter(df_pc, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1) # kitcehn is used mostly in the evening times 
plot(houseWeek$Sub_metering_2) # morning times and evening times laundry with fridge running all times 
plot(houseWeek$Sub_metering_3) # AC and Water being used more in the morning and evening times. 

housemonth <- filter(df_pc, year == 2008 & between(month,3,9))
plot(housemonth$Sub_metering_1)
plot(housemonth$Sub_metering_2)
plot(housemonth$Sub_metering_3)

#Visualize a Single Day with Plotly

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(newDF, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, type = 'scatter', mode = 'lines')
# What does sub-meter 1 correspond with in this home?
# They used the kitchen between 6 to 7 pm on 9th Jan 2008. 


## Subset of 2008 summer  - All observations
housesummer <- filter(newDF, year == 2008 & between(month,3,9))
## Plot sub-meter 1
plot_ly(housesummer, x = ~housesummer$DateTime, y = ~housesummer$Sub_metering_1, type = 'scatter', mode = 'lines')
plot_ly(housesummer, x = ~housesummer$DateTime, y = ~housesummer$Sub_metering_2, type = 'scatter', mode = 'lines')
plot_ly(housesummer, x = ~housesummer$DateTime, y = ~housesummer$Sub_metering_3, type = 'scatter', mode = 'lines')


## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#Reducing Granularity
## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(newDF, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#So what can you learn from this visualization?
#At 6 am they use the water and from 7 am to 1 pm they use the AC. They cook before 6 pm. They use the water or AC at 9 pm. 

#What peaks might represent the water heater? 
#morning peaks or the night peaks
#How about the AC? 
# constant usuage between 8 to 1 pm maybe considered as AC. 
#What could be happening in the laundry room? 
# Refrigerator running at intervals 
#How many times during this day are kitchen appliances being used? 
# Only once in the evening  
#Lastly, in your opinion, does the data from these three sub-meters contain useful information for the homeowner? 
#somewhat, we can tell that they were in the house using the Ac/heater and in the evening cooked dinner. 


## Subset the 9th day of January 2007 - 10 Minute frequency
houseDay2007 <- filter(newDF, year == 2007 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))
## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay2007, x = ~houseDay2007$DateTime, y = ~houseDay2007$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay2007$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay2007$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


## Subset the 9th day of January 2009 - 10 Minute frequency
houseDay2009 <- filter(newDF, year == 2009 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))
## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay2009, x = ~houseDay2009$DateTime, y = ~houseDay2009$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay2009$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay2009$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


#week Visualization 
houseWeek2008<- filter(newDF, year == 2008 & week == 3 & (day == 15 | day == 16 | day == 17 | day == 18 | day == 19 | day == 20| day == 21))
plot_ly(houseWeek2008, x = ~houseWeek2008$DateTime, y = ~houseWeek2008$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek2008$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek2008$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Jan Week 3 2008",
         xaxis = list(title = "Week"),
         yaxis = list (title = "Power (watt-hours)"))

houseWeek2009<- filter(newDF, year == 2009 & week == 3)
plot_ly(houseWeek2009, x = ~houseWeek2009$DateTime, y = ~houseWeek2009$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek2009$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek2009$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Jan Week 3 2009",
         xaxis = list(title = "Week"),
         yaxis = list (title = "Power (watt-hours)"))

houseWeek2007<- filter(newDF, year == 2007 & week == 3)
plot_ly(houseWeek2007, x = ~houseWeek2007$DateTime, y = ~houseWeek2007$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseWeek2007$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek2007$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Jan Week 3 2007",
         xaxis = list(title = "Week"),
         yaxis = list (title = "Power (watt-hours)"))

  
#Month Visualization = January 
houseMonth<- filter(newDF, year == 2008 & month == 1)
View(houseMonth)
## Plot sub-meter 1, 2 and 3 with title, legend and labels
plot_ly(houseMonth, x = ~houseMonth$DateTime, y = ~houseMonth$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseMonth$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseMonth$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January, 2008",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (watt-hours)"))

houseMonth2007 <- filter(newDF, year == 2007 & month == 1)
## Plot sub-meter 1, 2 and 3 with title, legend and labels
plot_ly(houseMonth2007, x = ~houseMonth2007$DateTime, y = ~houseMonth2007$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseMonth2007$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseMonth2007$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January, 2007",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (watt-hours)"))

houseMonth2009 <- filter(newDF, year == 2009 & month == 1)
## Plot sub-meter 1, 2 and 3 with title, legend and labels
plot_ly(houseMonth2009, x = ~houseMonth2009$DateTime, y = ~houseMonth2009$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseMonth2009$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseMonth2009$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January, 2009",
         xaxis = list(title = "Month"),
         yaxis = list (title = "Power (watt-hours)"))

#Create a visualization for a time period of your choice. Both "Day" and "Week" highlight typical patterns in a home. 
#What might be another period of time that could provide insights?
#Use plotly and experiment with granularity until you find the visualization that maximizes information gain for the viewer. 

HouseWeekDay <- filter(newDF, year == 2008 & (week == 2 | week == 3) & (day == 9 | day == 16))

plot_ly(HouseWeekDay, x = ~HouseWeekDay$DateTime, y = ~HouseWeekDay$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~HouseWeekDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~HouseWeekDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January, 2008 - week days",
         xaxis = list(title = "Week Day"),
         yaxis = list (title = "Power (watt-hours)"))


#Optional Work 
WeekDay_year <- filter(df_pc, year == 2008 & (week == 2 | week == 3) & (day == 9 | day == 16))
#Produce pie chart visualizations that are likely to provide insight, e.g.,
mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
ggplot(WeekDay_year, aes(x = "", y = Sub_metering_1, fill = month)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_shape_manual(values=c(3, 16, 17))+ 
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  theme_void()

#Percentage of total use at various times of day by each sub-meter.
houseDaySummer2008 <- filter(newDF, year == 2008 & month == 6 & day == 9 & (minute == 0 | minute == 30 | minute == 50))
View(houseDaySummer2008)
values <- houseDaySummer2008$Sub_metering_1
indices <- values!=0
pie(values[indices],labels=row.names(values)[indices],col=brewer.pal(length(values[indices]),'Spectral'))
legend("topleft",legend=row.names(values)[indices],fill=brewer.pal(length(values[indices]),'Spectral'))
percentlabels<- round(100*values/sum(values), 1)
pielabels<- paste(percentlabels, "%", sep="")
pie(values, main="Sub meter 1 Piechart -Total Use at Various time of a day", labels=pielabels, cex=0.8)
#sub meter 2
values1 <- houseDaySummer2008$Sub_metering_2
indices1 <- values1!=0
percentlabels1<- round(100*values1/sum(values1), 1)
pielabels1<- paste(percentlabels1, "%", sep="")
pie(values1, main="Sub meter 2 Piechart -Total Use at Various time of a day", labels=pielabels1, cex=0.8)
#sub meter 3
values2 <- houseDaySummer2008$Sub_metering_3
indices2 <- values2!=0
percentlabels2 <- round(100*values2/sum(values2), 1)
pielabels2 <- paste(percentlabels2, "%", sep="")
pie(values2, main="Sub meter 3 Piechart -Total Use at Various time of a day", labels=pielabels2, cex=0.8)


#----------------------------------------------
#summarize and aggregate and plot by sub meters 
#----------------------------------------------

N <- newDF %>% 
  group_by(year) %>% 
  summarise(SubMeter1=sum(Sub_metering_1),SubMeter2=sum(Sub_metering_2),SubMeter3=sum(Sub_metering_3))
View(N)
d <- melt(N, id.vars="year")
# Everything on the same plot
ggplot(d, aes(year,value, col=variable)) + 
  geom_point() + 
  stat_smooth() 
# Separate plots
ggplot(d, aes(year,value)) + 
  geom_point() + 
  stat_smooth() +
  facet_wrap(~variable)

#Percentage of total power use over a day by each sub-meter.
houseDay2008 <- filter(newDF, year == 2008 & month == 7 & day == 10 )
TotalSub <- aggregate(houseDay2008$Sub_metering_1,by=list(houseDay2008$Sub_metering_1),FUN=sum)
View(houseDay2008)
TotalSub1 <- sum(houseDay2008$Sub_metering_1)
TotalSub2 <- sum(houseDay2008$Sub_metering_2)
TotalSub3 <- sum(houseDay2008$Sub_metering_3)

df_tp <- data.frame(
  group = c("SubMeter1", "SubMeter2", "Submeter3"),
  value = c(TotalSub1, TotalSub2, TotalSub3)
)
head(df_tp)
bp<- ggplot(df_tp, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)
pie
#Percentage of total power use over an entire year by each sub-meter.
houseYear2008 <- filter(newDF, year == 2008 )
TotalYearSub1 <- sum(houseYear2008$Sub_metering_1)
TotalYearSub2 <- sum(houseYear2008$Sub_metering_2)
TotalYearSub3 <- sum(houseYear2008$Sub_metering_3)
Totalyear <- TotalYearSub1 + TotalYearSub2 +TotalYearSub3
percentlabelssub1 <- round(100*TotalYearSub1/sum(Totalyear), 1)
percentlabelssub2 <- round(100*TotalYearSub2/sum(Totalyear), 1)
percentlabelssub3 <- round(100*TotalYearSub3/sum(Totalyear), 1)
pielabels_year2008_1 <- paste(percentlabelssub1, "%", sep="")
pielabels_year2008_2 <- paste(percentlabelssub2, "%", sep="")
pielabels_year2008_3 <- paste(percentlabelssub3, "%", sep="")

df_percentage_year2008 <- data.frame(
  group = c("SubMeter1", "SubMeter2", "Submeter3"),
  value = c(percentlabelssub1, percentlabelssub2, percentlabelssub3)
)
B <- ggplot(df_percentage_year2008 , aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
pie <- B + coord_polar("y", start=0)
pie

#Produce any other visualizations that you believe may provide insight.
TotalSub1 <- sum(newDF$Sub_metering_1)
TotalSub2 <- sum(newDF$Sub_metering_2)
TotalSub3 <- sum(newDF$Sub_metering_3)
Totalsub <- TotalSub1+TotalSub2+TotalSub3
percentsub1 <- round(100*TotalSub1/sum(Totalsub), 1)
percentsub2 <- round(100*TotalSub2/sum(Totalsub), 1)
percentsub3 <- round(100*TotalSub3/sum(Totalsub), 1)





##------------------------------
#More plots for story 
##------------------------------

rd<-as.Date(as.character(df$Date),format="%Y-%m-%d")
fix(rd)
plot(df$Sub_metering_1~rd,type="l",col="red",axes=F)
str(df)


View(newDF)
plot(newDF$year,newDF$Sub_metering_1,xlab="year", ylab="Sub_metering_1", type="l",lwd=2,col="blue")

par(mfrow=c(3,1))
plot(newDF$Sub_metering_1, type="l", ylab="Sub Meter 1", xlab="Year", lwd=2,main="submeter 1 usage",col="orange")
plot(newDF$Sub_metering_2, type="l", ylab="Sub Meter 2", xlab="Year", lwd=2,main="submeter 2 usage",col="orange")
plot(newDF$Sub_metering_3, type="l", ylab="Sub Meter 3", xlab="Year", lwd=2,main="submeter 3 usage",col="orange")

data <- newDF
data$Date <- as.Date(as.character(data$Date),format="%Y-%m-%d")
str(newDF)
View(data)
plot(data[,9],data[,11], ylab="Sub Meter 1", xlab="Year",type="l", lwd=2,main="submeter 1 usage",col="orange",ylim=c(0,80))
plot(data[,9],data[,12], ylab="Sub Meter 2", xlab="Year",type="l", lwd=2,main="submeter 2 usage",col="orange",ylim=c(0,80))
plot(data[,9],data[,13], ylab="Sub Meter 3", xlab="Year",type="l", lwd=2,main="submeter 3 usage",col="orange",ylim=c(0,30))

par(mfrow=c(1,1))

mydata4 <- subset(data,subset = data$Date >= '2008-01-01' & data$Date <='2008-12-31')
head(mydata4)

par(mfrow=c(3,1))
plot(mydata4[,9],mydata4[,11], ylab="Sub Meter 1", xlab="Year",type="l", lwd=2,main="submeter1 2008 usage",col="orange",ylim=c(0,80))
plot(mydata4[,9],mydata4[,12], ylab="Sub Meter 2", xlab="Year",type="l", lwd=2,main="submeter2 2008 usage",col="orange",ylim=c(0,80))
plot(mydata4[,9],mydata4[,13], ylab="Sub Meter 3", xlab="Year",type="l", lwd=2,main="submeter3 2008 usage",col="orange",ylim=c(0,30))

mydata5 <- subset(data,subset = data$month >= 7 & data$month <= 10)
plot(mydata5[,9],mydata5[,11], ylab="Sub Meter 1", xlab="Year",type="l", lwd=2,main="submeter1 between July-Oct usage",col="orange",ylim=c(0,80))
grid()
abline(h=c(15,30,45,60))
plot(mydata5[,9],mydata5[,12], ylab="Sub Meter 2", xlab="Year",type="l", lwd=2,main="submeter2 between July-Oct  usage",col="orange",ylim=c(0,80))
grid()
abline(h=c(15,30,45,60))
plot(mydata5[,9],mydata5[,13], ylab="Sub Meter 3", xlab="Year",type="l", lwd=2,main="submeter3 between July-Oct  usage",col="orange",ylim=c(0,30))
grid()
abline(h=c(15,30,45,60))


#boxplot(data$Sub_metering_1~data$month, main="Sub Meter 1", ylab="sub meter 1",xlab="month",ylim=c(0,80),las=1,col="red")
##------------------------------
#time series
##------------------------------
## Subset to one observation per week on Wednesdays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(newDF, weekDay == 2 & hour == 20 & minute == 1)
View(house070809weekly)
## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, frequency=52, start=c(2007,1))
autoplot(tsSM3_070809weekly)
## Plot sub-meter 3 with autoplot - add labels, color

autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
## Create TS object with SubMeter2
tsSM2_070809weekly <- ts(house070809weekly$Sub_metering_2, frequency=52, start=c(2007,1))
autoplot(tsSM2_070809weekly)
## Create TS object with SubMeter1
tsSM1_070809weekly <- ts(house070809weekly$Sub_metering_1, frequency=52, start=c(2007,1))
autoplot(tsSM1_070809weekly)

plot.ts(tsSM3_070809weekly)
plot.ts(tsSM2_070809weekly)
plot.ts(tsSM1_070809weekly)
plot(df_pc$Sub_metering_1~df_pc$year)

housyear <- filter(df_pc, year == 2008)
View(housyear)
plot(housyear$Sub_metering_1~housyear$month)
plot(housyear$Sub_metering_2~housyear$month)
plot(housyear$Sub_metering_3~housyear$month)


## Subset to one observation per week on Wednesdays at 8:00pm for 2007, 2008 and 2009
house070809month<- filter(newDF, hour == 18)
View(house070809month)
## Create TS object with SubMeter3
tsSM3_070809month <- ts(house070809month$Sub_metering_3, frequency=52, start=c(2007,1))
autoplot(tsSM3_070809month)

##------------------------------
#Forecasting 
##------------------------------
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)
## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)
## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))
## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

#Produce Two More Forecasts

#Visualizations and analysis needed for your report:
  
#Sub-meter 1 with your choice of frequency, time period and confidence levels
#tsSM1_070809weekly 
fitSM1 <- tslm(tsSM1_070809weekly ~ trend + season) 
summary(fitSM1)
forecastfitSM1 <- forecast(fitSM1, h=20)
plot(forecastfitSM1)
forecastfitSM1c <- forecast(fitSM1, h=20, level=c(80,90))
plot(forecastfitSM1c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

#Sub-meter 2 with your choice of frequency, time period and confidence levels
#tsSM2_070809weekly

fitSM2 <- tslm(tsSM2_070809weekly ~ trend + season) 
summary(fitSM2)
forecastfitSM2 <- forecast(fitSM2, h=20)
plot(forecastfitSM2)
forecastfitSM2c <- forecast(fitSM2, h=20, level=c(80,90))
plot(forecastfitSM2c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

#One comparison chart showing the R2 and RMSE of each model you built
R21<-summary(fitSM1)$r.squared
R22<-summary(fitSM2)$r.squared
R23<-summary(fitSM3)$r.squared
RMSE1<-sqrt(mean(fitSM1$residuals^2))
RMSE2<-sqrt(mean(fitSM2$residuals^2))
RMSE3<-sqrt(mean(fitSM3$residuals^2))
Meter <- c("Submeter 1","Submeter 2","Submeter 3")
RSquared <- c("0.346", "0.371","0.278")
RMSE <- c("8.98", "5.86","6.82")
df_summary <- data.frame(Meter,RSquared, RMSE)
View(df_summary)

##---------------------------------
#Decomposing a Seasonal Time Series
##---------------------------------
## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
## Plot decomposed sub-meter 3 
plot(components070809SM3weekly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3weekly)
components070809SM3weekly$trend 
components070809SM3weekly$seasonal 
components070809SM3weekly$observed 
#Visualizations and analysis needed for your report:
  
#The sub-meter 3 decomposed plot you built in the walkthrough above
#Sub-meter 1 decomposed plot with your choice of frequency and time period
#tsSM1_070809weekly 
components070809SM1weekly <- decompose(tsSM1_070809weekly)
plot(components070809SM1weekly)
summary(components070809SM1weekly)
components070809SM1weekly$trend 
components070809SM1weekly$seasonal 
components070809SM1weekly$observed 
#Sub-meter 2 decomposed plot with your choice of frequency and time
#tsSM2_070809weekly 
components070809SM2weekly <- decompose(tsSM2_070809weekly)
plot(components070809SM2weekly)
summary(components070809SM2weekly)
components070809SM2weekly$trend 
components070809SM2weekly$seasonal 
components070809SM2weekly$observed 
#One comparison chart showing the summary statistics for the seasonal, trend and remainder components from each decomposed object

CompleteTrend <- cbind(summary(components070809SM1weekly), summary(components070809SM2weekly),summary(components070809SM3weekly))
View(CompleteTrend)


##---------------------------------
#Holt-Winters Forecasting
##---------------------------------
#The sub-meter 3 forecast plot and a plot containing only the forecasted area from the walkthrough above
## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))
## HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")
## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))

hw1 <- HoltWinters(tsSM3_070809weekly)
forecastSM3 <- predict(hw1, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw1, forecastSM3)
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))


#Visualizations and analysis needed for your report:
#Sub-meter 1 forecast plot and a plot containing only the forecasted area. Your choice of frequency and time period.
tsSM1_070809Adjusted <- tsSM1_070809weekly - components070809SM1weekly$seasonal
autoplot(tsSM1_070809Adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM1_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809, ylim = c(0, 25))
## HoltWinters forecast & plot
tsSM1_HW070809for <- forecast(tsSM1_HW070809, h=25)
plot(tsSM1_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")
## Forecast HoltWinters with diminished confidence levels
tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25))

hw2 <- HoltWinters(tsSM1_070809weekly)
forecastSM1 <- predict(hw2, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw2, forecastSM1)
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 1", start(2010))


#Sub-meter 2 forecast plot and a plot containing only the forecasted area. Your choice of frequency and time period.
tsSM2_070809Adjusted <- tsSM2_070809weekly - components070809SM2weekly$seasonal
autoplot(tsSM2_070809Adjusted)
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM2_070809Adjusted))

## Holt Winters Exponential Smoothing & Plot
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM2_HW070809, ylim = c(0, 25))
## HoltWinters forecast & plot
tsSM2_HW070809for <- forecast(tsSM3_HW070809, h=25)
plot(tsSM2_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2")
## Forecast HoltWinters with diminished confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))

hw3 <- HoltWinters(tsSM2_070809weekly)
forecastSM2 <- predict(hw3, n.ahead = 12, prediction.interval = T, level = 0.95)
plot(hw3, forecastSM2)
## Plot only the forecasted area
plot(tsSM2_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", start(2010))

