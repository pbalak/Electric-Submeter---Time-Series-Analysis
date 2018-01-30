## C4T1 - Deep Data Analytics - Submetering - Initial Analysis of data
## Last Update : 2017.11.26
## File : C4T1_Deep_Data_Analytics_Initial_Data_Analysis.R

## Project Name: Course 4 Task1

##################
## Project Notes
##################

#Project Summary :Just to explore the data and gain some insights into it

######################
## HouseKeeping ##
#####################
#clear objects

rm(list=ls())

#get working directory
getwd()


################
# Load Packages
################
install.packages("caret")
install.packages("corrplot")
install.packages("readr")
#install.packages("doMC", repos="http://R-Forge.R-project.org")    # parallel processing

### parallel processing in Windows #####
library(doParallel) 
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
########### Check number of cores and workers available 
detectCores()
getDoParWorkers()
############

library(caret) # Classification and Regression trianing
library(janitor) # For cleaning the input attributes.Need to use clean_names() while importing the data.
##library(binr)
library(arules) # For Discretize
library(readr) 
library(doMC)
library(corrplot)
require(dplyr)
require(tidyr)
require(lubridate)
require(ggplot2)

###############################################
## Import Data
###############################################

submeter_data=read.table("C:/Users/Sheetal/Desktop/Big Data and Data Analytics/Module 4/Task1/DataSet/household_power_consumption/household_power_consumption.txt",sep = ";",header = TRUE,stringsAsFactors = FALSE)
nrow(submeter_data)
class(submeter_data)
head(submeter_data)
str(submeter_data)
summary(submeter_data)
## Taking a back up
submeter_data_back<-submeter_data
str(submeter_data_back)

names(submeter_data[3:9])

##################
# Pre-process DS 
##################
##Check for any NA ?
any(is.na(submeter_data))

## combine date and time into single column using unite() in tidyr

hhpwrDT<-submeter_data%>% unite(DateTime,Date,Time,sep=" ",remove=FALSE)

str(hhpwrDT)
summary(hhpwrDT)

# convert DateTime variable to POSIXct; if we use as.date(), we will use the timestamp and hence POSIXct

## test

#hhpwrDT$DateTime<-as.Date(hhpwrDT$DateTime)

#str(hhpwrDT)
#'data.frame':	2075259 obs. of  10 variables:
#  $ DateTime             : Date, format: "0016-12-20" "0016-12-20" "0016-12-20" ...

hhpwrDT$DateTime <- as.POSIXct(hhpwrDT$DateTime,
                               format = "%d/%m/%Y %H:%M:%S",
                               tz = "America/New_York")
class(hhpwrDT$DateTime)
tz(hhpwrDT$DateTime) 

# convert Date to as.Date

hhpwrDT$Date<-as.Date(hhpwrDT$Date, "%d/%m/%Y")

## convert all other attributes into numeric

#The as.numeric() function returns a vector of the levels of your factor
#and not the original values. Hence, it is required to convert a factor variable 
#to character before converting it to numeric.


## SEE BELOW for NA. Submeter 3 have lot of NAs and hence converting other attributes except that.
names(hhpwrDT[4:10])
#hhpwrDT[4:9]<-as.numeric(as.character(hhpwrDT[4:9]))
hhpwrDT$Global_active_power<-as.numeric(hhpwrDT$Global_active_power)
hhpwrDT$Global_reactive_power<-as.numeric(hhpwrDT$Global_reactive_power)
hhpwrDT$Voltage<-as.numeric(hhpwrDT$Voltage)
hhpwrDT$Global_intensity<-as.numeric(hhpwrDT$Global_intensity)
hhpwrDT$Sub_metering_1<-as.numeric(hhpwrDT$Sub_metering_1)
hhpwrDT$Sub_metering_2<-as.numeric(hhpwrDT$Sub_metering_2)
hhpwrDT$Sub_metering_3<-as.numeric(hhpwrDT$Sub_metering_3)

str(hhpwrDT)
summary(hhpwrDT)

## NA

sum(is.na(hhpwrDT$Global_active_power))
sum(is.na(hhpwrDT$Global_reactive_power))
sum(is.na(hhpwrDT$Voltage))
sum(is.na(hhpwrDT$Global_intensity))
sum(is.na(hhpwrDT$Sub_metering_1))
sum(is.na(hhpwrDT$Sub_metering_2))
sum(is.na(hhpwrDT$Sub_metering_3)) ## 25979

head(hhpwrDT)


#####################
# Filtering pipeline
#####################

# MAIN DS
# Total kWh per SM by year 

yr.sum.wide <- hhpwrDT%>% mutate(Year=year(DateTime))%>% filter(Year==2007|Year==2008|Year==2009|Year==2010) %>%
  group_by(Year)%>%
  summarize(SM1=round(sum(Sub_metering_1/1000,na.rm=TRUE),3),
            SM2=round(sum(Sub_metering_2/1000,na.rm=TRUE),3),
            SM3=round(sum(Sub_metering_3/1000,na.rm=TRUE),3),
            FirstDateTime=first(DateTime))
yr.sum.wide
names(yr.sum.wide)
class(yr.sum.wide)

summary(hhpwrDT)
### Max submeter readings###

max_sub_reading_1 <- hhpwrDT%>% filter(Sub_metering_1==max(hhpwrDT$Sub_metering_1,na.rm = TRUE))
max_sub_reading_1

max_sub_reading_2 <- hhpwrDT%>% filter(Sub_metering_2==max(hhpwrDT$Sub_metering_2,na.rm = TRUE))
max_sub_reading_2

max_sub_reading_3 <- hhpwrDT%>% filter(Sub_metering_3==max(hhpwrDT$Sub_metering_3,na.rm = TRUE))
max_sub_reading_3


## by month and year


yr.month.sum.wide <- hhpwrDT%>% mutate(Year=year(DateTime),Month= month(DateTime))%>%
  filter(Year==2007|Year==2008|Year==2009|Year==2010) %>%
  group_by(Month)%>%
  summarize(SM1=round(sum(Sub_metering_1/1000,na.rm=TRUE),3),
            SM2=round(sum(Sub_metering_2/1000,na.rm=TRUE),3),
            SM3=round(sum(Sub_metering_3/1000,na.rm=TRUE),3))

yr.month.sum.wide


#most_Used_month and time

most_Used_month_time <- hhpwrDT%>%mutate(Year=year(DateTime),Month= month(DateTime),Total=Sub_metering_1+Sub_metering_2+Sub_metering_3) %>%
   filter(Total==max(Total,na.rm=TRUE))%>%
  group_by(Date,Time)%>% summarize(Total)

most_Used_month_time
#2009-01-24 13:00:00

## Most Used Month

most_month_bygroup



###########################################################
### Technical requirements###

Filtering
#- 1. Create a subset that shows the average kWh used for each submeter for each weekday (Mon-Sun) during Winter (Dec - Feb) over the period from Jan-2007 through Oct-2010. This subset should only have 7 values (one for each day - it reflects the typical usage per day during Winter). 
#- 2. Create a subset that shows the average hourly kWh used for each hour of the day during January 2010. This subset should only have 24 values (it reflects the typical usage per hour of day during Jan-10). 

## hhpwrDT is the data set

summary(hhpwrDT)

## subset for period from 2007 through 2010

hhpwrDT_year_07_10<- hhpwrDT%>%mutate(year=year(DateTime))%>%
  filter(year == 2007 | year== 2008 | year== 2009 | year == 2010)

nrow(hhpwrDT_year_07_10)
head(hhpwrDT_year_07_10)

## subset for months

hhpwrDT_year_07_10_Dec_to_Feb <- hhpwrDT_year_07_10%>% mutate(month=month(DateTime))%>%
filter(month==12|month==1|month==2)

nrow(hhpwrDT_year_07_10_Dec_to_Feb)
head(hhpwrDT_year_07_10_Dec_to_Feb)
tail(hhpwrDT_year_07_10_Dec_to_Feb)

## quick test using sql
sql_test<- sqldf("select * from hhpwrDT_year_07_10_Dec_to_Feb where month=2 and year=2010 ")
head(sql_test)

## creating a column for the weekday

hhpwrDT_year_07_10_Dec_to_Feb_weekday<-hhpwrDT_year_07_10_Dec_to_Feb%>%mutate(day=weekdays(DateTime))

head(hhpwrDT_year_07_10_Dec_to_Feb_weekday)

## Submeter 1 Average

hhpwrDT_year_07_10_Dec_to_Feb_weekday$day<-as.factor(hhpwrDT_year_07_10_Dec_to_Feb_weekday$day)
summary(hhpwrDT_year_07_10_Dec_to_Feb_weekday)

#submtr1_avg<- hhpwrDT_year_07_10_Dec_to_Feb_weekday%>%
 # mean(Sub_metering_1,na.rm=TRUE)%>%
  #group_by(day)

sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday<-sqldf("select avg(Sub_metering_1/1000) as Submeter1,avg(Sub_metering_2/1000) as Submeter2,avg(Sub_metering_3/1000) as Submeter3,day as Weekday from hhpwrDT_year_07_10_Dec_to_Feb_weekday group by day")
sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday

str(sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday)

sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday$Weekday=factor(sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday$Weekday,levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
str(sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday)
#Submeter1 Submeter2 Submeter3    Weekday
#1 0.9074911 0.9947991  7.944548    Friday
#2 0.8265489 0.5676798  7.623044    Monday
#3 2.2398038 2.0573945  8.737395  Saturday
#4 2.1894015 2.2573190  7.097254    Sunday
#5 0.8603120 0.6278895  7.606246  Thursday
#6 0.7664299 1.4184598  7.746459   Tuesday
#7 1.2458437 2.0662200  7.443320 Wednesday

head(hhpwrDT)

hhpwrDT_Jan2010_byhour<- hhpwrDT%>% mutate(year=year(DateTime))%>%
  filter(year==2010)%>%mutate(month=month(DateTime))%>%
  filter(month==1)%>%mutate(hour=hour(DateTime))


hhpwrDT_Jan2010_byhour_out<- sqldf("select avg(Sub_metering_1/1000) as Submeter1,avg(Sub_metering_2/1000) as Submeter2,avg(Sub_metering_3/1000) as Submeter3,hour as Hour from hhpwrDT_Jan2010_byhour group by hour")

hhpwrDT_Jan2010_byhour_out


## PLot

## Multi-Variate plot for weekday

##convert sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday to long format using gather()

long_sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday<- sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday %>% gather(Submeter,AvgConsumption,Submeter1:Submeter3)
long_sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday ## Important and will be used later for plot

  
  ## Trying legend with long_sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday
  
  require(reshape2)

### LEGEN WORKED######## Converted into Long format using gather() as mentioned above
plot_1 <- ggplot(long_sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday,aes(x=Weekday,y=AvgConsumption))+
  geom_line(aes(color=Submeter,group=Submeter))+
  geom_point(aes(colour=Submeter,shape=Submeter,group=Submeter),size=3)+
  theme_bw()+
  ylab("Avg. Consumption (KWh)")+
  ggtitle("Avg. consumption per Submeter for each weekday - Winter 2007-2010")


plot_1 

## Facet Grid
plot_multivariate_weekday <- ggplot(long_sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday, aes(x = as.numeric(Weekday),y=AvgConsumption))+
  geom_line()
  
plot_multivariate_weekday+facet_grid(Submeter ~ .)


## MultiVariate line plot

plot_multi_variate<-ggplot(sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday,aes(x=as.numeric(Weekday)))+
  geom_line(aes(y=Submeter1),color='blue')+
  geom_line(aes(y=Submeter2),color='grey')+
  geom_line(aes(y=Submeter3),color='red')+
  xlab("Day")+
  ylab("Submeter Consumption")+
  ggtitle("Avg. consumption per Submeter for each weekday")

plot_multi_variate

plot_multi_variate_theme <- plot_multi_variate+theme_bw()
plot_multi_variate_theme


## Plot 2

long_convert_hour_data<-hhpwrDT_Jan2010_byhour_out %>% gather(Submeter,AvgConsumption,Submeter1:Submeter3)
long_convert_hour_data


plot_2 <- ggplot(long_convert_hour_data,aes(x=Hour,y=AvgConsumption))+
  geom_line(aes(color=Submeter,group=Submeter))+
  geom_point(aes(colour=Submeter,shape=Submeter,group=Submeter),size=3)+
  theme_bw()+
  ylab("Avg. Consumption (KWh)")+
  ggtitle("Avg. consumption per Submeter for each Hour - Jan 2010")


plot_2

## PLot 3 - subset of total Kwh used for each submeter from Jan 2007 through Dec 2009


yr.sum.07_09 <- hhpwrDT%>% mutate(Year=year(DateTime))%>% filter(Year==2007|Year==2008|Year==2009) %>%
  group_by(Year)%>%
  summarize(Submeter1=round(sum(Sub_metering_1/1000,na.rm=TRUE),3),
            Submeter2=round(sum(Sub_metering_2/1000,na.rm=TRUE),3),
            Submeter3=round(sum(Sub_metering_3/1000,na.rm=TRUE),3),
            FirstDateTime=first(DateTime))

yr.sum.07_09

long_yr.sum.07_09<-yr.sum.07_09 %>% gather(Submeter,AvgConsumption,Submeter1:Submeter3)
long_yr.sum.07_09




## Using break to display the x  axis accordingly . Else two years of same value are displayed.
plot_3 <- ggplot(long_yr.sum.07_09,aes(x=FirstDateTime,y=AvgConsumption))+
  geom_line(aes(color=Submeter,group=Submeter))+
  geom_point(aes(colour=Submeter,shape=Submeter,group=Submeter),size=3)+
  theme_bw()+
  xlab("Date")+
  ylab("Total Consumption (KWh)")+
  ggtitle("Total Consumption per Submeter - Jan 2007 - Dec 2009")+
  (scale_x_datetime(labels=date_format("%Y"),breaks=date_breaks("1 year")))


plot_3


## Putting all together

grid.arrange(plot_1,plot_2,plot_3,ncol=1)


## Additional analysis - 2009 in detail for SM3

yr.avg.09.SM3 <- hhpwrDT%>% mutate(Year=year(DateTime))%>% filter(Year==2009) %>% 
  mutate(Month=month(DateTime))%>% group_by(Month)%>%
  summarize(Submeter3=round(mean(Sub_metering_3/1000,na.rm=TRUE),3))

yr.avg.09.SM3


## SM2

yr.avg.09.SM2 <- hhpwrDT%>% mutate(Year=year(DateTime))%>% filter(Year==2009) %>% 
  mutate(Month=month(DateTime))%>% group_by(Month)%>%
  summarize(Submeter2=round(mean(Sub_metering_2/1000,na.rm=TRUE),3))

yr.avg.09.SM2


## PLotting time series for SM3 in 2009 by day of each month

plot_SM3_2009<- hhpwrDT%>% mutate(Year=year(DateTime))%>% filter(Year==2009)%>% group_by(Date=as.Date(DateTime)) %>%
summarise(SM3=round(sum(Sub_metering_3/1000,na.rm=TRUE),3))

nrow(plot_SM3_2009)
str(plot_SM3_2009)
head(plot_SM3_2009)
tail(plot_SM3_2009)

ggplot_plot_SM3_2009 <- ggplot(plot_SM3_2009, aes(x=Date,y=SM3), xlab(Date),ylab(SM3))+geom_point()+ stat_smooth(colour="blue")+
  ggtitle("SM3 consumption each day - 2009")+
  (scale_x_date(labels=date_format("%b %y"),breaks=date_breaks("2 month")))


## 2008

plot_SM3_2008<- hhpwrDT%>% mutate(Year=year(DateTime))%>% filter(Year==2008)%>% group_by(Date=as.Date(DateTime)) %>%
  summarise(SM3=round(sum(Sub_metering_3/1000,na.rm=TRUE),3))

ggplot_plot_SM3_2008<-ggplot(plot_SM3_2008, aes(x=Date,y=SM3), xlab(Date),ylab(SM3))+geom_point()+ stat_smooth(colour="blue")+
  ggtitle("SM3 consumption each day - 2008")+
  (scale_x_date(labels=date_format("%b %y"),breaks=date_breaks("2 month")))

## 2007

plot_SM3_2007<- hhpwrDT%>% mutate(Year=year(DateTime))%>% filter(Year==2007)%>% group_by(Date=as.Date(DateTime)) %>%
  summarise(SM3=round(sum(Sub_metering_3/1000,na.rm=TRUE),3))

ggplot_plot_SM3_2007<-ggplot(plot_SM3_2007, aes(x=Date,y=SM3), xlab(Date),ylab(SM3))+geom_point()+ stat_smooth(colour="blue")+
  ggtitle("SM3 consumption each day - 2007")+
  (scale_x_date(labels=date_format("%b %y"),breaks=date_breaks("2 month")))


grid.arrange(ggplot_plot_SM3_2009,ggplot_plot_SM3_2008,ggplot_plot_SM3_2007,ncol=3)

ggplot_plot_SM3_2009



#### Total Submeter - SM1+SM2+SM3

head(hhpwrDT)
str(hhpwrDT)

hhpwrDT_Total_SM <- hhpwrDT%>%mutate(SM_Total=Sub_metering_1+Sub_metering_2+Sub_metering_3)
head(hhpwrDT_Total_SM)
tail(hhpwrDT_Total_SM)


# Filter 2007 through 2009

hhpwrDT_Total_SM_07_09 <- hhpwrDT_Total_SM %>% mutate (Year=year(DateTime)) %>%
  filter(Year == 2007 | Year == 2008 | Year == 2009) %>% group_by(Year) %>%
  summarise(Total=sum(SM_Total/1000,na.rm=TRUE),FirstDateTime=first(DateTime)) 

hhpwrDT_Total_SM_07_09

# plotting by Year

plot_total_SM_07_09 <- ggplot(hhpwrDT_Total_SM_07_09, aes(x=FirstDateTime , y=Total)) + geom_line(colour="blue")+
  geom_point(size=4)+theme_bw()+
  xlab("Date")+
  ylab("Total Consumption of Submeters (KWh)")+
  ggtitle("Total Consumption of all Submeters - Jan 2007 - Dec 2009")+
  (scale_x_datetime(labels=date_format("%Y"),breaks=date_breaks("1 year")))

plot_total_SM_07_09


## Other equipments in Watt Hour

# global_active_power*1000/60 - sub_metering_1 - sub_metering_2 - sub_metering_3

head(hhpwrDT_Total_SM)

hhpwrDT_Total_Other <- hhpwrDT_Total_SM%>% mutate(Others=(Global_active_power*1000)/60 - SM_Total)
head(hhpwrDT_Total_Other)

hhpwrDT_Total_Other_07_09 <- hhpwrDT_Total_Other %>% mutate (Year=year(DateTime)) %>%
  filter(Year == 2007 | Year == 2008 | Year == 2009) %>% group_by(Year) %>%
  summarise(Total=sum(Others/1000,na.rm=TRUE),FirstDateTime=first(DateTime))

hhpwrDT_Total_Other_07_09

plot_total_Others_07_09 <- ggplot(hhpwrDT_Total_Other_07_09, aes(x=FirstDateTime , y=Total)) + geom_line(colour="blue")+
  geom_point(size=4)+theme_bw()+
  xlab("Date")+
  ylab("Total Consumption of Submeters (KWh)")+
  ggtitle("Total Consumption of Other equipments not recorded in Submeters - Jan 2007 - Dec 2009")+
  (scale_x_datetime(labels=date_format("%Y"),breaks=date_breaks("1 year")))

plot_total_Others_07_09


#Submeter 3 - Jan through May - 2007 , 2008 and 2009 - Daily and Hourly Average consumption(to get a particular Day and/or time for max usage)


SM3_07_09_day <- hhpwrDT %>% mutate(Year=year(DateTime)) %>% mutate (day=weekdays(DateTime)) %>%
  mutate (month=month(DateTime)) %>% filter(Year == 2007 | Year==2008| Year==2009 | month %in% 1:5)

head(SM3_07_09_day)
tail(SM3_07_09_day)


SM3_07_09_day_group <- SM3_07_09_day %>% group_by(Year,day) %>% 
  summarise(AvgConsumption = mean(Sub_metering_3/1000,na.rm=TRUE)) %>%
  filter(Year%in% 2007:2009)

SM3_07_09_day_group

head(SM3_07_09_day_group,21)

## PLot

SM3_07_09_day_group$day <- factor(SM3_07_09_day_group$day,levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
SM3_07_09_day_group$Year<-factor(SM3_07_09_day_group$Year)

plot_4<- ggplot(SM3_07_09_day_group,aes(x=day,y=AvgConsumption))+
geom_line(aes(color=Year,group=Year))+
  geom_point(aes(colour=Year,shape=Year,group=Year),size=3)+  
  theme_bw()+
  xlab("DAY")+
  ylab("Average Consumption (KWh)")+
  ggtitle("Avg. SM3 consumption for selective months- 2007 through 2009")

plot_4



## Hourly - SM3 - Jan through May - 2007 , 2008 and 2009

SM3_07_09_hour <- hhpwrDT %>% mutate(Year=year(DateTime)) %>% mutate (hour=hour(DateTime)) %>%
  mutate (month=month(DateTime)) %>% filter(Year == 2007 | Year==2008| Year==2009 | month %in% 1:5)

head(SM3_07_09_hour)
tail(SM3_07_09_hour)

SM3_07_09_hour_group <- SM3_07_09_hour %>% group_by(Year,hour) %>% 
  summarise(AvgConsumption = mean(Sub_metering_3/1000,na.rm=TRUE)) %>%
  filter(Year%in% 2007:2009)

head(SM3_07_09_hour_group)

SM3_07_09_hour_group$Year<-factor(SM3_07_09_hour_group$Year)
plot_5<- ggplot(SM3_07_09_hour_group,aes(x=hour,y=AvgConsumption))+
  geom_line(aes(color=Year,group=Year))+
  geom_point(aes(colour=Year,shape=Year,group=Year),size=3)+
  theme_bw()+
  xlab("DAY")+
  ylab("Average Consumption (KWh)")+
  ggtitle("Avg. SM3 consumption for selective months- 2007 through 2009 on hourly basis")

plot_5


## Plot by month and Year of complete enegy consumption - SM1+SM2+SM3

hhpwrDT_bymonth <- hhpwrDT %>% mutate(Date=as.Date(DateTime)) %>% mutate(Total=(Sub_metering_1+Sub_metering_2+Sub_metering_3)/1000)%>%
  mutate(month=month(DateTime))%>%mutate(year=year(DateTime)) %>%
  filter(year %in% 2007:2010)%>%
  select(Total,month,year,DateTime)%>%
  group_by(month,year)%>%
  summarise(Total=sum(Total,na.rm=TRUE),Date=first(DateTime))

plot_6 <- ggplot(hhpwrDT_bymonth,aes(x=Date , y=Total ,color=factor(month)))+geom_bar(stat="identity", na.rm = TRUE)+
  ggtitle("Total SM consumption over the time by month")+
ylab("Total consumption (KWH)")+
(scale_x_datetime(labels=date_format ("%b %y"), breaks=date_breaks("3 month")))+
  theme(legend.position="none")

plot_6

head(hhpwrDT_bymonth)
nrow(hhpwrDT_bymonth)




####### Average consumption on all days - 2007 to 2009 for Sm3

SM3_07_09_daily_addn <- hhpwrDT %>% mutate(Year=year(DateTime)) %>% mutate (day=weekdays(DateTime)) %>%
  mutate (month=month(DateTime)) %>% filter(Year == 2007 | Year==2008| Year==2009)

head(SM3_07_09_daily_addn)
tail(SM3_07_09_daily_addn)


SM3_07_09_day_group_addn <- SM3_07_09_daily_addn %>% group_by(Year,day) %>% 
  summarise(AvgConsumption = mean(Sub_metering_3/1000,na.rm=TRUE))

SM3_07_09_day_group_addn

head(SM3_07_09_day_group_addn,21)

## PLot

SM3_07_09_day_group_addn$day <- factor(SM3_07_09_day_group$day,levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
SM3_07_09_day_group_addn$Year<-factor(SM3_07_09_day_group$Year)

plot_4_addn<- ggplot(SM3_07_09_day_group_addn,aes(x=day,y=AvgConsumption))+
  geom_line(aes(color=Year,group=Year))+
  geom_point(aes(colour=Year,shape=Year,group=Year),size=3)+  
  theme_bw()+
  xlab("DAY")+
  ylab("Average Consumption (KWh)")+
  ggtitle("Avg. SM3 consumption on daily basis- 2007 through 2009")

plot_4_addn



### hourly basis - SM3 2007 to 2009



SM3_07_09_hour_add <- hhpwrDT %>% mutate(Year=year(DateTime)) %>% mutate (hour=hour(DateTime)) %>%
  mutate (month=month(DateTime)) %>% filter(Year == 2007 | Year==2008| Year==2009)

head(SM3_07_09_hour_add)
tail(SM3_07_09_hour)

SM3_07_09_hour_group_add <- SM3_07_09_hour_add %>% group_by(Year,hour) %>% 
  summarise(AvgConsumption = mean(Sub_metering_3/1000,na.rm=TRUE))

head(SM3_07_09_hour_group_add)
nrow(SM3_07_09_hour_group_add)

SM3_07_09_hour_group_add$Year<-factor(SM3_07_09_hour_group_add$Year)

plot_5_add<- ggplot(SM3_07_09_hour_group_add,aes(x=hour,y=AvgConsumption))+
  geom_line(aes(color=Year,group=Year))+
  geom_point(aes(colour=Year,shape=Year,group=Year),size=3)+
  theme_bw()+
  xlab("DAY")+
  ylab("Average Consumption (KWh)")+
  ggtitle("Avg. SM3 consumption - 2007 through 2009 on hourly basis")

plot_5_add


#--------------------------------------
#Trial for Re-Submission
#----------------------------------
submeter_data=read.table("C:/Users/Sheetal/Desktop/Big Data and Data Analytics/Module 4/Task1/DataSet/household_power_consumption/household_power_consumption.txt",sep = ";",header = TRUE,stringsAsFactors = FALSE)
hhpwrDT<-submeter_data%>% unite(DateTime,Date,Time,sep=" ",remove=FALSE)


hhpwrDT$DateTime <- as.POSIXct(hhpwrDT$DateTime,
                               format = "%d/%m/%Y %H:%M:%S",
                               tz = "America/New_York")
class(hhpwrDT$DateTime)
tz(hhpwrDT$DateTime) 

# convert Date to as.Date

hhpwrDT$Date<-as.Date(hhpwrDT$Date, "%d/%m/%Y")

## convert all other attributes into numeric

#The as.numeric() function returns a vector of the levels of your factor
#and not the original values. Hence, it is required to convert a factor variable 
#to character before converting it to numeric.


## SEE BELOW for NA. Submeter 3 have lot of NAs and hence converting other attributes except that.
names(hhpwrDT[4:10])
#hhpwrDT[4:9]<-as.numeric(as.character(hhpwrDT[4:9]))
hhpwrDT$Global_active_power<-as.numeric(hhpwrDT$Global_active_power)
hhpwrDT$Global_reactive_power<-as.numeric(hhpwrDT$Global_reactive_power)
hhpwrDT$Voltage<-as.numeric(hhpwrDT$Voltage)
hhpwrDT$Global_intensity<-as.numeric(hhpwrDT$Global_intensity)
hhpwrDT$Sub_metering_1<-as.numeric(hhpwrDT$Sub_metering_1)
hhpwrDT$Sub_metering_2<-as.numeric(hhpwrDT$Sub_metering_2)
hhpwrDT$Sub_metering_3<-as.numeric(hhpwrDT$Sub_metering_3)

hhpwrDT_year_07_10<- hhpwrDT%>%mutate(year=year(DateTime))%>%mutate(month=month(DateTime))%>%mutate(day=weekdays(DateTime))%>%
  filter(year == 2007 | year== 2008 | year== 2009 | year == 2010)%>%
  filter(month == 12 | month == 1 | month == 2)%>%
  select(Sub_metering_1,Sub_metering_2,Sub_metering_3,day)%>%
  group_by(day)%>%
  summarize(SM1=(mean(Sub_metering_1/1000,na.rm=TRUE)),
            SM2=(mean(Sub_metering_2/1000,na.rm=TRUE)),
            SM3=(mean(Sub_metering_3/1000,na.rm=TRUE)))

hhpwrDT_year_07_10$day<-factor(hhpwrDT_year_07_10$day,levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
  

long_sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday<- hhpwrDT_year_07_10 %>% gather(Submeter,AvgConsumption,SM1:SM3)
long_sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday

## Trying legend with long_sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday

require(reshape2)

### LEGEN WORKED######## Converted into Long format using gather() as mentioned above
plot_1 <- ggplot(long_sql_hhpwrDT_year_07_10_Dec_to_Feb_weekday,aes(x=day,y=AvgConsumption))+
  geom_line(aes(color=Submeter,group=Submeter))+
  geom_point(aes(colour=Submeter,shape=Submeter,group=Submeter),size=3)+
  theme_bw()+
  ylab("Avg. Consumption (KWh)")+
  ggtitle("Avg. consumption per Submeter for each weekday - Winter 2007-2010")


plot_1 
