## Decompose
install.packages("ggseas")
## Re-using the same Data Set 
View(hhpwrDT_bymonth_SM3_sort)

hhpwrDT_bymonth_ts_SM3_decomp <- ts(hhpwrDT_bymonth_SM3_sort$Total,frequency = 12,start=c(2007,1), end = c(2010,10))
hhpwrDT_bymonth_ts_SM3_decomp

summary(hhpwrDT_bymonth_ts_SM3_decomp)

plot.ts(hhpwrDT_bymonth_ts_SM3_decomp)

hhpwrDT_bymonth_ts_SM3_decomp_components<-decompose(hhpwrDT_bymonth_ts_SM3_decomp)

plot(hhpwrDT_bymonth_ts_SM3_decomp_components)

summary(hhpwrDT_bymonth_ts_SM3_decomp_components)
summary(hhpwrDT_bymonth_ts_SM3_decomp_components$trend)
summary(hhpwrDT_bymonth_ts_SM3_decomp_components$seasonal)
summary(hhpwrDT_bymonth_ts_SM3_decomp_components$random)


## 2 - Feb 2010 hourly decompose over a month - Feb 2010

View(head(hhpwrDT))

feb_2010_hour <- hhpwrDT%>% mutate(Year=year(DateTime))%>% mutate(month=month(DateTime)) %>%
mutate(hour=hour(DateTime))%>% mutate(date=date(DateTime)) %>%
  filter(Year==2010) %>%
    filter(month==02) %>% select(Sub_metering_3,hour,date) %>% group_by(hour,date)%>%
    summarize(SM3=round(sum(Sub_metering_3/1000,na.rm=TRUE),3)) %>%
  arrange(date)

View((feb_2010_hour))


## converting to Time series data 
##https://robjhyndman.com/hyndsight/dailydata/
##https://robjhyndman.com/hyndsight/seasonal-periods/

#Trying mutiseasonal time series

###############################################################
ts_feb_2010_hour <- msts(feb_2010_hour$SM3,seasonal.periods = c(24,168)) ## Assuming daily or Weekly trend(Refer to the table in the above link)
## msts -> Multi seasonal time series

#One convenient model for multiple seasonal time series is a TBATS model:
ts_fit <- tbats(ts_feb_2010_hour)
plot(forecast(ts_fit))
################################################################

## Trying one by one - Daily seasonality (hours into days) 

ts_feb_2010_1<-ts(feb_2010_hour$SM3,frequency = 24)
plot.ts(ts_feb_2010_1)

summary(ts_feb_2010_1)


ts_feb_2010_1_decomp<-decompose(ts_feb_2010_1)
plot(ts_feb_2010_1_decomp)

summary(ts_feb_2010_1_decomp$trend)
summary(ts_feb_2010_1_decomp$seasonal)
summary(ts_feb_2010_1_decomp$random)

## Trying one by one - Weekly seasonality (hours into Weeks) 

ts_feb_2010_2<-ts(feb_2010_hour$SM3,frequency = 168)
plot.ts(ts_feb_2010_2)

summary(ts_feb_2010_2)

ts_feb_2010_2_decomp<-decompose(ts_feb_2010_2)
plot(ts_feb_2010_2_decomp)

summary(ts_feb_2010_2_decomp$trend)
summary(ts_feb_2010_2_decomp$seasonal)
summary(ts_feb_2010_2_decomp$random)

#####################
###plan of attack
#####################

## decomposing any 3 time series data and plot different components that are 
## relevant


## Total usage of power over the last years 2007 through 2009 - monthly

hhpwrDT_Total_SM_07_10_decomp <- hhpwrDT_Total_SM %>% mutate (Year=year(DateTime)) %>%
  filter(Year == 2007 | Year == 2008 | Year == 2009 | Year==2010)
head(hhpwrDT_Total_SM_07_09_decomp)
tail(hhpwrDT_Total_SM_07_09_decomp)

monthly_Total_Consumption_07_10 <- hhpwrDT_Total_SM_07_10_decomp %>% mutate(Total=(SM_Total)/1000)%>%
  mutate(month=month(DateTime))%>%mutate(year=year(DateTime)) %>% mutate(day=day(DateTime)) %>%
  select(Total,month,year,DateTime)%>%
  group_by(month,year)%>%
  summarise(Total=sum(Total,na.rm=TRUE),Date=first(DateTime)) %>%
  arrange(Date)
  
  head(monthly_Total_Consumption_07_10)
  tail(monthly_Total_Consumption_07_10)
  
  ts_daily_total <- ts(monthly_Total_Consumption_07_10$Total,frequency = 12,start = c(2007,1), end = c(2010,10))
  plot.ts(ts_daily_total)
  
  decompose_ts_daily_total <- decompose(ts_daily_total)
  plot(decompose_ts_daily_total)
  
decompose_ts_daily_total$trend
plot(decompose_ts_daily_total$trend,ylab="Total Power Consumption(KWH)" , main="Trend For Total Power Consumption from Jan 2007 to Nov 2010")  

## SM3 total - 2007 through 2010
hhpwrDT_bymonth_ts_SM3

plot(decompose(hhpwrDT_bymonth_ts_SM3))
plot(decompose(hhpwrDT_bymonth_ts_SM3)$trend,ylab="SM3 Power consumption(KWH)" , main="SM3 consumption from Jan 2007 through Nov 2010")

# Seasonal Adjustment

decomp_SM3<-decompose(hhpwrDT_bymonth_ts_SM3)
plot(decomp_SM3)
plot(hhpwrDT_bymonth_ts_SM3)

hhpwrDT_bymonth_ts_SM3_Adjust<-hhpwrDT_bymonth_ts_SM3-decomp_SM3$seasonal

plot(hhpwrDT_bymonth_ts_SM3_Adjust)
plot(hhpwrDT_bymonth_ts_SM3)

SM3_forecast <-HoltWinters(hhpwrDT_bymonth_ts_SM3_Adjust,beta=FALSE,gamma=FALSE)

SM3_forecast$SSE

plot(SM3_forecast)


SM3_fore1<- forecast:::forecast.HoltWinters(SM3_forecast,h=24)
plot(SM3_fore1)


## SM1 total consumption

hhpwrDT_bymonth_SM1 <- hhpwrDT %>% mutate(Date=as.Date(DateTime)) %>% mutate(Total=(Sub_metering_1)/1000)%>%
  mutate(month=month(DateTime))%>%mutate(year=year(DateTime)) %>%
  filter(year %in% 2007:2010)%>%
  select(Total,month,year,DateTime)%>%
  group_by(month,year)%>%
  summarise(Total=sum(Total,na.rm=TRUE),Date=first(DateTime))%>%
  arrange(Date)

hhpwrDT_bymonth_ts_SM1<- ts(hhpwrDT_bymonth_SM1$Total,frequency = 12, start = c(2007,1) , end= c(2010,10))

plot(decompose(hhpwrDT_bymonth_ts_SM1))
plot(decompose(hhpwrDT_bymonth_ts_SM1)$trend,ylab="SM1 Power consumption(KWH)" , main="SM1 consumption from Jan 2007 through Nov 2010")




## SM2 total consumption

hhpwrDT_bymonth_SM2 <- hhpwrDT %>% mutate(Date=as.Date(DateTime)) %>% mutate(Total=(Sub_metering_2)/1000)%>%
  mutate(month=month(DateTime))%>%mutate(year=year(DateTime)) %>%
  filter(year %in% 2007:2010)%>%
  select(Total,month,year,DateTime)%>%
  group_by(month,year)%>%
  summarise(Total=sum(Total,na.rm=TRUE),Date=first(DateTime))%>%
  arrange(Date)

hhpwrDT_bymonth_ts_SM2<- ts(hhpwrDT_bymonth_SM2$Total,frequency = 12, start = c(2007,1) , end= c(2010,10))


plot(decompose(hhpwrDT_bymonth_ts_SM2))
plot(decompose(hhpwrDT_bymonth_ts_SM2)$trend,ylab="SM2 Power consumption(KWH)" , main="SM2 consumption from Jan 2007 through Nov 2010")
