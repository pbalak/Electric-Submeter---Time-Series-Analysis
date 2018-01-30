## Holt Winters
install.packages("forecast")
require(forecast)
#HW: 
#Need non-seasonal for HW. Therefore, create a ts object for SM3 for 
#each of the following 4 seasons: Win-09/10, Spr-10, Smr-10, Fall-10 (thru 11/25). 
#To d0 this, create a subset that shows kWh by day over each season, then forecast the
#next 30 days. Plot the fit and forecast objects for each season. (The plot will include 
#the data leading up to the forecast, and the forecast. In the POA, it mentions to plot 
#'forecast only', but don't worry about plotting only the forecast.) The plot will show 
#the actual data and the forecasted (in red) in the same chart. Note: to create the
#HW forecast object, may need to use forecast() I/O forecast.HoltWinters(). 
#Also, be sure to evaluate the residuals using the Ljung-Box test, etc. 
#Refer to the Little Book of R.  L

## Winter - SM3

hhpwrDT_byday_HW_Winter <- hhpwrDT %>% mutate(Total=(Sub_metering_3)/1000)%>%
  mutate(month=month(DateTime))%>%mutate(year=year(DateTime)) %>% mutate(day=day(DateTime)) %>%
  filter(Date >= as.Date("2009-12-01"))%>%
  filter(Date <= as.Date("2010-02-28"))%>%
  select(Total,day,month,year,DateTime)%>%
  group_by(day,month,year)%>%
  summarise(Total=sum(Total,na.rm=TRUE),Date=first(DateTime))%>%
  arrange(Date)

head(hhpwrDT_byday_HW_Winter)
tail(hhpwrDT_byday_HW_Winter)

nrow(hhpwrDT_byday_HW_Winter)

#inds <- seq(as.Date("2009-12-01"), as.Date("2010-03-01"), by = "day")

#set.seed(25)
#ts_hhpwrDT_byday_HW <- ts(hhpwrDT_byday_HW_Winter$Total , frequency = 365, start = c(2009, as.numeric(format(inds[1], "%j"))))

ts_hhpwrDT_byday_HW1 <- ts(hhpwrDT_byday_HW_Winter$Total , frequency = 30) # daily obs on monthly cycle


plot(ts_hhpwrDT_byday_HW1)

decomp_ts_hhpwrDT_byday_HW <- decompose(ts_hhpwrDT_byday_HW1)

plot(decomp_ts_hhpwrDT_byday_HW)

## Seasonal Adjustment

decomp_ts_hhpwrDT_byday_HW_Adjusted<-ts_hhpwrDT_byday_HW1-decomp_ts_hhpwrDT_byday_HW$seasonal

plot(decomp_ts_hhpwrDT_byday_HW_Adjusted,main="Seasonally Adjusted Winter_SM3")


## HoltWinters model

SM3_Winter_forecast <-HoltWinters(decomp_ts_hhpwrDT_byday_HW_Adjusted,beta=FALSE,gamma=FALSE,l.start=10.821) #l.start is the first value of the level 
plot(SM3_Winter_forecast)

SM3_Winter_forecast$SSE

## forecast

SM3_Winter_forecast2<-forecast:::forecast.HoltWinters(SM3_Winter_forecast,h=30)
SM3_Winter_forecast2
SM3_Winter_forecast2$residuals

plot(SM3_Winter_forecast2,xaxt = "n",main="HW Forecasting for 30 days beyond Winter 09/10" , xlab="Month", ylab= "SM3 consumption (KWH)")
axis(1, at=1:5, labels=c("Dec 09","Jan 10","Feb 10","Mar 10","Apr 10"))
## ggplot

ggplot(hhpwrDT_byday_HW_Winter, aes(Date, Total)) + geom_line() +
  scale_x_datetime(labels=date_format("%b %y")) + xlab("Date") + ylab("SM3 Consumption (KWH)")+
  ggtitle("Winter 2009/2010 SM3 Consumption")



### acf - Not working (not sure if its because we are using forecast:::forecast.HoltWinters
# instead of forecast.HoltWinters() as mentionedi n the little book of R)

acf(SM3_Winter_forecast2$residuals[2:90],lag.max = 20)


## Box test 

Box.test(SM3_Winter_forecast2$residuals, lag=20, type="Ljung-Box")

## Histogram

plotForecastErrors(SM3_Winter_forecast2$residuals[2:90])

#####################################################
## Spring - SM3

hhpwrDT_byday_HW_Spring <- hhpwrDT %>% mutate(Total=(Sub_metering_3)/1000)%>%
  mutate(month=month(DateTime))%>%mutate(year=year(DateTime)) %>% mutate(day=day(DateTime)) %>%
  filter(Date >= as.Date("2010-03-01"))%>%
  filter(Date <= as.Date("2010-05-31"))%>%
  select(Total,day,month,year,DateTime)%>%
  group_by(day,month,year)%>%
  summarise(Total=sum(Total,na.rm=TRUE),Date=first(DateTime))%>%
  arrange(Date)

head(hhpwrDT_byday_HW_Spring)
tail(hhpwrDT_byday_HW_Spring)

nrow(hhpwrDT_byday_HW_Spring)

#inds <- seq(as.Date("2009-12-01"), as.Date("2010-03-01"), by = "day")

#set.seed(25)
#ts_hhpwrDT_byday_HW <- ts(hhpwrDT_byday_HW_Winter$Total , frequency = 365, start = c(2009, as.numeric(format(inds[1], "%j"))))

ts_hhpwrDT_byday_HW2_Spring <- ts(hhpwrDT_byday_HW_Spring$Total , frequency = 30) # daily obs on monthly cycle


plot(ts_hhpwrDT_byday_HW2_Spring, ylab="SM3 Total Consumption(KWH)" , main="SM3 Spring 2010 Consumption")

decomp_ts_hhpwrDT_byday_HW2_Spring <- decompose(ts_hhpwrDT_byday_HW2_Spring)

plot(decomp_ts_hhpwrDT_byday_HW2_Spring)

## Seasonal Adjustment

decomp_ts_hhpwrDT_byday_HW_Adjusted2_Spring<-ts_hhpwrDT_byday_HW2_Spring-decomp_ts_hhpwrDT_byday_HW2_Spring$seasonal

plot(decomp_ts_hhpwrDT_byday_HW_Adjusted2_Spring,main="Seasonally Adjusted Spring_SM3")


## HoltWinters model

SM3_Spring_forecast2 <-HoltWinters(decomp_ts_hhpwrDT_byday_HW_Adjusted2_Spring,beta=FALSE,gamma=FALSE)
SM3_Spring_forecast2
SM3_Spring_forecast2$fitted
plot(SM3_Spring_forecast2)

SM3_Spring_forecast2$SSE

## forecast

SM3_Spring_forecast_2<- forecast:::forecast.HoltWinters(SM3_Spring_forecast2,h=30)
SM3_Spring_forecast_2


plot(SM3_Spring_forecast_2,xaxt = "n",main="HW Forecasting for 30 days beyond Spring -10" , xlab="Month", ylab= "SM3 consumption (KWH)")
axis(1, at=1:5, labels=c("Mar 10","Apr 10","May 10","Jun 10","Jul 10"))


acf(SM3_Spring_forecast_2$residuals[2:90],lag.max = 20)


## Box test 

Box.test(SM3_Spring_forecast_2$residuals, lag=20, type="Ljung-Box")

## Histogram

plotForecastErrors(SM3_Spring_forecast_2$residuals[2:93])

## ggplot

ggplot(hhpwrDT_byday_HW_Spring, aes(Date, Total)) + geom_line() +
  scale_x_datetime(labels=date_format("%b %y")) + xlab("Date") + ylab("SM3 Consumption (KWH)")+
  ggtitle("Spring 2010 SM3 Consumption")



#####################################################
## Summer - SM3

hhpwrDT_byday_HW_Summer <- hhpwrDT %>% mutate(Total=(Sub_metering_3)/1000)%>%
  mutate(month=month(DateTime))%>%mutate(year=year(DateTime)) %>% mutate(day=day(DateTime)) %>%
  filter(Date >= as.Date("2010-06-01"))%>%
  filter(Date <= as.Date("2010-08-31"))%>%
  select(Total,day,month,year,DateTime)%>%
  group_by(day,month,year)%>%
  summarise(Total=sum(Total,na.rm=TRUE),Date=first(DateTime))%>%
  arrange(Date)

head(hhpwrDT_byday_HW_Summer)
tail(hhpwrDT_byday_HW_Summer)

nrow(hhpwrDT_byday_HW_Summer)

#inds <- seq(as.Date("2009-12-01"), as.Date("2010-03-01"), by = "day")

#set.seed(25)
#ts_hhpwrDT_byday_HW <- ts(hhpwrDT_byday_HW_Winter$Total , frequency = 365, start = c(2009, as.numeric(format(inds[1], "%j"))))

ts_hhpwrDT_byday_HW2_Summer <- ts(hhpwrDT_byday_HW_Summer$Total , frequency = 30) # daily obs on monthly cycle


plot(ts_hhpwrDT_byday_HW2_Summer, ylab="SM3 Total Consumption(KWH)" , main="SM3 Summer 2010 Consumption")

decomp_ts_hhpwrDT_byday_HW2_Summer <- decompose(ts_hhpwrDT_byday_HW2_Summer)

plot(decomp_ts_hhpwrDT_byday_HW2_Summer)

## Seasonal Adjustment

decomp_ts_hhpwrDT_byday_HW_Adjusted2_Summer<-ts_hhpwrDT_byday_HW2_Summer-decomp_ts_hhpwrDT_byday_HW2_Summer$seasonal

plot(decomp_ts_hhpwrDT_byday_HW_Adjusted2_Summer,main="Seasonally Adjusted Summer_SM3")


## HoltWinters model

SM3_Summer_forecast2 <-HoltWinters(decomp_ts_hhpwrDT_byday_HW_Adjusted2_Summer,beta=FALSE,gamma=FALSE)
SM3_Summer_forecast2
SM3_Summer_forecast2$fitted
plot(SM3_Summer_forecast2)

SM3_Summer_forecast2$SSE

## forecast

SM3_Summer_forecast_2<- forecast:::forecast.HoltWinters(SM3_Summer_forecast2,h=30)
SM3_Summer_forecast_2


plot(SM3_Summer_forecast_2,xaxt = "n",main="HW Forecasting for 30 days beyond Summer -10" , xlab="Month", ylab= "SM3 consumption (KWH)")
axis(1, at=1:5, labels=c("Jun 10","Jul 10","Aug 10","Sep 10","Oct 10"))


acf(SM3_Summer_forecast_2$residuals[2:92],lag.max = 20)


## Box test 

Box.test(SM3_Summer_forecast_2$residuals, lag=20, type="Ljung-Box")

## Histogram

plotForecastErrors(SM3_Summer_forecast_2$residuals[2:92])

## ggplot

ggplot(hhpwrDT_byday_HW_Summer, aes(Date, Total)) + geom_line() +
  scale_x_datetime(labels=date_format("%b %y")) + xlab("Date") + ylab("SM3 Consumption (KWH)")+
  ggtitle("Summer 2010 SM3 Consumption")

#####################################################
## Fall - SM3

hhpwrDT_byday_HW_Fall <- hhpwrDT %>% mutate(Total=(Sub_metering_3)/1000)%>%
  mutate(month=month(DateTime))%>%mutate(year=year(DateTime)) %>% mutate(day=day(DateTime)) %>%
  filter(Date >= as.Date("2010-09-01"))%>%
  filter(Date <= as.Date("2010-11-25"))%>%
  select(Total,day,month,year,DateTime)%>%
  group_by(day,month,year)%>%
  summarise(Total=sum(Total,na.rm=TRUE),Date=first(DateTime))%>%
  arrange(Date)

head(hhpwrDT_byday_HW_Fall)
tail(hhpwrDT_byday_HW_Fall)

nrow(hhpwrDT_byday_HW_Fall)

#inds <- seq(as.Date("2009-12-01"), as.Date("2010-03-01"), by = "day")

#set.seed(25)
#ts_hhpwrDT_byday_HW <- ts(hhpwrDT_byday_HW_Winter$Total , frequency = 365, start = c(2009, as.numeric(format(inds[1], "%j"))))

ts_hhpwrDT_byday_HW2_Fall <- ts(hhpwrDT_byday_HW_Fall$Total , frequency = 30) # daily obs on monthly cycle


plot(ts_hhpwrDT_byday_HW2_Fall, ylab="SM3 Total Consumption(KWH)" , main="SM3 Fall 2010 Consumption")

decomp_ts_hhpwrDT_byday_HW2_Fall <- decompose(ts_hhpwrDT_byday_HW2_Fall)

plot(decomp_ts_hhpwrDT_byday_HW2_Fall)

## Seasonal Adjustment

decomp_ts_hhpwrDT_byday_HW_Adjusted2_Fall<-ts_hhpwrDT_byday_HW2_Fall-decomp_ts_hhpwrDT_byday_HW2_Fall$seasonal

plot(decomp_ts_hhpwrDT_byday_HW_Adjusted2_Fall,main="Seasonally Adjusted Fall_SM3")


## HoltWinters model

SM3_Fall_forecast2 <-HoltWinters(decomp_ts_hhpwrDT_byday_HW_Adjusted2_Fall,beta=FALSE,gamma=FALSE)
SM3_Fall_forecast2
SM3_Fall_forecast2$fitted
plot(SM3_Fall_forecast2)

SM3_Fall_forecast2$SSE

## forecast

SM3_Fall_forecast_2<- forecast:::forecast.HoltWinters(SM3_Fall_forecast2,h=30)
SM3_Fall_forecast_2


plot(SM3_Fall_forecast_2,xaxt = "n",main="HW Forecasting for 30 days beyond Fall -10" , xlab="Month", ylab= "SM3 consumption (KWH)")
axis(1, at=1:5, labels=c("Sep 10","Oct 10","Nov 10","Dec 10","Jan 10"))


acf(SM3_Fall_forecast_2$residuals[2:86],lag.max = 20)


## Box test 

Box.test(SM3_Fall_forecast_2$residuals, lag=20, type="Ljung-Box")

## Histogram

plotForecastErrors(SM3_Fall_forecast_2$residuals[2:86])

## ggplot

ggplot(hhpwrDT_byday_HW_Fall, aes(Date, Total)) + geom_line() +
  scale_x_datetime(labels=date_format("%b %y")) + xlab("Date") + ylab("SM3 Consumption (KWH)")+
  ggtitle("Fall 2010 SM3 Consumption")
