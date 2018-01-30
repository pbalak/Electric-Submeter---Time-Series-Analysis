### C4 T2 - continuation to C4T1 
### Forecasting 

install.packages("forecast")
install.packages("tslm")

library(tslm)
library(forecast)
## Monthly, Quaterly and Yearly

## Monthly Time series data - Total energy consumed by all Submeters in KWH

hhpwrDT_bymonth_sort <- sqldf("select * from hhpwrDT_bymonth order by year")
View(hhpwrDT_bymonth_sort)
## Need only Total and dat from the data set

str(hhpwrDT_bymonth)
hhpwrDT_bymonth_total<-hhpwrDT_bymonth_sort$Total
hhpwrDT_bymonth_total

hhpwrDT_bymonth_ts <- ts(hhpwrDT_bymonth_total,frequency = 12,start=c(2007,1))
hhpwrDT_bymonth_ts

plot.ts(hhpwrDT_bymonth_ts,ylab="Total Consumption (KWH)")

## checking for Axes - Alternate to above method to show the X axis (time ) as Month-Year

rdate <- hhpwrDT_bymonth_sort$Date

plot(hhpwrDT_bymonth_sort$Total~rdate,type="l",col="red",axes=F,xlab="Date",ylab="Total Consumption (KWH)")
box()
axis(1,rdate,format(rdate,"%b-%y"))

## For SM3

hhpwrDT_bymonth_SM3 <- hhpwrDT %>% mutate(Date=as.Date(DateTime)) %>% mutate(Total=(Sub_metering_3)/1000)%>%
  mutate(month=month(DateTime))%>%mutate(year=year(DateTime)) %>%
  filter(year %in% 2007:2010)%>%
  select(Total,month,year,DateTime)%>%
  group_by(month,year)%>%
  summarise(Total=sum(Total,na.rm=TRUE),Date=first(DateTime))

hhpwrDT_bymonth_SM3

hhpwrDT_bymonth_SM3_sort <- sqldf("select * from hhpwrDT_bymonth_SM3 order by year")
View(hhpwrDT_bymonth_SM3_sort)

hhpwrDT_bymonth_SM3_total<-hhpwrDT_bymonth_SM3_sort$Total

hhpwrDT_bymonth_ts_SM3 <- ts(hhpwrDT_bymonth_SM3_total,frequency = 12,start=c(2007,1))
hhpwrDT_bymonth_ts_SM3

plot.ts(hhpwrDT_bymonth_ts_SM3,ylab="SM3 Consumption (KWH)")

## checking for Axes - Alternate to above method to show the X axis (time ) as Month-Year

rdate1 <- hhpwrDT_bymonth_SM3_sort$Date

plot(hhpwrDT_bymonth_SM3_sort$Total~rdate1,type="l",col="red",axes=F,xlab="Date",ylab="SM3 Consumption (KWH)")
box()
axis(1,rdate1,format(rdate1,"%b-%y"))


## Model Fit - Additive Linear Model for Total consumption (SM1 + SM2+ Sm3)

tslm_Additive1<- tslm(hhpwrDT_bymonth_ts~trend+season,lamda=NULL)

accuracy(tslm_Additive1)
tslm_Additive1$fitted
## comparing 
hhpwrDT_bymonth_ts
plot(tslm_Additive1)

plot(hhpwrDT_bymonth_ts)
lines(tslm_Additive1$fitted,col="blue")

forecast_tslm_Additive1_total<- forecast(tslm_Additive1,h=24)
forecast_tslm_Additive1_total
plot(forecast_tslm_Additive1_total)


acf(forecast_tslm_Additive1_total$residuals,lag.max = 20)

Box.test(forecast_tslm_Additive1_total$residuals,lag = 20, type="Ljung-Box")

## function definition is in sep file Utility_plotForecastErrors.R

plotForecastErrors(forecast_tslm_Additive1_total$residuals)

## Model Fit - Multiplicative Linear Model

tslm_Multiplicative1<- tslm(hhpwrDT_bymonth_ts~trend*season,lamda=NULL)
accuracy(tslm_Multiplicative1)

tslm_Multiplicative1$fitted
## comparing 
hhpwrDT_bymonth_ts

plot(tslm_Multiplicative1)


plot(hhpwrDT_bymonth_ts)
lines(tslm_Multiplicative1$fitted,col="blue")

forecast_tslm_Multiplicative_total<- forecast(tslm_Multiplicative1,h=24)
forecast_tslm_Multiplicative_total
plot(forecast_tslm_Multiplicative_total)


acf(forecast_tslm_Multiplicative_total$residuals,lag.max = 20)

Box.test(forecast_tslm_Multiplicative_total$residuals,lag = 20, type="Ljung-Box")

# function definition is in sep file Utility_plotForecastErrors.R

plotForecastErrors(forecast_tslm_Multiplicative_total$residuals)



## For SM3

## Model Fit - Additive Linear Model for Total consumption (SM1 + SM2+ Sm3)

tslm_Additive1_SM3<- tslm(hhpwrDT_bymonth_ts_SM3~trend+season,lamda=NULL)

accuracy(tslm_Additive1_SM3)
tslm_Additive1_SM3$fitted

plot(tslm_Additive1_SM3)


plot(hhpwrDT_bymonth_ts_SM3)
lines(tslm_Additive1_SM3$fitted,col="blue")

forecast_tslm_Additive1_SM3<- forecast(tslm_Additive1_SM3,h=24)
forecast_tslm_Additive1_SM3
plot(forecast_tslm_Additive1_SM3)


acf(forecast_tslm_Additive1_SM3$residuals,lag.max = 20)

Box.test(forecast_tslm_Additive1_SM3$residuals,lag = 20, type="Ljung-Box")

## function definition is in sep file Utility_plotForecastErrors.R

plotForecastErrors(forecast_tslm_Additive1_SM3$residuals)

## Model Fit - Multiplicative Linear Model

tslm_Multiplicative_SM3<- tslm(hhpwrDT_bymonth_ts_SM3~trend*season,lamda=NULL)
accuracy(tslm_Multiplicative_SM3)

tslm_Multiplicative_SM3$fitted


plot(tslm_Multiplicative_SM3)


plot(hhpwrDT_bymonth_ts_SM3)
lines(tslm_Multiplicative_SM3$fitted,col="blue")

forecast_tslm_Multiplicative_SM3<- forecast(tslm_Multiplicative_SM3,h=24)
forecast_tslm_Multiplicative_SM3
plot(forecast_tslm_Multiplicative_SM3)


acf(forecast_tslm_Multiplicative_SM3$residuals,lag.max = 20)

Box.test(forecast_tslm_Multiplicative_SM3$residuals,lag = 20, type="Ljung-Box")

## function definition is in sep file Utility_plotForecastErrors.R

plotForecastErrors(forecast_tslm_Multiplicative_SM3$residuals)



##########################################################
## 2 - SM3 total based on the Quaterly data

View(hhpwrDT_bymonth_SM3_sort)

hhpwrDT_Season_SM3 <- hhpwrDT_bymonth_SM3_sort %>% mutate (season= 
                      ifelse(month %in% c(3, 1, 2), "Q1",
                      ifelse(month %in% c(6, 4, 5), "Q2",
                      ifelse(month %in% c(9, 7, 8), "Q3",
                      ifelse(month %in% c(12, 10, 11), "Q4", "Error")))))


View(hhpwrDT_Season_SM3)



hhpwrDT_Season_SM3_Final<- sqldf("select sum(Total),season, year from hhpwrDT_Season_SM3
                                 group by season, year order by year")
View(hhpwrDT_Season_SM3_Final)


hhpwrDT_Season_SM3_ts <- ts(hhpwrDT_Season_SM3_Final$`sum(Total)`,frequency = 4,start=c(2007,1),end=c(2010,3))
plot(hhpwrDT_Season_SM3_ts)
hhpwrDT_Season_SM3_ts

## just checking if log() is doing any difference as it seems to be mulplicative seasonal
log_hhpwrDT_Season_SM3_ts<-log(hhpwrDT_Season_SM3_ts)
plot(log_hhpwrDT_Season_SM3_ts)


## Model Fit - Additive Linear Model for Total consumption (SM1 + SM2+ Sm3)

tslm_Additive1_SM3_Qtr<- tslm(hhpwrDT_Season_SM3_ts~trend+season,lamda=NULL)

accuracy(tslm_Additive1_SM3_Qtr)
tslm_Additive1_SM3_Qtr$fitted

plot(tslm_Additive1_SM3)


plot(hhpwrDT_Season_SM3_ts)
lines(tslm_Additive1_SM3_Qtr$fitted,col="blue")

forecast_tslm_Additive1_SM3_Qtr<- forecast(tslm_Additive1_SM3_Qtr,h=12)
forecast_tslm_Additive1_SM3_Qtr
plot(forecast_tslm_Additive1_SM3_Qtr)


acf(forecast_tslm_Additive1_SM3_Qtr$residuals,lag.max = 20)

Box.test(forecast_tslm_Additive1_SM3_Qtr$residuals,lag = 1, type="Ljung-Box")

## function definition is in sep file Utility_plotForecastErrors.R

plotForecastErrors(forecast_tslm_Additive1_SM3_Qtr$residuals)

## Model Fit - Multiplicative Linear Model - Better

tslm_Multiplicative_SM3_Qtr<- tslm(hhpwrDT_Season_SM3_ts~trend*season,lamda=NULL)
accuracy(tslm_Multiplicative_SM3_Qtr)

tslm_Multiplicative_SM3_Qtr$fitted


plot(tslm_Multiplicative_SM3_Qtr)


plot(hhpwrDT_Season_SM3_ts)
lines(tslm_Multiplicative_SM3_Qtr$fitted,col="blue")

forecast_tslm_Multiplicative_SM3_Qtr<- forecast(tslm_Multiplicative_SM3_Qtr,h=12)
forecast_tslm_Multiplicative_SM3_Qtr
plot(forecast_tslm_Multiplicative_SM3_Qtr)


acf(forecast_tslm_Multiplicative_SM3_Qtr$residuals,lag.max = 20)

Box.test(forecast_tslm_Multiplicative_SM3_Qtr$residuals,lag = 14, type="Ljung-Box")

## function definition is in sep file Utility_plotForecastErrors.R

plotForecastErrors(forecast_tslm_Multiplicative_SM3_Qtr$residuals)


#################################################################
## 3 - Yearly data
################################################################

View(hhpwrDT_bymonth_SM3_sort)

hhpwrDT_Season_SM3_Final_Annual<- sqldf("select sum(Total),year from hhpwrDT_bymonth_SM3_sort
                                 group by  year order by year")
View(hhpwrDT_Season_SM3_Final_Annual)

hhpwrDT_Annual_SM3_ts <- ts(hhpwrDT_Season_SM3_Final_Annual$`sum(Total)`,frequency = 1,start=2007,end=2009)
plot(hhpwrDT_Annual_SM3_ts)


## Model Fit - Additive Linear Model for Total consumption (SM1 + SM2+ Sm3)

tslm_Additive1_SM3_Annual<- tslm(hhpwrDT_Annual_SM3_ts~trend,lamda=NULL)
## just trend as there is no seasonality

summary(tslm_Additive1_SM3_Annual)
accuracy(tslm_Additive1_SM3_Annual)
tslm_Additive1_SM3_Annual$fitted

plot(tslm_Additive1_SM3_Annual)


plot(hhpwrDT_Annual_SM3_ts)
lines(tslm_Additive1_SM3_Annual$fitted,col="blue")

forecast_tslm_Additive1_SM3_Annual<- forecast(tslm_Additive1_SM3_Annual,h=2)
forecast_tslm_Additive1_SM3_Annual
plot(forecast_tslm_Additive1_SM3_Annual)

summary(forecast_tslm_Additive1_SM3_Annual)

acf(forecast_tslm_Additive1_SM3_Annual$residuals,lag.max = 20)

Box.test(forecast_tslm_Additive1_SM3_Annual$residuals,lag = 2, type="Ljung-Box")

## function definition is in sep file Utility_plotForecastErrors.R

plotForecastErrors(forecast_tslm_Additive1_SM3_Annual$residuals)



###########################################################
## Technical Requirements


#Create a subset that shows the total kWh per month for submeter 3 for the months Jan-07 through Oct-10. Forecast for Nov-10 through Dec-11.


View(hhpwrDT_bymonth_SM3_sort)

hhpwrDT_bymonth_ts_SM3 <- ts(hhpwrDT_bymonth_SM3_sort$Total,frequency = 12,start=c(2007,1), end = c(2010,10))
hhpwrDT_bymonth_ts_SM3

plot.ts(hhpwrDT_bymonth_ts_SM3,ylab="SM3 Consumption (KWH)")

## checking for Axes - Alternate to above method to show the X axis (time ) as Month-Year

rdate1 <- hhpwrDT_bymonth_SM3_sort$Date

plot(hhpwrDT_bymonth_SM3_sort$Total~rdate1,type="l",col="red",axes=F,xlab="Date",ylab="SM3 Consumption (KWH)")
box()
axis(1,rdate1,format(rdate1,"%b-%y"))



tslm_Additive1<- tslm(hhpwrDT_bymonth_ts_SM3~trend+season,lamda=NULL)

summary(tslm_Additive1)
accuracy(tslm_Additive1)

plot(tslm_Additive1)


plot(hhpwrDT_bymonth_ts)
lines(tslm_Additive1$fitted,col="blue")

forecast_tslm_Additive1_total<- forecast(tslm_Additive1,h=24)
summary(forecast_tslm_Additive1_total)
forecast_tslm_Additive1_total
plot(forecast_tslm_Additive1_total, xlab = "Year" , ylab="SM3 Consumption (KWh)" , main="Submeter 3 Forecasts")


acf(forecast_tslm_Additive1_total$residuals,lag.max = 20)

Box.test(forecast_tslm_Additive1_total$residuals,lag = 20, type="Ljung-Box")

## function definition is in sep file Utility_plotForecastErrors.R

plotForecastErrors(forecast_tslm_Additive1_total$residuals)

## Model Fit - Multiplicative Linear Model

tslm_Multiplicative1<- tslm(hhpwrDT_bymonth_ts~trend*season,lamda=NULL)
accuracy(tslm_Multiplicative1)
summary(tslm_Multiplicative1)
tslm_Multiplicative1$fitted
## comparing 
hhpwrDT_bymonth_ts

plot(tslm_Multiplicative1)


plot(hhpwrDT_bymonth_ts)
lines(tslm_Multiplicative1$fitted,col="blue")

forecast_tslm_Multiplicative_total<- forecast(tslm_Multiplicative1,h=24)
forecast_tslm_Multiplicative_total
plot(forecast_tslm_Multiplicative_total)


acf(forecast_tslm_Multiplicative_total$residuals,lag.max = 20)

Box.test(forecast_tslm_Multiplicative_total$residuals,lag = 20, type="Ljung-Box")

# function definition is in sep file Utility_plotForecastErrors.R

plotForecastErrors(forecast_tslm_Multiplicative_total$residuals)




#############################

#Create a subset that shows the total kWh per month for submeter 1 for the months Jan-07 through Oct-10. Forecast for Nov-10 through Dec-12.

hhpwrDT_bymonth_SM1_sort <- hhpwrDT %>%
  mutate(month=month(DateTime))%>%mutate(year=year(DateTime)) %>%
  filter(year %in% 2007:2010)
  
head(hhpwrDT_bymonth_SM1_sort)

hhpwrDT_bymonth_SM1_sort_sql<-sqldf("select month,year,sum(Sub_metering_1/1000) as Total from hhpwrDT_bymonth_SM1_sort group by month,year order by year,month ")


View(hhpwrDT_bymonth_SM1_sort_sql)

hhpwrDT_bymonth_ts_SM1 <- ts(hhpwrDT_bymonth_SM1_sort_sql$Total,frequency = 12,start=c(2007,1), end = c(2010,10))
hhpwrDT_bymonth_ts_SM1

plot.ts(hhpwrDT_bymonth_ts_SM1,ylab="SM1 Consumption (KWH)")


tslm_Additive2<- tslm(hhpwrDT_bymonth_ts_SM1~trend+season,lamda=NULL)

summary(tslm_Additive2)
accuracy(tslm_Additive2)

plot(tslm_Additive2)



forecast_tslm_Additive2_total<- forecast(tslm_Additive2,h=24)
summary(forecast_tslm_Additive2_total)
forecast_tslm_Additive2_total
plot(forecast_tslm_Additive2_total, xlab = "Year" , ylab="SM1 Consumption (KWh)" , main="Submeter  Forecasts")

#############################

#Create a subset that shows the total kWh per month for submeter 2 for the months Jan-07 through Oct-10. Forecast for Nov-10 through Dec-12.

hhpwrDT_bymonth_SM2_sort <- hhpwrDT %>%
  mutate(month=month(DateTime))%>%mutate(year=year(DateTime)) %>%
  filter(year %in% 2007:2010)

head(hhpwrDT_bymonth_SM2_sort)

hhpwrDT_bymonth_SM2_sort_sql<-sqldf("select month,year,sum(Sub_metering_2/1000) as Total from hhpwrDT_bymonth_SM2_sort group by month,year order by year,month ")


View(hhpwrDT_bymonth_SM2_sort_sql)

hhpwrDT_bymonth_ts_SM2 <- ts(hhpwrDT_bymonth_SM2_sort_sql$Total,frequency = 12,start=c(2007,1), end = c(2010,10))
hhpwrDT_bymonth_ts_SM2

plot.ts(hhpwrDT_bymonth_ts_SM2,ylab="SM2 Consumption (KWH)")


tslm_Additive3<- tslm(hhpwrDT_bymonth_ts_SM2~trend+season,lamda=NULL)

summary(tslm_Additive3)
accuracy(tslm_Additive3)

plot(tslm_Additive3)



forecast_tslm_Additive3_total<- forecast(tslm_Additive3,h=24)
summary(forecast_tslm_Additive3_total)
forecast_tslm_Additive3_total
plot(forecast_tslm_Additive3_total, xlab = "Year" , ylab="SM2 Consumption (KWh)" , main="Submeter2  Forecasts")


