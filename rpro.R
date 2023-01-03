#standard libraries
library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(astsa)
library(forecast)
library(readxl)
library(urca)
library(ggfortify)
library(tsutils)

#Import data from a file

mydata<-read_excel("C:\\Users\\Surya\\r project\\Rainfall.xlsx")

#gather()->gathers columns into key - value pairs

mydata <- mydata %>% gather(key = "Tahun", value = "Curah_Hujan", -Bulan)

#ts()->converts into time series objects

mydata_ts <- ts(data = mydata[,3], frequency = 12, start = c(2005,1))

#window()->extract the subset of the time data
mydata_ts <- window(mydata_ts, start=c(2006,1))

#plotting the time series using autoplot()

#specifying how the program will plot data
options(repr.plot.width = 14, repr.plot.height = 8)
#generic plotting function
autoplot(mydata_ts) + ylab("Rainfall (mm2)") + xlab("Datetime") + 
  scale_x_date(date_labels = '%b - %Y', breaks = '1 year', minor_breaks = '2 month') +
  theme_bw() + ggtitle("Banten Rainfall 2006 - 2018")

#decompose the time series into seasonal,trend, remainder components using stl()
#stl->extratcting seasonal,trend, remainder components using loess 
decomp <- stl(mydata_ts[,1], s.window = 'periodic')

#plotting decomposition

autoplot(decomp) + theme_bw() + scale_x_date(date_labels = '%b - %Y', breaks = '1 year', minor_breaks = '2 month') +
  ggtitle("Remainder")

#calculating the trend and sesonal strength(correlation btn trend ans seasonality)
Tt <- trendcycle(decomp)
St <- seasonal(decomp)
Rt <- remainder(decomp)

Ft <- round(max(0,1 - (var(Rt)/var(Tt + Rt))),1)
Fs <- round(max(0,1 - (var(Rt)/var(St + Rt))),1)

data.frame('Trend Strength' = Ft , 'Seasonal Strength' = Fs)

#seasonal plot
seasonplot(mydata_ts, year.labels = TRUE, col = 1:13, main = "Seasonal Plot", ylab= "Rainfall (mm2)")
 
#seasonal box plot,subseries plot
seasplot(mydata_ts, outplot = 3, trend = NULL, main = "Seasonal Subseries Plot", ylab= "Rainfall (mm2)")
seasplot(mydata_ts, outplot = 2, trend = NULL, main = "Seasonal Box Plot", ylab= "Rainfall (mm2)")

mydata_train <- window(mydata_ts, end = c(2017,12)) #Create Train Set
mydata_test <- window(mydata_ts, start = c(2018,1)) #Create Test Set

#stationary test

summary(ur.kpss(mydata_train)) #Kwiatkowski-Phillips-Schmidt-Shin Test
summary(ur.df(mydata_train)) #Dickey-Fuller Test

#checking correlation between any two points on a given interval using ACF and PACF test

acf2(mydata_train)

#ARIMA model -> to calculate AICs value inorder to find the best fit

fit1 <- Arima(mydata_train, order = c(1,0,2), seasonal = c(1,0,2))
fit2 <- Arima(mydata_train, order = c(2,0,2), seasonal = c(2,0,2))
fit3 <- Arima(mydata_train, order = c(1,0,1), seasonal = c(1,0,1))
fit4 <- Arima(mydata_train, order = c(2,0,1), seasonal = c(2,0,1))
fit5 <- Arima(mydata_train, order = c(0,0,2), seasonal = c(0,0,2))
fit6 <- auto.arima(mydata_train, stepwise = FALSE, approximation = FALSE)

data.frame('Model-1' = fit1$aicc, 'Model-2' = fit2$aicc, 'Model-3' = fit3$aicc,
           'Model-4' = fit4$aicc, 'Model-5' = fit5$aicc,'Auto.Arima'= fit6$aicc,row.names = "AICc Value")

#to check the best fit by calculating minimum AICs value using Ljung-Box test

checkresiduals(fit1)

#ETS model

fit_ets <- ets(mydata_train, damped =TRUE)

#checking residuals again with ETS model using Ljung-Box test

checkresiduals(fit_ets)

#forecasting time series using smooth functions

model_1 <- forecast(fit1, h=12) 
model_ets <- forecast(fit_ets, h=12)

model_1 <- as.data.frame(model_1$mean)
model_ets <- as.data.frame(model_ets$mean)

colnames(model_1) <- "Curah_Hujan"
colnames(model_ets) <- "Curah_Hujan"

mydata_train_df <- as.data.frame(mydata_train)

model_1_plot <- rbind(mydata_train_df,model_1)
model_ets_plot <- rbind(mydata_train_df, model_ets)

#mutate()->adds new variables to the existing ones

model_1_plot <- model_1_plot %>% 
  mutate('Date' = seq(from = as.Date("2006-01-01", '%Y-%m-%d'), to = as.Date("2018-12-31",'%Y-%m-%d'),by = 'month'))

model_ets_plot <- model_ets_plot %>% 
  mutate('Date' = seq(from = as.Date("2006-01-01", '%Y-%m-%d'), to = as.Date("2018-12-31",'%Y-%m-%d'),by = 'month'))

mydata_ts_df <- as.data.frame(mydata_ts)

mydata_ts_df <- mydata_ts_df %>% 
  mutate('Date' = seq(from = as.Date("2006-01-01", '%Y-%m-%d'), to = as.Date("2018-12-31",'%Y-%m-%d'),by = 'month'))

mydata_train_df <- mydata_train_df %>% 
  mutate('Date' = seq(from = as.Date("2006-01-01", '%Y-%m-%d'), to = as.Date("2017-12-31",'%Y-%m-%d'),by = 'month'))

colors <- c("ARIMA Model Forecast 2018" = "blue", "ETS Model Forecast 2018" = "red", "Actual Data" = "black")



ggplot() + geom_line(model_1_plot,mapping = aes(x=Date, y=Curah_Hujan, color= "ARIMA Model Forecast 2018"),lty = 2) +
  geom_line(model_ets_plot,mapping = aes(x=Date, y=Curah_Hujan, color= "ETS Model Forecast 2018"),lty= 2) +
  geom_line(mydata_ts_df,mapping = aes(x=Date, y=Curah_Hujan, color= "Actual Data"), lty = 1, show.legend = TRUE) +
  ylab("Rainfall (mm2)") + xlab("Datetime") + 
  scale_x_date(date_labels = '%b - %Y', breaks = '1 year', minor_breaks = '2 month') +
  theme_bw() + ggtitle("Banten Rainfall 2006 - 2018") + scale_color_manual(values=colors)

accuracy(forecast(fit1, h=12), mydata_test)

accuracy(forecast(fit_ets, h=12), mydata_test)
#Create Model
ARIMA_Model <- Arima(mydata_ts, order = c(1,0,2), 
                     seasonal = c(1,0,2))
ETS_Model <- ets(mydata_ts, damped = TRUE, model = "AAA")

#ARIMA Model Forecast
autoplot(forecast(ARIMA_Model, h=24)) + theme_bw() + 
  ylab("Rainfall (mm2)") + xlab("Datetime") + 
  scale_x_date(date_labels = '%b - %Y', 
               breaks = '1 year', minor_breaks = '2 month') +
  theme_bw() + ggtitle("Banten Rainfall Forecast 2019-2020 
  ARIMA Model")
#ETS Model Forecast
autoplot(forecast(ETS_Model, h=24)) + theme_bw() + 
  ylab("Rainfall (mm2)") + xlab("Datetime") + 
  scale_x_date(date_labels = '%b - %Y', breaks = '1 year', 
               minor_breaks = '2 month') +
  theme_bw() + ggtitle("Banten Rainfall Forecast 2019-2020 
  ETS Model")