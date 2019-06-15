#for personal use.plaease dont copy
C:\Users\ankit singh\Documents\R\win-library\3.4



Walmart<-read.csv("F://r_git_upload//univariate//data//Walmart.csv") #Reading data
Walmart #View data
Walmart_ts=ts(Walmart$Values,start=c(2011,1),end=c(2014,6),frequency=12) #Convering dataset to timeseries format
Walmart_ts #View time series format
plot(decompose(Walmart_ts)) #Plot trends,cycles in time series data

library(tseries)
adf.test(((Walmart_ts)), alternative="stationary", k=0) #Finding degree of differencing)
adf.test(diff((Walmart_ts)), alternative="stationary", k=0) #p=0.01 hence acceepted and value of d=1;

datatsdiff=diff(Walmart_ts) 
plot(datatsdiff)

acf(datatsdiff, lag.max=20)             # plot a correlation
acf(datatsdiff, lag.max=20, plot=FALSE)
pacf(datatsdiff, lag.max=20)             # plot a partial correlation
pacf(datatsdiff, lag.max=20, plot=FALSE)
#From ACF & PACF we can figure out that d is equal to 1


library(forecast)

arima(Walmart_ts, order=c(2,1,2)) #aic=511.62
arima(Walmart_ts, order=c(0,1,0)) #aic=510.55 #Lowest AIC hence selected.
arima(Walmart_ts, order=c(1,1,0)) #aic=512.38
arima(Walmart_ts, order=c(0,1,1)) #aic=512.38

auto.arima(Walmart_ts)//Best model(0,1,0) and further validated


library(forecast)
datatsarima <- arima(Walmart_ts, order=c(0,1,0))
datatsforecasts <- forecast(datatsarima, h=1*6) #Forecasting
plot(datatsforecasts)
datatsforecasts