#gold forecasting 
setwd("C:/Abhi notes/class3-2/eda/lab")
gold<-read.csv("gold.csv")
library(forecast)
library(tseries)
View(gold)
goldts<-ts(gold$Price,start=min(gold$Month),end=max(gold$Month),frequency = 1)
class(goldts)
plot(goldts)
acf(goldts)
pacf(goldts)
adf.test(goldts)
goldmodel=auto.arima(goldts,ic="aic",trace=TRUE)
goldf=forecast(goldmodel,level=c(95),h=24)
goldf
plot(goldf)
accuracy(goldf)

#gdp forecasting 
setwd("C:/Abhi notes/class3-2/eda/lab")
gdp<-read.csv("gdp.csv")
library(forecast)
library(tseries)
gdpts<-ts(gdp$GDP_gr,start=min(gdp$Year),end=max(gdp$Year),frequency = 1)
class(gdpts)
plot(gdpts)
acf(gdpts)
pacf(gdpts)
adf.test(gdpts)
gdpmodel=auto.arima(gdpts,ic="aic",trace=TRUE)
gdpf=forecast(gdpmodel,level=c(95),h=10)
gdpf
plot(gdpf)
accuracy(gdpf)

