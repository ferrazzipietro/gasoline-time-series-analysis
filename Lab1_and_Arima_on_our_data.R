library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(dplyr)
library(tseries) 
library(astsa)
library(knitr)



## READ and PREPARE DATA

gasoline_month <- read.csv('data/merged_data.csv')
gasoline_month$date <- gasoline_month$date %>% as.Date
gasoline_month <- gasoline_month[,-1]

## EXPLORATORY ANALISYS
head(gasoline_month)

plot(gasoline_month$PREZZO ~ gasoline_month$date, type="b",
     main="gasoline total-price over time")
plot(gasoline_month$NETTO ~ gasoline_month$date, type="b",
     main="gasoline net-price over time")


##create a variable 'trend'
tt<- 1:NROW(gasoline_month)

##acf of variable "gasoline_month$NETTO "
acf(gasoline_month$NETTO,  lag.max=100)

# ARIMA

plot(diff(log(gasoline_month$NETTO)) ~ gasoline_month$date[-1], type="b",
     main="differentiate (lag1) log-gasoline price over time")

adf.test(diff(log(gasoline_month$PREZZO))) # la diff. è stazionaria

par(mfrow=c(1,2))
acf(diff(gasoline_month$PREZZO))
pacf(diff(gasoline_month$PREZZO))
arima21 <- sarima(log(gasoline_month$PREZZO), 2,1,0)

# NET PRICE (NO TAXES)
#par(mfrow=c(1,1))
plot(gasoline_month$NETTO ~ gasoline_month$date, type="b",
     main="net gasoline price over time")

plot(diff(log(gasoline_month$NETTO)) ~ gasoline_month$date[-1], type="b",
     main="differentiate (lag1) log- net gasoline price over time")
par(mfrow=c(1,2))
acf(diff(log(gasoline_month$PREZZO)))
pacf(diff(log(gasoline_month$PREZZO)))
arima20 <- sarima(diff(log(gasoline_month$NETTO)), 2,0,0, no.constant=T) # pessime code
shapiro.test(arima20$fit$residuals) # residui non normali
par(mfrow=c(1,1))

