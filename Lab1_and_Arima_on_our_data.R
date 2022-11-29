library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(dplyr)
library(tseries) 
library(astsa)
library(knitr)
library(readxl)
install.packages("readxl")

## READ and PREPARE DATA

gasoline_month <- read.csv("data/prezzi_mensili_benzina_dal_1996_a_20221028.csv")
gasoline_month <- gasoline_month %>% mutate(date = as.Date(paste(ANNO, CODICE_MESE, "01", 
                                                                 sep="-", format = "%d-%m-%y")))

co2_emissions_vehicles <- read_xlsx("data/DatiCopertTrasportoStrada1990-2020.xlsx",
                           sheet = "CO2_TOTAL") # The last columns *T are the totals
cols_to_be_kept <- colnames(co2_emissions_vehicles)[grepl("*T", colnames(co2_emissions_vehicles))]
cols_to_be_kept <- cols_to_be_kept[7:31]
co2_emissions_vehicles <- co2_emissions_vehicles %>% select(all_of(colnames(co2_emissions_vehicles)[1:6]),
                                                            all_of(cols_to_be_kept))
co2_emissions_vehicles <- co2_emissions_vehicles %>% filter()

km_by_vehicles <- read_xlsx("data/DatiCopertTrasportoStrada1990-2020.xlsx",
                                    sheet = "veickm") # The last columns *T are the totals
cols_to_be_kept <- c(1:4, 11:ncol(km_by_vehicles))
km_by_vehicles <- km_by_vehicles[,cols_to_be_kept]

oil_price <- read_xlsx("data/oil_price_monthy.xlsx")
oil_price <- oil_price %>% mutate(Date = as.Date(Month))
oil_price <- oil_price %>% filter(Date >= as.Date("1996-01-01")) %>%
  select(Date, Price)

## CREATING NEW VARIABLE CO2/km
# create the ratio btw co2 emitted and km done. This could be interesting


## EXPLORATORY AN.
head(gasoline_month)

plot(gasoline_month$PREZZO ~ gasoline_month$date, type="b",
     main="gasoline total-price over time")
plot(gasoline_month$NETTO ~ gasoline_month$date, type="b",
     main="gasoline net-price over time")


##create a variable 'time'
tt<- 1:NROW(gasoline_month)

##acf of variable "gasoline_month$NETTO "
acf(gasoline_month$NETTO,  lag.max=100)

##fit a linear regression model 
fit1 <- lm(gasoline_month$NETTO ~ tt)
summary(fit1)

##plot of the model
plot(tt, gasoline_month$NETTO , xlab="Time", ylab="Facebook users")
abline(fit1, col=3)

##check the residuals? are they autocorrelated? Test of DW
dwtest(fit1)

##check the residuals
resfit1<- residuals(fit1)
plot(resfit1,xlab="Time", ylab="residuals" )


# ##let us do the same with a linear model for time series, so we transform the data into a ts object
# ts <- ts(gasoline_month$NETTO , frequency = 1)
# ts.plot(ts, type="o")
# 
# ## we fit a linear model with the tslm function
# fit_season<- tslm(ts~trend + season)
# 
# ###obviously it gives the same results of the first model
# summary(fit_season)
# dwtest(fit_season)
# 
# ##check the residuals
# resfit_season<- residuals(fit_season)
# plot(resfit_season,xlab="Time", ylab="residuals" )
# acf(resfit_season)

# ###plot of the model
# plot(gasoline_month$NETTO, ylab="Gasoline price", xlab="Time")
# lines(fitted(fit_season), col=2)


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


