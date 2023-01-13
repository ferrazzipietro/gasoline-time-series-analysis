#setwd("D:/OneDrive/Master/BDMA/Courses/Semester_3/Time_Series_Analysis/Project/gasoline-time-series-analysis")
library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(dplyr)
library(tseries) 
library(astsa)
library(glmnet)
library(knitr)
library(gbm)
library(caret)
library(gam)
library(Metrics)
source('utilities_models.R')

gasoline_month <- open_data("data/merged_data.csv")
vars <- (gasoline_month %>% colnames)

FORECAST = TRUE
if(FORECAST) gasoline_month <- generate_data_for_forecast(gasoline_month, 3)
n <- nrow(gasoline_month)

REPLACE_NA_WITH_0 <- F
if (REPLACE_NA_WITH_0) gasoline_month <- gasoline_month %>% replace(is.na(.), 0)

TRAIN_TEST_SPLIT <- T
idx <- floor(n*0.8)
if (! TRAIN_TEST_SPLIT) idx <- n
train_data <- gasoline_month[1:idx,]
test_data <- gasoline_month[idx:n,]
n_test <- nrow(test_data)


#********************* Correlation matrix *************************#

emissions_arima <- auto.arima(train_data$weighted_emission)
emissions_arima$fitted %>% plot
train_data$weighted_emission %>% points(type='l', col=2)

oil_price_arima <- auto.arima(train_data$oil_price)
oil_price_arima$fitted %>% plot
train_data$oil_price %>% points(type='l', col=2)

empl_rate_arima <- auto.arima(train_data$empl_rate)
empl_rate_arima$fitted %>% plot
train_data$empl_rate %>% points(type='l', col=2)

eni_stocks_val_arima <- auto.arima(train_data$eni_stocks_val)
eni_stocks_val_arima$fitted %>% plot
train_data$eni_stocks_val %>% points(type='l', col=2)

euro_dollar_rate_arima <- auto.arima(train_data$euro_dollar_rate)
euro_dollar_rate_arima$fitted %>% plot
train_data$euro_dollar_rate %>% points(type='l', col=2)

residuals <- data.frame('emissions'=emissions_arima$residuals,
                        'oil price'=oil_price_arima$residuals,
                        'empl rate'=empl_rate_arima$residuals,
                        'eni stocks value'=eni_stocks_val_arima$residuals,
                        'euro-dollar rate'=euro_dollar_rate_arima$residuals)
library(corrplot)

corrplot(cor(residuals),
         method='circle',
         type='lower',
         diag=F,
         tl.col='#21A78E',
         cl.col='#21A78E',
         shade.col='#21A78E',
         pch.col='#21A78E')

corrplot(cor(train_data %>% select(weighted_emission, oil_price, empl_rate, eni_stocks_val, euro_dollar_rate)),
         method='circle',
         type='upper',
         diag=F,
         tl.col='#21A78E',
         cl.col='#21A78E',
         shade.col='#21A78E',
         pch.col='#21A78E')


cor(train_data$weighted_emission, 1:idx)
           