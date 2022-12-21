#setwd("~/Desktop/gasoline-time-series-analysis")
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
source('utilities_models.R')
# library(devtools)
# setwd('/Users/pietroferrazzi/Desktop/uni/times_series/progetto')
# setwd('./GasolinePackage/')
# install('GasolinePackage')
# setwd('/Users/pietroferrazzi/Desktop/uni/times_series/progetto/gasoline-time-series-analysis/')

# Reading files and Primary visualisation
gasoline_month <- open_data("data/merged_data.csv")
vars_idxs <- 2:ncol(gasoline_month)
n <- nrow(gasoline_month)
vars <- (gasoline_month %>% colnames)



FORECAST = TRUE
if(FORECAST) gasoline_month <- generate_data_for_forecast(gasoline_month, 3)

# train test split
#training -- 75% 241 data points
#testing --- 25% 80  data points

replace_NA_with_0 <- T
if (replace_NA_with_0) gasoline_month <- gasoline_month %>% replace(is.na(.), 0)

EXPLANATORY <- F
idx <- 241
if (EXPLANATORY) idx <- n

train_data=gasoline_month[1:idx,]
test_data=gasoline_month[idx:n,]

#***************************** Linear regression ***************************************#

linear_reg_model <- lm(PRICE ~ MONTH + X + weighted_emission  + 
                         empl_rate  + euro_dollar_rate,
                       data = train_data) #multiple linear
summary(linear_reg_model)
plot(linear_reg_model)
pred_linreg<-predict(linear_reg_model, newdata = gasoline_month)

vis_prediction(gasoline_month$PRICE,pred_linreg)
vis_residual(linear_reg_model)
eval_model(gasoline_month$PRICE,pred_linreg)



#*************************************** GAM *******************************************#

#Start with a linear model (df=1)

gam_normal <- gam(PRICE~MONTH + X + weighted_emission +  oil_price + 
                    empl_rate + eni_stocks_val + euro_dollar_rate,
                  data=train_data)
summary(gam_normal)
pred_gam_normal <- predict(gam_normal, newdata=gasoline_month)
vis_prediction(gasoline_month$PRICE,pred_gam_normal)
vis_residual(gam_normal)
eval_model(gasoline_month$PRICE,pred_gam_normal)

#Perform stepwise selection using gam scope
#Values for df should be greater than 1, with df=1 implying a linear fit
only_interesting_vars <- train_data[,vars_idxs] %>% mutate(response=train_data$PRICE)
scope <- ncol(only_interesting_vars) # PRICE column
sc = gam.scope(only_interesting_vars[,-scope], response=scope, arg=c("df=2","df=3","df=4"))
gam_s<- step.Gam(gam_normal, scope=sc, trace=F)
summary(gam_s)
AIC(gam_s)
par(mfrow=c(3,2))
plot(gam_s, se=T)
par(mfrow=c(1,1))

pred_gam_s <- predict(gam_s, newdata=gasoline_month)
vis_prediction(gasoline_month$PRICE,pred_gam_s)
vis_residual(gam_s)
eval_model(gasoline_month$PRICE,pred_gam_s)

### gam with loess
sc_loess = gam.scope(only_interesting_vars[,-scope], response=scope, arg=c("df=2","df=3","df=4"),
                     smoother = 'lo')
gam_loess<- step.Gam(gam_normal, scope=sc_loess, trace=F)
summary(gam_loess)
AIC(gam_s)
AIC(gam_loess)
par(mfrow=c(3,2))
plot(gam_s, se=T)
par(mfrow=c(1,1))
pred_gam_loess <- predict(gam_loess, newdata=gasoline_month)
vis_prediction(gasoline_month$PRICE,pred_gam_loess)
vis_residual(gam_loess)
eval_model(gasoline_month$PRICE,pred_gam_loess)



#***************************** Gradient Boosting ***************************************#

gbm = gbm(PRICE~MONTH + X + weighted_emission + oil_price + 
            empl_rate + eni_stocks_val + euro_dollar_rate,
          data = train_data,distribution = "gaussian", 
          cv.folds = 10,
          shrinkage = .01,
          n.minobsinnode = 10, 
          n.trees = 500)

pred_gbm <- predict(gbm, newdata=gasoline_month)
vis_prediction(gasoline_month$PRICE,pred_gbm)


residue = gasoline_month$PRICE - pred_gbm
par(mfrow=c(1,2))
plot(residue,type='l',main="Residual Time Series Plot")
acf(residue,main="Residual ACF Plot")
par(mfrow=c(1,1))

eval_model(gasoline_month$PRICE,pred_gbm)

##### ARIMA ###########

## READ and PREPARE DATA

gasoline_month <- read.csv('data/merged_data.csv')
gasoline_month$date <- gasoline_month$date %>% as.Date
gasoline_month <- gasoline_month[,-1]

## EXPLORATORY ANALISYS
head(gasoline_month)

plot(gasoline_month$PRICE ~ gasoline_month$date, type="b",
     main="gasoline total-price over time")
plot(gasoline_month$NETTO ~ gasoline_month$date, type="b",
     main="gasoline net-price over time")


##create a variable 'trend'
tt<- 1:NROW(gasoline_month)

##acf of variable "gasoline_month$NETTO "
acf(gasoline_month$PRICE,  lag.max=100)

# ARIMA

plot(diff(log(gasoline_month$PRICE)) ~ gasoline_month$date[-1], type="b",
     main="differentiate (lag1) log-gasoline price over time")

adf.test(diff(log(gasoline_month$PRICE))) # la diff. è stazionaria

par(mfrow=c(1,2))
acf(diff(gasoline_month$PRICE))
pacf(diff(gasoline_month$PRICE))
arima21 <- sarima(log(gasoline_month$PRICE), 2,1,0)

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

