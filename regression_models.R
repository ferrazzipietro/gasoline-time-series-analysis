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
library(Metrics)
source('utilities_models.R')

# Reading files and Primary visualisation
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


#***************************** Linear regression ***************************************#

linear_reg_model <- lm(PRICE ~ MONTH + X + weighted_emission  + 
                         empl_rate  + euro_dollar_rate,
                       data = train_data) 
summary(linear_reg_model)
plot(linear_reg_model)

eval_linear <- model_evaluation(linear_reg_model, gasoline_month, test_data)

#*************************************** GAM *******************************************#

#Start with a linear model (df=1)

gam_normal <- gam(PRICE~MONTH + X + weighted_emission +  oil_price + 
                    empl_rate + eni_stocks_val + euro_dollar_rate,
                  data=train_data)
#summary(gam_normal)
eval_gam_normal <- model_evaluation(gam_normal, gasoline_month, test_data)


#Perform stepwise selection using gam scope
#Values for df should be greater than 1, with df=1 implying a linear fit
only_interesting_vars <- train_data[,2:ncol(train_data)] %>% mutate(response=train_data$PRICE)
scope <- ncol(only_interesting_vars)
sc = gam.scope(only_interesting_vars[,-scope], response=scope, arg=c("df=2","df=3","df=4"))
gam_s<- step.Gam(gam_normal, scope=sc, trace=F)
summary(gam_s)
AIC(gam_s)
par(mfrow=c(3,2))
plot(gam_s, se=T)
par(mfrow=c(1,1))

eval_gam_s <- model_evaluation(gam_s, gasoline_month, test_data)

### gam with loess
sc_loess = gam.scope(only_interesting_vars[,-scope], response=scope, arg=c("df=2","df=3","df=4"),
                     smoother = 'lo')
gam_loess<- step.Gam(gam_normal, scope=sc_loess, trace=F)
summary(gam_loess)
par(mfrow=c(3,2))
plot(gam_s, se=T)
par(mfrow=c(1,1))
eval_gam_loess <- model_evaluation(gam_loess, gasoline_month, test_data)


#***************************** Gradient Boosting ***************************************#

gbm = gbm(PRICE~MONTH + X + weighted_emission + oil_price + 
            empl_rate + eni_stocks_val + euro_dollar_rate,
          data = train_data,distribution = "gaussian", 
          cv.folds = 1,
          shrinkage = .01,
          n.minobsinnode = 2, 
          n.trees = 1500)
#summary(gbm)
eval_gbm <- model_evaluation(gbm, gasoline_month, test_data, print_AIC=F)


#*************************************** ARIMA *******************************************#
price <- train_data$PRICE
plot_ts_and_correlogram(price)
plot_ts_and_correlogram(price, differentiate = T)

sarima21 <- sarima(price, 2,1,0, no.constant=T)
pred <- predict(sarima21$fit, n.ahead=n_test)

eval_arima <- model_evaluation(sarima21, gasoline_month, test_data, ARIMA=T)



#*************************************** FINAL COMPARAISON *******************************************#
eval_linear$rmse
eval_gam_loess$rmse
eval_gam_normal$rmse
eval_gam_s$rmse
eval_gbm$rmse
eval_arima$rmse

#*************************************** BASS MODEL *******************************************#
bm <- BM(gasoline_month$PRICE,display = T)
summary(bm)
GBMr1tw<- GBM(gasoline_month$PRICE,shock = "exp",nshock = 1,prelimestimates = c(8.503278e+05, 9.549630e-04, 5.497510e-03, 130, -0.1,-0.1))

GBMr1tw<- GBM(gasoline_month$PRICE,shock = "rett",nshock = 1,prelimestimates = c(8.503278e+05, 9.549630e-04, 5.497510e-03, 160, 220,0.1))
summary(GBMr1tw)

