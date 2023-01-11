#setwd("~/Desktop/gasoline-time-series-analysis")
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

linear_reg_model <- lm(PRICE ~ as.factor(MONTH) + X + weighted_emission  + 
                         empl_rate  + euro_dollar_rate + oil_price + eni_stocks_val,
                       data = train_data) 
summary(linear_reg_model)
#plot(linear_reg_model)

linear_reg_model <- lm(PRICE ~ as.factor(MONTH) + X + weighted_emission  + 
                         empl_rate  + euro_dollar_rate + oil_price ,
                       data = train_data) 
summary(linear_reg_model)

eval_linear <- model_evaluation(linear_reg_model, gasoline_month, test_data)


#***************************** Lasso regression ***************************************#

lasso_reg_model <- glmnet(y = train_data %>% select(PRICE) %>% as.matrix,
                          x = train_data %>% select(MONTH, X, weighted_emission,
                                                    empl_rate, euro_dollar_rate, 
                                                    oil_price, eni_stocks_val) %>% as.matrix,
                          alpha = 1)
plot(lasso_reg_model, xvar="lambda", label=T)# in funzione di lambda.

previsione <- predict(lasso_reg_model, newx = test_data %>% select(MONTH, X, weighted_emission,
                                                                   empl_rate, euro_dollar_rate, 
                                                                   oil_price, eni_stocks_val) %>%
                        as.matrix)
# Calcolo gli errori e scelgo il modello sulla loro base
errori <- (test_data$PRICE - previsione)^2 %>% apply(2, mean) # per ogni colonna calcolo la media degli errori
lambda_opt_ind <- errori %>% which.min()
lambda_opt <- lasso_reg_model$lambda[lambda_opt_ind]
predict(lasso_reg_model, type="coefficients", s=lambda_opt) # vettore dei coefficienti ottimi stimati
summary(lasso_reg_model)
pred.lasso <- previsione[,lambda_opt_ind]

eval_lasso <- model_evaluation(lasso_reg_model, gasoline_month, test_data, LASSO=T, lambda_opt_index=lambda_opt_ind)

#*************************************** GAM *******************************************#

#Start with a linear model (df=1)

gam_normal <- gam(PRICE~MONTH + X + weighted_emission +  oil_price + 
                    empl_rate + eni_stocks_val + euro_dollar_rate,
                  data=train_data)
summary(gam_normal)
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
plot(gam_s)

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

sarima21 <- sarima(price, 2,1,3, no.constant=T)
sarima21$ttable
pred <- predict(sarima21$fit, n.ahead=n_test)

eval_arima <- model_evaluation(sarima21, gasoline_month, test_data, ARIMA=T)



#*************************************** FINAL COMPARAISON *******************************************#
eval_linear$rmse
eval_lasso$rmse
eval_gam_loess$rmse
eval_gam_normal$rmse
eval_gam_s$rmse
eval_gbm$rmse
eval_arima$rmse

rmse <- c(eval_linear$rmse,
          eval_lasso$rmse,
          #eval_gam_loess$rmse,
          eval_gam_normal$rmse,
          eval_gam_s$rmse,
          eval_gbm$rmse,
          eval_arima$rmse)
plot(rmse/1000, pch=15, col='#21A78E', cex=2,
     ylab='RMSE (€)',ylim=c(0.05,0.19), xlim=c(0,7))
text(1:length(rmse), rmse/1000, c('linear', 'lasso', 'linear gam', 'spline gam',
                                 'gbm', 'arima'), pos=1, offset = 1)
grid()

#******************** BEST MODEL: ARIMA over LASSO ***************************#

res_lasso<- eval_lasso$predictions[1:idx] - train_data$PRICE
plot(res_lasso~train_data$date, type='l', 
     main='Is there heteroschedastity?', xlab='date')
acf(diff(res_lasso), lag.max = 80)

arima_on_lasso <- sarima(res_lasso, 0,1,3, 0,1,1,S=12, no.constant = T,
                         max.lag=50)

arima_on_lasso$ttable
arima_on_lasso_pred <- predict(arima_on_lasso$fit, n.ahead=n_test) 
arima_on_lasso_pred <- arima_on_lasso_pred$pred %>% as.vector

final_forecasts <- eval_lasso$predictions[(idx):n] + arima_on_lasso_pred

plot(test_data$PRICE, type='l')
points(1:65,final_forecasts, col=2, type='l')
points(1:65,eval_lasso$predictions[(idx):n], col=3, type='l')

rmse(test_data$PRICE,final_forecasts)
rmse(test_data$PRICE, eval_lasso$test_predictions)
plot(gasoline_month$PRICE, type='l')


#******************** ARIMAX ***************************#
arimax <- sarima(train_data$PRICE, 2,1,2, 1,0,0,S=12, 
                          xreg = train_data %>% 
                            select(weighted_emission,
                                   empl_rate, euro_dollar_rate, 
                                   oil_price, eni_stocks_val) %>%
                            as.matrix
                            , no.constant=T)

arimax_pred <- predict(arimax$fit, newxreg=test_data %>% 
                         select(weighted_emission,empl_rate, euro_dollar_rate, 
                                oil_price, eni_stocks_val) %>% as.matrix,
                       n.ahead=n_test)
plot(test_data$PRICE, type='l')
points(1:65,as.vector(arimax_pred$pred), col=2, type='l')
rmse(test_data$PRICE,lasso_arima_pred$pred)

#*************************************** BASS MODEL *******************************************#
bm <- BM(gasoline_month$PRICE,display = T)
summary(bm)
GBMr1tw<- GBM(gasoline_month$PRICE,shock = "exp",nshock = 1,prelimestimates = c(8.503278e+05, 9.549630e-04, 5.497510e-03, 130, -0.1,-0.1))

GBMr1tw<- GBM(gasoline_month$PRICE,shock = "rett",nshock = 1,prelimestimates = c(8.503278e+05, 9.549630e-04, 5.497510e-03, 160, 220,0.1))
summary(GBMr1tw)


#****************************** PLOT THE PREDICTION OF EVERY MODEL ***********************#
vis_prediction(gasoline_month$PRICE, eval_linear$predictions)
vis_prediction(gasoline_month$PRICE, eval_lasso$predictions)
vis_prediction(gasoline_month$PRICE, eval_gam_loess$predictions)
vis_prediction(gasoline_month$PRICE, eval_gam_normal$predictions)
vis_prediction(gasoline_month$PRICE, eval_gam_s$predictions)
vis_prediction(gasoline_month$PRICE, eval_gbm$predictions)
vis_prediction(gasoline_month$PRICE, eval_arima$predictions)


