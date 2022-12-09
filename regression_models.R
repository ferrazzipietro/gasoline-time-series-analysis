
# OLS linear regression
# include variables
# Gradient boosting
# GAM model
setwd("~/Desktop/gasoline-time-series-analysis")
options(warn=0)
#install.packages('gam')
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


vis_prediction<-function(val, pred_val){
  plot(val, type="l", main="gasoline total-price over time", xaxt = "n")
  #axis(1, at=c(1,60,120,180,240,300), labels=gasoline_month$date[c(1,60,120,180,240,300)])
  lines(pred_val, lwd=2, col='red')
  abline(v=241,lty = 2)}

vis_residual<-function(model){
  par(mfrow=c(1,2))
  plot(residuals(model),type='l',main="Residual Time Series Plot")
  acf(residuals(model),main="Residual ACF Plot")
  par(mfrow=c(1,1))}

eval_model<-function(val,pred_val)
{
  residuals = val - pred_val
  RMSE = sqrt(mean(residuals^2))
  cat('The root mean square error of the test data is ', round(RMSE,3),'\n')
  
  y_test_mean = mean(val)
  # Calculate total sum of squares
  tss =  sum((val - y_test_mean)^2 )
  # Calculate residual sum of squares
  rss =  sum(residuals^2)
  # Calculate R-squared
  rsq  =  1 - (rss/tss)
  cat('The R-square of the test data is ', round(rsq,3), '\n')
  
}


#suppressMessages(library(dplyr))


# Reading files and Primary visualisation
gasoline_month <- read.csv("data/merged_data.csv") %>% as_tibble() %>%
  mutate(date = as.Date(date, "%Y-%m-%d"))
n <- nrow(gasoline_month)
gasoline_month %>% colnames()
vars_idxs <- c(4, 5, 14:18)
vars <- (gasoline_month %>% colnames)[vars_idxs]
# train test split
#training -- 75% 241 data points
#testing --- 25% 80  data points

replace_NA_with_0 <- T
if (replace_NA_with_0) gasoline_month <- gasoline_month %>% replace(is.na(.), 0)

explanatory <- F
idx <- 241
if (explanatory) idx <- n

train_data=gasoline_month[1:idx,]
test_data=gasoline_month[idx:n,]

#***************************** Linear regression ***************************************#

linear_reg_model <- lm(PRICE ~ MONTH + X + weighted_emission + oil_price + 
                         empl_rate + eni_stocks_val + euro_dollar_rate,
                       data = train_data) #multiple linear
summary(linear_reg_model)
plot(linear_reg_model)
pred_linreg<-predict(linear_reg_model, newdata = gasoline_month)


#plot(gasoline_month$DATE,gasoline_month$PRICE, type="l", main="gasoline total-price over time")
#axis(1, at=c(1,60,120,180,240,300), labels=gasoline_month$YEAR[c(1,60,120,180,240,300)])
#lines(gasoline_month$DATE,pred_linreg, lwd=2, col='red')
#abline(gasoline_month$DATE,v=241,type='d')
#tsdisplay(residuals(linear_reg_model))

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

