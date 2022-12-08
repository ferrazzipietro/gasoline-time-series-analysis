
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
  axis(1, at=c(1,60,120,180,240,300), labels=gasoline_month$YEAR[c(1,60,120,180,240,300)])
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
gasoline_month <- read.csv("data/monthly_gasoline_prices_1996_2022.csv")
#gasoline_month <- read.csv("data/merged_data.csv")
gasoline_month$DATE <- as.Date(gasoline_month$DATE, "%Y-%m-%d")
plot(gasoline_month$PRICE ~ gasoline_month$DATE, type="l",
     main="gasoline total-price over time")


# train test split
#training -- 75% 241 data points
#testing --- 25% 80  data points

train_data=gasoline_month[1:241,]
test_data=gasoline_month[242:321,]

#***************************** Linear regression ***************************************#

#linear_reg_model <- lm(PRICE ~ MONTH_NAME, data = train_data) #linear
linear_reg_model <- lm(PRICE ~ MONTH_NAME + X,data = train_data) #multiple linear
summary(linear_reg_model)
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
plot(g3, se=T)

gam_normal <- gam(PRICE~MONTH+X, data=train_data)
pred_gam_normal <- predict(gam_normal, newdata=gasoline_month)
vis_prediction(gasoline_month$PRICE,pred_gam_normal)
vis_residual(gam_normal)
eval_model(gasoline_month$PRICE,pred_gam_normal)

#Perform stepwise selection using gam scope
#Values for df should be greater than 1, with df=1 implying a linear fit

sc = gam.scope(gasoline_month$MONTH+gasoline_month$X, response=gasoline_month$PRICE, arg=c("df=2","df=3","df=4"))
gam_step<- step.Gam(gam_normal, scope=sc, trace=T)
summary(g4)

### gam with s
g1 <- gam(NZ~s(tt)+seas+s(Japan))


gam_s <- gam(PRICE~s(MONTH+X), data=train_data)
pred_gam_s <- predict(gam_s, newdata=gasoline_month)
vis_prediction(gasoline_month$PRICE,pred_gam_s)
vis_residual(gam_s)
eval_model(gasoline_month$PRICE,pred_gam_s)

### gam with loess

gam_loess <- gam(PRICE~lo(MONTH+X), data=train_data)
pred_gam_loess <- predict(gam_loess, newdata=gasoline_month)
vis_prediction(gasoline_month$PRICE,pred_gam_loess)
vis_residual(gam_loess)
eval_model(gasoline_month$PRICE,pred_gam_loess)




#***************************** Gradient Boosting ***************************************#

gbm = gbm(PRICE~MONTH+X, data = train_data,distribution = "gaussian", cv.folds = 10,shrinkage = .01,n.minobsinnode = 10, n.trees = 500)

pred_gbm <- predict(gbm, newdata=gasoline_month)
vis_prediction(gasoline_month$PRICE,pred_gbm)


residue = gasoline_month$PRICE - pred_gbm
par(mfrow=c(1,2))
plot(residue,type='l',main="Residual Time Series Plot")
acf(residue,main="Residual ACF Plot")
par(mfrow=c(1,1))

eval_model(gasoline_month$PRICE,pred_gbm)

