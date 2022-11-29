
# OLS linear regression
# include variables
# Gradient boosting
# GAM model

library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(dplyr)
library(tseries) 
library(astsa)
library(knitr)


options(warn=0)
#suppressMessages(library(dplyr))


# Reading files and Primary visualisation
gasoline_month <- read.csv("data/monthly_gasoline_prices_1996_2022.csv")
gasoline_month$DATE <- as.Date(gasoline_month$DATE, "%Y-%m-%d")
plot(gasoline_month$PRICE ~ gasoline_month$DATE, type="l",
     main="gasoline total-price over time")


# train test split
#training -- 75% 241 data points
#testing --- 25% 80  data points

train_data=gasoline_month[1:241,]
test_data=gasoline_month[242:321,]

####### Linear regression

#linear_reg_model <- lm(PRICE ~ MONTH_NAME, data = train_data) #linear
linear_reg_model <- lm(PRICE ~ MONTH_NAME + X,data = train_data) #multiple linear
summary(linear_reg_model)
pred_linreg<-predict(linear_reg_model, newdata = gasoline_month)


#plot(gasoline_month$DATE,gasoline_month$PRICE, type="l", main="gasoline total-price over time")
#axis(1, at=c(1,60,120,180,240,300), labels=gasoline_month$YEAR[c(1,60,120,180,240,300)])
#lines(gasoline_month$DATE,pred_linreg, lwd=2, col='red')
#abline(gasoline_month$DATE,v=241,type='d')

plot(gasoline_month$PRICE, type="l", main="gasoline total-price over time", xaxt = "n")
axis(1, at=c(1,60,120,180,240,300), labels=gasoline_month$YEAR[c(1,60,120,180,240,300)])
lines(pred_linreg, lwd=2, col='red')
abline(v=241,lty = 2)



####### GAM

mod_gam2 = gam(Overall ~ s(Income) + s(Edu) + s(Health), data = pisa)
summary(mod_gam2)

# Note that mod_gam2$model is the data that was used in the modeling process, 
# so it will have NAs removed.
testdata = data.frame(
  Income = seq(.4, 1, length = 100),
  Edu    = mean(mod_gam2$model$Edu),
  Health = mean(mod_gam2$model$Health)
)

predictions = predict(
  mod_gam2,
  newdata = testdata,
  type = 'response',
  se = TRUE
)

df_preds = data.frame(testdata, predictions) %>%
  mutate(lower = fit - 1.96 * se.fit,
         upper = fit + 1.96 * se.fit)

ggplot(aes(x = Income, y = fit), data = df_preds) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'gray92') +
  geom_line(color = '#56B4E9')



#### Gradient Boosting








#### Non Parametrin (Knn, local regression)





