#' @description Function that process the data to have temporal-dependent variable shifted of how_many_months
#' @param how_many_months number of months to anticipate the time dependent variables of.
#' @keywords months
#' @export
#' @examples
#' gasoline_month <- read.csv("data/merged_data.csv")
#' gasoline_month <- generate_data_for_forecast(gasoline_month, how_many_months)
generate_data_for_forecast <- function(gasoline_month, how_many_months){
  # 
  n <- nrow(gasoline_month)
  data_for_forecasts <- gasoline_month[(how_many_months+1):n,]
  data_for_forecasts$oil_price <- gasoline_month$oil_price[1:(n-how_many_months)]
  data_for_forecasts$eni_stocks_val <- gasoline_month$eni_stocks_val[1:(n-how_many_months)]
  data_for_forecasts$euro_dollar_rate <- gasoline_month$euro_dollar_rate[1:(n-how_many_months)]
  return(data_for_forecasts)
}

#' @description Open the data and perform a basic cleaning selecting only interesting variables
#'
#' This function allows you to express your love of cats.
#' @param path_to_file path to the CSV file to be opened
open_data <- function(path_to_file){
  gasoline_month <- read.csv(path_to_file) %>% as_tibble() %>%
    mutate(date = as.Date(date, "%Y-%m-%d"))
  gasoline_month <- gasoline_month %>% select(PRICE, X, date, MONTH, weighted_emission, oil_price, empl_rate, eni_stocks_val, euro_dollar_rate)
  return(gasoline_month)
}

#' @description Function to visualize the predictions of a model
#'
#' This function allows you to express your love of cats.
#' @param path_to_file path to the CSV file to be opened
vis_prediction<-function(val, pred_val){
  plot(val, type="l", main="gasoline total-price over time", xaxt = "n", ylab='PRICE (euro * 1000)')
  axis(1, at=c(1,60,120,180,240,300), labels=gasoline_month$date[c(1,60,120,180,240,300)])
  lines(pred_val, lwd=2, col='#21A78E')
  abline(v=241,lty = 2)
}

vis_residual<-function(model){
  plot(residuals(model),type='l',main="Residual Time Series Plot")
  acf(residuals(model),main="Residual ACF Plot", lwd=3)
  }

eval_model<-function(val,pred_val)
{
  residuals = val - pred_val
  RMSE = sqrt(mean(residuals^2))
  cat('The root mean square error of the all data is ', round(RMSE,3),'\n')
  
  y_test_mean = mean(val)
  # Calculate total sum of squares
  tss =  sum((val - y_test_mean)^2 )
  # Calculate residual sum of squares
  rss =  sum(residuals^2)
  # Calculate R-squared
  rsq  =  1 - (rss/tss)
  cat('The R-square of the all data is ', round(rsq,3), '\n')
  
}


#' @description Function that helps to evaluate the models. It plots the predictions
#' versus the real data, the residuals and the residuals ACF, the RMSE and the R-square on the full
#' dataset
#' @param model model to be evaluated.
#' @param all_data full dataset (containing both train and test)
#' @param test_data test data
#' @param LASSO true if the model is a LASSO regression, false otherwise
#' @param ARIMA true if the model is an ARIMA regression, false otherwise
#' @param lambda_opt_index to be used only for LASSO. Provide the index of optimal value of lambda 
#' @value return the RMSE of the model on the test data, the predictions on the all
#' data and the predictions on the test data.
#' @examples
#' gasoline_month <- read.csv("data/merged_data.csv")
#' linear_reg_model <- lm(PRICE ~ X, data = train_data)
#' n <- nrow(gasoline_month)
#' idx <- floor(n*0.8)
#' train_data <- gasoline_month[1:idx,]
#' test_data <- gasoline_month[idx:n,]
#' model_evaluation(linear_reg_model, gasoline_month, test_data)
model_evaluation <- function(model, all_data, test_data, print_AIC = T, ARIMA=F, LASSO = F, lambda_opt_index=NULL){
  n_test <- nrow(test_data)
  if(LASSO & ARIMA){
    cat('LASSO and ARIMA cannot be TRUE at the same time')
    return(-1)
  }
  par(mfrow=c(2,2))
  if(ARIMA){
    pred_test_data <- predict(model$fit, n.ahead=n_test)
    pred_test_data <- as.vector(pred_test_data$pred)
  }else if(LASSO){
    pred_all_data <- predict(model, newx = all_data %>% 
                               select(MONTH, X, weighted_emission, empl_rate, 
                                      euro_dollar_rate, oil_price, eni_stocks_val) %>%
                               as.matrix)
    pred_all_data <- pred_all_data[,lambda_opt_index]
    pred_test_data <- predict(model, newx = test_data %>% 
                                select(MONTH, X, weighted_emission, empl_rate, 
                                       euro_dollar_rate, oil_price, eni_stocks_val) %>%
                                as.matrix)
    pred_test_data <- pred_test_data[,lambda_opt_index]
    vis_prediction(all_data$PRICE, pred_all_data)
    vis_residual(linear_reg_model)
    eval_model(all_data$PRICE, pred_all_data)  
  }else{
    pred_all_data <- predict(model, newdata = all_data)
    pred_test_data <- predict(model, newdata = test_data)
    if(print_AIC) cat('The AIC of the model is ', AIC(model),'\n')
    vis_prediction(all_data$PRICE,pred_all_data)
    vis_residual(linear_reg_model)
    eval_model(all_data$PRICE, pred_all_data)  
  }
  par(mfrow=c(1,1))
  rmse <- rmse(test_data$PRICE, pred_test_data)
  cat('The root mean square error of the test data is ', round(rmse,3),'\n')
  
  if(! ARIMA) return( list('rmse'=rmse, 'predictions'=pred_all_data, 'test_predictions'=pred_test_data) )
  if(ARIMA) return(list('rmse'=rmse, 'test_predictions'=pred_test_data))
}

plot_ts_and_correlogram <- function(price, lag_max=100, differentiate=F, log_transform=F){
  par(mfrow=c(2,1))
  if(log_transform) price=log(price)
  if(differentiate) price <- diff(price)
  plot(price, type='l')
  acf(price,  lag.max=100, lwd=3)
  par(mfrow=c(1,1))
  stationariety_test <- adf.test(price)
  cat('pval for alternative hypothesis: stationary = ', 
      stationariety_test$p.value, '\n')
}


