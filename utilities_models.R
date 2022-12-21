#' Open data
#' @name open_data
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

#' @description Function that process the data to have temporal-dependent variable shifted of how_many_months
#' @param how_many_months number of months to anticipate the time dependent variables of.
#' @keywords months
#' @export
#' @examples
#' gasoline_month <- read.csv("data/merged_data.csv")
#' gasoline_month <- generate_data_for_forecast(gasoline_month, how_many_months)

generate_data_for_forecast <- function(gasoline_month, how_many_months){
  # 
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
  plot(val, type="l", main="gasoline total-price over time", xaxt = "n")
  #axis(1, at=c(1,60,120,180,240,300), labels=gasoline_month$date[c(1,60,120,180,240,300)])
  lines(pred_val, lwd=2, col='red')
  abline(v=241,lty = 2)
}

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
