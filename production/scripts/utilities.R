#' @description Utility function that changes the names of the columns in a 
#' suitable way.
#' @param data dataset or tibble. Data which columns' names have to be changed
#' @param new_prefix_to_add character. prefix to add to all the columns' names
#' @param remove_chars boolean. Default is TRUE. If TRUE, some specific characters will
#' be removed from the names
#' @return The data with new columns' names
change_cols_names <- function(data, new_prefix_to_add, remove_chars=T){
  idx_cols_to_be_renamed <- grepl("^[[:digit:]]", colnames(data))
  if(remove_chars){
    col_names <- sapply(colnames(data)[idx_cols_to_be_renamed], 
                        function(x) gsub('*_T', '', x))
  }else col_names <- colnames(data)[idx_cols_to_be_renamed]
  new_names <- paste0(new_prefix_to_add, col_names)
  colnames(data)[idx_cols_to_be_renamed] <- new_names
  return(data)
}

#' @description Utility function that computes the ratio between two values, 
#' divided by 1000
#' @param num numerator
#' @param den denominator
#' @return The ratio / 1000
calculate_ratio <- function(num, den){
  ratio <- num / den * 1000 # from tons to kg
  return(ratio)
}

#' @description Utility function that selects the columns of the vehicles'
#' emissions dataset to be used to get the ratio (vehicle emission efficiency)
#' and calculates it
#' @param data dataset or tibble. Data which columns' names have to be changed
#' @return Original data with the new 'ratio_CO2_km_year' column
calculate_ratio_cols <- function(data){
  years_as_chars <- (1996:2020) %>% as.character()
  col_names <- colnames(data)
  idx_co2_col <- 7
  idx_km_col <- 32
  new_cols <- rep(NA, nrow(data))
  for(i in idx_co2_col:(idx_km_col-1)){
    num <- data %>% select(all_of(col_names[idx_co2_col]))
    den <- data %>% select(all_of(col_names[idx_km_col]))
    ratio <- calculate_ratio(num, den)
    new_cols <- cbind(new_cols, ratio)
    idx_co2_col <- idx_co2_col+1
    idx_km_col <- idx_km_col+1
  }
  new_cols <- new_cols[,-1]
  colnames(new_cols) <- paste('ratio_CO2_km_', years_as_chars, sep='')
  data <- cbind(data, new_cols)
  return(data)
}

#' @description Utility function that calculate the yearly average of
#' the CO2_km ratio weighted by the km
#' @param data dataset or tibble
#' @return The data with new columns
weighted_avg_col <- function(data){
  data_tmp <- data[,7:ncol(data)]
  idx_km_col <- 26
  idx_ratio_col <- 51
  weighted_emission <- NULL
  for(i in 0:(2020-1996)){
    km <- data_tmp[,i+idx_km_col]
    ratio <- data_tmp[,i+idx_ratio_col]
    ratio[which.max(km)]
    weighted_emission <- c(weighted_emission,
                           weighted.mean(ratio, km) )
  }
  weighted_emission <- data.frame(year = 1996:2020,
                                  weighted_emission = weighted_emission)
  return(weighted_emission)
}

#' @description Utility function that infers missing data through a 
#' polynomial regression and plot the results. It has been used to select the best model
#' @param var variable to be inferred
#' @param model model to use to infer the missing values
#' @param title graphical parameters. Title of the plot
#' @return Predictions to infer the missing data using the provided model 
plot_predictions_NAs <- function(var, model, title, time){
  plot(var~time, main=paste(title, 'data vs predictions'),
       sub = 'NAs will be substituted with the red line',
       type='l')
  pred <- predict(model, newdata = data.frame(var=var[is.na(var)], time=time[is.na(var)]))
  points(pred~time[is.na(var)], col=2, type='p', pch=19)
  return(pred)
}

#' @description Utility function that infers missing data through a 
#' polynomial regression
#' @param data data in which there are missing data to be inferred
#' @param var_name character. Name of the variable which values has 
#' to be inferred
#' @param degrees Integer. Order of the polynomial to be used to infer the
#' missing values. Must be between 1 and 5. Defaul is 3.
#' @param save_result Default FALSE If TRUE, the result are saved in the data 
#' that will be returned. FALSE only if you need to check which is the best
#' solution. 
#' @return Data. If save_result, the data is different from the input one,
#' inference will be performed
substitute_NAs_polinomial_regr <- function(data, var_name, degrees = 3, save_result=F){
  var <- (data %>% as.data.frame() %>% select(all_of(var_name)) )[,1]
  time = 1:nrow(data)
  if (degrees==1){
    mod <- lm(var ~ time)
    pred_NA <- plot_predictions_NAs(var, mod, 'degree = 1', time)
  } else if (degrees==2) {
    mod <- lm(var ~ time + I(time^2))
    pred_NA <- plot_predictions_NAs(var, mod, 'degree = 2', time)
  } 
  else if (degrees==3) {
    mod <- lm(var ~ time + I(time^2) + I(time^3))
    pred_NA <- plot_predictions_NAs(var, mod, 'degree = 3', time)
  } else if(degrees==4) {
    mod <- lm(var ~ time + I(time^2) + I(time^3) + I(time^4))
    pred_NA <- plot_predictions_NAs(var, mod, 'degree = 4', time)
  }else if (degrees==5) {
    mod <- lm(var ~ time + I(time^2) + I(time^3) + I(time^4) + + I(time^5))
    pred_NA <- plot_predictions_NAs(var, mod, 'degree = 5', time)
  } else print('The degree must been btw 1 and 5')
  if(save_result) {
    new_var <- var
    new_var[is.na(var)] <- pred_NA
    data <- data %>% mutate(!!var_name := new_var)
  }
  return(data)
}

#' @description Preprocess the gasoline price data
#' @param gasoline_month gasoline price data
#' @return processed gasoline_month data
preprocess_gasoline <- function(gasoline_month){
  gasoline_month <- gasoline_month %>% mutate(DATA = as.Date(paste(ANNO, CODICE_MESE, "01", 
                                                                   sep="-", format = "%d-%m-%y")))
  colnames(gasoline_month) <- c("YEAR", "MONTH", "MONTH_NAME", "PRDUCT",
                                "PRODUCT_NAME", "PRICE", "IVA_TAX",
                                "ACCISA_TAX", "NET_PRICE", "DATE")
  return(gasoline_month)
}


#' @description Preprocess the CO2 emissions data
#' @param co2_emissions_vehicles CO2 emissions data
#' @return processed CO2 emissions data
preprocess_CO2_emissions <- function(co2_emissions_vehicles){
  cols_to_be_kept <- colnames(co2_emissions_vehicles)[grepl("*T", colnames(co2_emissions_vehicles))]
  cols_to_be_kept <- cols_to_be_kept[7:31]
  co2_emissions_vehicles <- co2_emissions_vehicles %>% 
    select(all_of(colnames(co2_emissions_vehicles)[1:6]),
           all_of(cols_to_be_kept)) %>% 
    filter(Fuel %in% c("Petrol", "Petrol Hybrid"))
  co2_emissions_vehicles <- change_cols_names(co2_emissions_vehicles, "CO2_")
  return(co2_emissions_vehicles)
}

#' @description Preprocess the km by vehicles dataset and the co2 emissions 
#' vehicles dataset
#' @param km_by_vehicles km by vehicles dataset or tibble
#' @param co2_emissions_vehicles co2 emissions vehicles dataset or tibble
#' @return processed km emissions veh
preprocess_km_by_vehicles <- function(km_by_vehicles, co2_emissions_vehicles){
  cols_to_be_kept <- c(1:4, 11:ncol(km_by_vehicles))
  km_by_vehicles <- km_by_vehicles[,cols_to_be_kept]
  km_by_vehicles <- km_by_vehicles %>% filter(Fuel %in% c("Petrol", "Petrol Hybrid"))
  km_by_vehicles <- change_cols_names(km_by_vehicles, "KM_", FALSE)
  
  # co2_emission/km ratio
  km_emissions_veh <- merge(co2_emissions_vehicles, km_by_vehicles) %>% as_tibble()
  km_emissions_veh <- calculate_ratio_cols(km_emissions_veh) %>% 
    mutate()
  return(km_emissions_veh)
}

#' @description Preprocess the oil price dataset 
#' @param oil_price oil price dataset or tibble
#' @return processed oil price
preprocess_oil_price <- function(oil_price){
  oil_price <- oil_price %>% 
    dplyr::rename(oil_price=Price)%>% 
    filter(Date >= as.Date("1996-01-01")) %>%
    select(Date, oil_price)
  return(oil_price)
}

#' @description Preprocess the empl rate dataset 
#' @param empl_rate empl rate dataset or tibble
#' @return processed empl rate
preprocess_employement_rate <- function(empl_rate){
  empl_rate <- empl_rate %>% 
    filter(Correzione == 'dati grezzi', Edizione == '01-Dic-2022', 
           Sesso == 'totale', ETA1=='Y15-64') %>% 
    mutate(date = as.Date(paste(TIME, '-01', sep=''), format="%Y-%m-%d")) %>% 
    select(date, Value) %>%
    dplyr::rename(empl_rate=Value) %>% 
    arrange(date)
  return(empl_rate)
}

#' @description Preprocess the eur dollar rate dataset 
#' @param eur_dollar eur dollar rate dataset or tibble
#' @return processed eur dollar rate dataset
preprocess_eur_dol <- function(eur_dollar){
  eur_dollar <- eur_dollar %>% mutate(date=as.Date(Date), euro_dollar_rate=Adj.Close) %>%
    select(date, euro_dollar_rate)
  return(eur_dollar)
}

#' @description Preprocess the eni stocks dataset 
#' @param eni_stocks eni stocks dataset or tibble
#' @return processed eni stocks dataset
preprocess_eni_stocks <- function(eni){
  eni <- eni %>%
    mutate(date=as.Date(Date), eni_stocks_val = Adj.Close) %>%
    filter(date >= as.Date('1996-01-01')) %>%
    select(date, eni_stocks_val)
  return(eni)
}


#' @description Get gasoline price from the Italian ministery website
#' @param download boolean. If true, the data will be downloaded
#' @details the raw dataset is saved in memory and then proccessed to obtain the
#' desired format
#' @return processed gasoline price dataset
get_gasoline_price <- function(download=T){    
  dest_file <- 'data/gasoline_price.csv'
  if(download){
    gasoline_price_url <- 'https://dgsaie.mise.gov.it/open_data_export.php?export-id=5&export-type=csv'
    download.file(gasoline_price_url, dest_file)
  }
  data <- read.csv(dest_file)
  data <- data %>% preprocess_gasoline
  return(data)
}

#' @description Get weighted vehicle emissions dataset from the ISPRA website
#' @param download boolean. If true, the data will be downloaded
#' @return processed weighted vehicle emissions dataset
get_weighted_vehicle_emissions <- function(download=T){
  dest_file <- 'data/vehicles_data.xlsx'
  if(download){
    vehicles_data_url <- 'http://emissioni.sina.isprambiente.it/wp-content/uploads/2022/04/DatiCopertTrasportoStrada1990-2020.xlsx'
    download.file(vehicles_data_url, dest_file)
  }
  co2_emissions_vehicles <- readxl::read_xlsx(dest_file,
                                      sheet = "CO2_TOTAL") # The last columns *T are the totals
  km_by_vehicles <- readxl::read_xlsx(dest_file,
                              sheet = "veickm") # The last columns *T are the totals
  co2_emissions_vehicles <- co2_emissions_vehicles %>% preprocess_CO2_emissions
  km_emissions_veh <- km_by_vehicles %>% preprocess_km_by_vehicles(co2_emissions_vehicles)
  yearly_CO2emission_km_ratio <- weighted_avg_col(km_emissions_veh)
  return(yearly_CO2emission_km_ratio)
}


#' @description Get employement rate dataset from the ISTAT website 
#' @param download boolean. If true, the data will be downloaded
#' making use of its API 
#' @return processed employement rate dataset
get_empl_rate <- function(download=T){
  data_destination <- './data/empl_rate.csv' 
  if(download){
    flags <- '-kL -H'
    to_csv_command <- ' "Accept: application/vnd.sdmx.data+csv;version=1.0.0"'
    data_url <- '"http://sdmx.istat.it/SDMXWS/rest/data/150_872" '
    # 150_875
    system(paste('curl', flags, to_csv_command, data_url, '>', data_destination))
  }
  empl_rate <- read.csv(data_destination)
  empl_rate <- empl_rate %>% preprocess_employement_rate
  return(empl_rate)
}

#' @description Get oil price dataset
#' @return processed oil price dataset
get_oil_price <- function(){
  oil_price <- quantmod::getSymbols('CL=F',src='yahoo',auto.assign=FALSE, periodicity='monthly', from=as.Date('1996-01-01'))
  dates <- zoo::index(oil_price)
  oil_price <- oil_price %>% as_tibble  %>%
    mutate(Price = `CL=F.Close`, Date=as.Date(dates)) %>% 
    select(Price, Date) %>% preprocess_oil_price 
  return(oil_price)
}

#' @description marge the datasets that are used by our model in one single dataset
#' @param get_gasoline_price dataset
#' @param yearly_CO2emission_km_ratio dataset
#' @param empl_rate dataset
#' @param oil_price dataset
#' @return processed merged dataset
merge_sources_in_one_ds <- function(gasoline_price, yearly_CO2emission_km_ratio, empl_rate, oil_price){
  tmp <- merge(gasoline_price, yearly_CO2emission_km_ratio, by.x='YEAR', by.y='year',all.x=T)
  tmp <- tmp %>% mutate(date = as.Date(paste('1/',MONTH, '/', YEAR, sep=''),
                                       format="%d/%m/%Y"))
  tmp <- merge(tmp, oil_price, by.x='date', by.y='Date', all=T)
  tmp <- merge(tmp, empl_rate, by.x='date', by.y='date', all=T)
  tmp <- merge(tmp, eni, by.x='date', by.y='date', all=T)
  data <- merge(tmp, eur_dollar, by.x='date', by.y='date', all=T)
  rows_with_no_price_available <- which(is.na(data$PRICE))
  data <- data[-rows_with_no_price_available,]
  data <- data %>% select('date', 'PRICE', 'IVA_TAX', 'ACCISA_TAX', 'NET_PRICE', 'MONTH',
                          'weighted_emission', 'oil_price', 'empl_rate', 'eni_stocks_val',
                          'euro_dollar_rate')
  data <-  data %>% 
    substitute_NAs_polinomial_regr('euro_dollar_rate', degrees=2, save_result = T) %>%
    substitute_NAs_polinomial_regr('weighted_emission', degrees=1, save_result = T) %>%
    substitute_NAs_polinomial_regr('empl_rate', degrees=3, save_result = T)
  smoothed_weighted_emission <- smooth.spline(data$weighted_emission,df=20)
  data <- data %>% mutate(weighted_emission=smoothed_weighted_emission$y)
  write.csv(data, 'data/merged_data.csv')
}
