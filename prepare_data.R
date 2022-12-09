suppressMessages(library(readxl))
suppressMessages(library(lmtest) )
suppressMessages(library(forecast))
suppressMessages(library(DIMORA))
suppressMessages(library(dplyr))
suppressMessages(library(tseries))
suppressMessages(library(astsa))
suppressMessages(library(knitr))
suppressMessages(library(readxl))
suppressMessages(source('utilities_data_preparation.R'))
args = commandArgs(trailingOnly=TRUE)

preprocess_gasoline <- function(gasoline_month){
  gasoline_month <- gasoline_month %>% mutate(DATA = as.Date(paste(ANNO, CODICE_MESE, "01", 
                                                                   sep="-", format = "%d-%m-%y")))
  
  colnames(gasoline_month) <- c("YEAR", "MONTH", "MONTH_NAME", "PRDUCT",
                                "PRODUCT_NAME", "PRICE", "IVA_TAX",
                                "ACCISA_TAX", "NET_PRICE", "DATE")
  return(gasoline_month)
}

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

preprocess_km_by_vehicles <- function(km_by_vehicles, co2_emissions_vehicles){
  cols_to_be_kept <- c(1:4, 11:ncol(km_by_vehicles))
  km_by_vehicles <- km_by_vehicles[,cols_to_be_kept]
  km_by_vehicles <- km_by_vehicles %>% filter(Fuel %in% c("Petrol", "Petrol Hybrid"))
  km_by_vehicles <- change_cols_names(km_by_vehicles, "KM_", FALSE)
  
  # co2_emission/km ratio
  km_emissions_veh <- merge(co2_emissions_vehicles, km_by_vehicles) %>% as_tibble()
  km_emissions_veh <- calculate_ratio_cols(km_emissions_veh)
  return(km_emissions_veh)
}

preprocess_oil_price <- function(oil_price){
  oil_price <- oil_price %>% 
    mutate(Date = as.Date(Month))%>% 
    dplyr::rename(oil_price=Price)%>% 
    filter(Date >= as.Date("1996-01-01")) %>%
    select(Date, oil_price)
  return(oil_price)
}

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

preprocess_eur_dol <- function(eur_dollar){
  eur_dollar <- eur_dollar %>% mutate(date=as.Date(Date), euro_dollar_rate=Adj.Close) %>%
    select(date, euro_dollar_rate)
  return(eur_dollar)
}

preprocess_eni_stocks <- function(eni){
  eni <- eni %>%
    mutate(date=as.Date(Date), eni_stocks_val = Adj.Close) %>%
    filter(date >= as.Date('1996-01-01')) %>%
    select(date, eni_stocks_val)
  return(eni)
}

main <- function(){
  gasoline_month <- read.csv("data/prezzi_mensili_benzina_dal_1996_a_20221028.csv")
  co2_emissions_vehicles <- read_xlsx("data/DatiCopertTrasportoStrada1990-2020.xlsx",
                                      sheet = "CO2_TOTAL") # The last columns *T are the totals
  
  km_by_vehicles <- read_xlsx("data/DatiCopertTrasportoStrada1990-2020.xlsx",
                              sheet = "veickm") # The last columns *T are the totals
  oil_price <- read_xlsx("data/oil_price_monthy.xlsx")
  empl_rate <- read.csv('data/employement_rate_monthly.csv')
  eur_dollar <- read.csv('data/euro_usd_rate.csv')
  eni <- read.csv('data/eni_stock_data.csv')
  
  gasoline_month <- gasoline_month %>% preprocess_gasoline
  oil_price <- oil_price %>% preprocess_oil_price
  empl_rate <- empl_rate %>% preprocess_employement_rate
  eur_dollar <-  eur_dollar %>% preprocess_eur_dol
  eni <- eni %>% preprocess_eni_stocks
  co2_emissions_vehicles <- co2_emissions_vehicles %>% preprocess_CO2_emissions
  km_emissions_veh <- km_by_vehicles %>% preprocess_km_by_vehicles(co2_emissions_vehicles)
  yearly_CO2emission_km_ratio <- weighted_avg_col(km_emissions_veh)
  
  # build the final data set
  tmp <- merge(gasoline_month, yearly_CO2emission_km_ratio, by.x='YEAR', by.y='year',all.x=T)
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
  
  write.csv(data, 'data/merged_data_NAs.csv')
  
  if(args[1]=="true" | args[1]=="True" | args[1]=="TRUE"){ 
    # NAs inference
    data <-  data %>% 
      substitute_NAs_polinomial_regr('euro_dollar_rate', degrees=2, save_result = T) %>%
      substitute_NAs_polinomial_regr('weighted_emission', degrees=1, save_result = T) %>%
      substitute_NAs_polinomial_regr('empl_rate', degrees=3, save_result = T)
    
    write.csv(data, 'data/merged_data.csv')
  }
}

main()
