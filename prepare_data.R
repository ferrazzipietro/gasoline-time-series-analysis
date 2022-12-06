suppressMessages(library(readxl))
suppressMessages(library(lmtest) )
suppressMessages(library(forecast))
suppressMessages(library(DIMORA))
suppressMessages(library(dplyr))
suppressMessages(library(tseries))
suppressMessages(library(astsa))
suppressMessages(library(knitr))
suppressMessages(library(readxl))


## READ and PREPARE DATA

# gasoline price
gasoline_month <- read.csv("data/prezzi_mensili_benzina_dal_1996_a_20221028.csv")
gasoline_month <- gasoline_month %>% mutate(date = as.Date(paste(ANNO, CODICE_MESE, "01", 
                                                                 sep="-", format = "%d-%m-%y")))
# vehicles emissions (CO2)
co2_emissions_vehicles <- read_xlsx("data/DatiCopertTrasportoStrada1990-2020.xlsx",
                                    sheet = "CO2_TOTAL") # The last columns *T are the totals
cols_to_be_kept <- colnames(co2_emissions_vehicles)[grepl("*T", colnames(co2_emissions_vehicles))]
cols_to_be_kept <- cols_to_be_kept[7:31]
co2_emissions_vehicles <- co2_emissions_vehicles %>% 
  select(all_of(colnames(co2_emissions_vehicles)[1:6]),
         all_of(cols_to_be_kept)) %>% 
  filter(Fuel %in% c("Petrol", "Petrol Hybrid"))

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

co2_emissions_vehicles <- change_cols_names(co2_emissions_vehicles, "CO2_")


# km per vehicle cathegory
km_by_vehicles <- read_xlsx("data/DatiCopertTrasportoStrada1990-2020.xlsx",
                            sheet = "veickm") # The last columns *T are the totals
cols_to_be_kept <- c(1:4, 11:ncol(km_by_vehicles))
km_by_vehicles <- km_by_vehicles[,cols_to_be_kept]
km_by_vehicles <- km_by_vehicles %>% filter(Fuel %in% c("Petrol", "Petrol Hybrid"))
km_by_vehicles <- change_cols_names(km_by_vehicles, "KM_", FALSE)

# co2_emission/km ratio
km_emissions_veh <- merge(co2_emissions_vehicles, km_by_vehicles) %>% as_tibble()


calculate_ratio <- function(num, den){
  ratio <- num / den * 1000 # from tons to kg
  return(ratio)
}

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

km_emissions_veh <- calculate_ratio_cols(km_emissions_veh)

# weighted average of the emissions per year using km and CO2/km ratio
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

yearly_CO2emission_km_ratio <- weighted_avg_col(km_emissions_veh)

#oil price
oil_price <- read_xlsx("data/oil_price_monthy.xlsx")
oil_price <- oil_price %>% mutate(Date = as.Date(Month))
oil_price <- oil_price %>% filter(Date >= as.Date("1996-01-01")) %>%
  select(Date, Price)

# build the final data set
oil_price <- oil_price %>% rename( oil_price=Price)
tmp1 <- merge(gasoline_month, yearly_CO2emission_km_ratio, by.x='ANNO', by.y='year')
tmp1 <- tmp1 %>% mutate(date = as.Date(paste('1/',CODICE_MESE, '/', ANNO, sep=''),
                                       format="%d/%m/%Y"))
data <- merge(tmp1, oil_price, by.x='date', by.y='Date')
data <- data %>% select('date', 'PREZZO', 'IVA', 'ACCISA', 'NETTO', 
                        'weighted_emission', 'oil_price')
write.csv(data, 'data/merged_data.csv')


