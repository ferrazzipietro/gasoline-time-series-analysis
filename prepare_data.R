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

## READ and PREPARE DATA

# gasoline price
gasoline_month <- read.csv("data/monthly_gasoline_prices_1996_2022.csv")
gasoline_month <- gasoline_month %>% mutate(date = as.Date(paste(YEAR, MONTH, "01", 
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
km_emissions_veh <- calculate_ratio_cols(km_emissions_veh)

# weighted average of the emissions per year using km and CO2/km ratio
yearly_CO2emission_km_ratio <- weighted_avg_col(km_emissions_veh)

#oil price
oil_price <- read_xlsx("data/oil_price_monthy.xlsx")
oil_price <- oil_price %>% 
  mutate(Date = as.Date(Month))%>% 
  rename(oil_price=Price)%>% 
  filter(Date >= as.Date("1996-01-01")) %>%
  select(Date, oil_price)

#employement rate
empl_rate <- read.csv('data/employement_rate_monthly.csv')
empl_rate <- empl_rate %>% 
  filter(Correzione == 'dati grezzi', Edizione == '01-Dic-2022', 
         Sesso == 'totale', ETA1=='Y15-64') %>% 
  mutate(date = as.Date(paste(TIME, '-01', sep=''), format="%Y-%m-%d")) %>% 
  select(date, Value) %>%
  rename(empl_rate=Value) %>% 
  arrange(date)

#euro vs dollar rate
eur_dollar <- read.csv('data/euro_usd_rate.csv')
eur_dollar <- eur_dollar %>% mutate(date=as.Date(Date), euro_dollar_rate=Adj.Close) %>%
  select(date, euro_dollar_rate)

# eni stocks
eni <- read.csv('data/eni_stock_data.csv')
eni <- eni %>%
  mutate(date=as.Date(Date), eni_stocks_val = Adj.Close) %>%
  filter(date >= as.Date('1996-01-01')) %>%
  select(date, eni_stocks_val)
plot(eni$eni_stocks_val~eni$date, type='b')

# build the final data set
tmp <- merge(gasoline_month, yearly_CO2emission_km_ratio, by.x='YEAR', by.y='year')
tmp <- tmp %>% mutate(date = as.Date(paste('1/',MONTH, '/', YEAR, sep=''),
                                       format="%d/%m/%Y"))
tmp <- merge(tmp, oil_price, by.x='date', by.y='Date')
tmp <- merge(tmp, empl_rate, by.x='date', by.y='date', all.x=T)
tmp <- merge(tmp, eni, by.x='date', by.y='date', all.x=T)
data <- merge(tmp, eur_dollar, by.x='date', by.y='date', all.x=T)

#colnames(tmp)
data <- data %>% select('date', 'PRICE', 'IVA_TAX', 'ACCISA_TAX', 'NET.PRICE', 'MONTH', 'MONTH_NAME', 'X',
                        'weighted_emission', 'oil_price', 'empl_rate', 'eni_stocks_val',
                        'euro_dollar_rate')
write.csv(data, 'data/merged_data.csv')


