# -------------------------
# EXPLORATORY DATA ANALYSIS
# -------------------------

# Set working directory (create a new one and comment this one if needed)
setwd("D:\\OneDrive\\Master\\BDMA\\Courses\\Semester_3\\Time_Series_Analysis\\Project\\gasoline-time-series-analysis")

# Import needed packages
library(ggplot2)

# Explore te datasets

# -------------
# Main Dataset
# -------------

# Gasoline's price in Italy

# Load the data
gasoline_price_italy <- read.csv(".//data//prezzi_mensili_benzina_dal_1996_a_20221028.csv", stringsAsFactors = T)

# Present the structure
str(gasoline_price_italy)

# Show first rows
head(gasoline_price_italy)

# Show last rows
tail(gasoline_price_italy)

# Check for missing values
colSums(is.na(gasoline_price_italy))

# Remarks
# - There are observations from every month from January 1996 to September 2022
# - The column NETTO is PREZZO - IVA - ACCISA, so it is the price of a liter of gasoline without taxes
# - The quantities in EUR are multiplied by 1000
# - The dataset does not contain missing values


# -----------------------
# Complementary Datasets
# -----------------------

# Petrol's price

# Load the data
oil_price <- read_xlsx(".//data//oil_price_monthy.xlsx")
oil_price <- data.frame(oil_price, stringsAsFactors = T)

# Present the structure
str(oil_price)

# Show first rows
head(oil_price)

# Show last rows
tail(oil_price)

# Check for missing values
colSums(is.na(oil_price))

# Remarks
# - The dataset contains monthly observations from 10th January 1992 to 1st November 2022
# - One barrel of oil is aprox. 159 liters
# - The dataset does not contain missing values


# Historic air emissions of cars in Italy

# Load the data
co2_emissions_vehicles <- read_xlsx("./data/DatiCopertTrasportoStrada1990-2020.xlsx", sheet = "CO2_TOTAL")
co2_emissions_vehicles <- data.frame(co2_emissions_vehicles, stringsAsFactors = T)

km_by_vehicles <- read_xlsx("./data/DatiCopertTrasportoStrada1990-2020.xlsx", sheet = "veickm")
km_by_vehicles <- data.frame(km_by_vehicles, stringsAsFactors = T)


# Present the structure
str(co2_emissions_vehicles)
str(km_by_vehicles)

# Show first rows
head(co2_emissions_vehicles)
head(co2_emissions_vehicles)

# Show last rows
tail(co2_emissions_vehicles)
tail(co2_emissions_vehicles)

# Check for missing values
colSums(is.na(co2_emissions_vehicles))
colSums(is.na(co2_emissions_vehicles))

# Remarks

# - The dataset contains yearly observations from 1990 to 2020
# - The tables are presented in a tricky way, this must be solve during the preprocessing


# Employment's rate in Italy

# Load the data
employment_rate <- read.csv("./data/employement_rate_monthly.csv", stringsAsFactors = T)

# Present the structure
str(employment_rate)

# Show first rows
head(employment_rate)

# Show last rows
tail(employment_rate)

# Check for missing values
colSums(is.na(employment_rate))

# Remarks:
# The dataset contains monthly observations from January 2004 to October 2022


# EUR-USD exchange rate

# Load the data
eur_usd <- read.csv("./data/euro_usd_rate.csv", stringsAsFactors = T)

# Present the structure
str(eur_usd)

# Show first rows
head(eur_usd)

# Show last rows
tail(eur_usd)

# Check for missing values
colSums(is.na(eur_usd))

# Remark:
# - The dataset contains daily observations from January 1st 2004 to December 7th 2022
# - The dataset does not contain missing values


# ENI stock price

# Load the data
eni_stock_price <- read.csv("./data/eni_stock_data.csv", stringsAsFactors = T)

# Present the structure
str(eni_stock_price)

# Show first rows
head(eni_stock_price)

# Show last rows
tail(eni_stock_price)

# Check for missing values
colSums(is.na(eni_stock_price))

# Remarks
# - The dataset contains daily observations from January 1st 1996 to January 12th 2022
# - The dataset does not contain missing values
# - Relevant columns for the study:
#   - Date
#   - Close