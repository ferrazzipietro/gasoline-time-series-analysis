options(warn=0)
suppressMessages(library(dplyr))
gasoline_month <- read.csv("data/prezzi_mensili_benzina_dal_1996_a_20221028.csv")
gasoline_month <- gasoline_month %>% mutate(DATA = as.Date(paste(ANNO, CODICE_MESE, "01", 
                                                                 sep="-", format = "%d-%m-%y")))

gasoline_month_eng <- gasoline_month
colnames(gasoline_month_eng) <- c("YEAR", "MONTH", "MONTH_NAME", "PRDUCT",
                                  "PRODUCT_NAME", "PRICE", "IVA_TAX",
                                  "ACCISA_TAX", "NET PRICE", "DATE")
write.csv(gasoline_month_eng, "data/monthly_gasoline_prices_1996_2022.csv")
