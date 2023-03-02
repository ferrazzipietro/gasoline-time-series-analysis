# gasoline-time-series-analysis
Gasoline’s seasonal price prediction in Italy
3 months in advance prediction of the price of gasoline for final consumer (EUR) based on monthly data.

# DATASET

We create our own dataset combining multiples sources:
- Monthly employment rate in Italy (2004-today) (ISTAT)
- Yearly vehicle gas emissions (1990-today) (ISPRA)
- Monthly oil’s price in Italy (1996-today) (IndexMundi)
- Daily EUR-USD exchange rate (2004-today) (YAHOO Finance)
- Daily ENI stock price (1996-today) (YAHOO Finance)

The evaluation of the models identify only some of them to be relevant. What follows take only them into account.

## Merge the data
Data comes from 3 different data sources, contained in three files: ``prezzi_mensili_benzina_dal_1996_a_20221028.csv`` ([MISE]), ``oil_price_monthy.xlsx`` ([IndexMundi]) and ``DatiCopertTrasportoStrada1990-2020.xlsx`` ([ISPRA]), ``employement_rate.csv`` ([ISTAT]). 

Interesting information is extracted. To obtain the merged dataset run:
``$ Rscript prepare_data.R infer_NAs_bool``

``infer_NAs_bool = true`` if you want to generate also the dataset without NAs. They are infered using a polynomial model.


## Generate plots
To save the plots in the ``/plots``directory run

``$ Rscript plots.R``

[//]: # (HyperLinks)

[MISE]: https://dgsaie.mise.gov.it/prezzi_carburanti_mensili.php?lang=en_US
[IndexMundi]: https://www.indexmundi.com/commodities/?commodity=crude-oil&months=360
[ISPRA]: http://emissioni.sina.isprambiente.it/serie-storiche-emissioni/
[ISTAT]: http://dati.istat.it/Index.aspx?DataSetCode=DCCV_TAXOCCU1#



