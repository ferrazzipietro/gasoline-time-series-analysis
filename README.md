# gasoline-time-series-analysis
Analysis of the price of gasoline in Italy

## Translate variables' name to english
$ Rscript from_italian_to_english.R

## Merge the data
Data comes from 3 different data sources, contained in three files: ``prezzi_mensili_benzina_dal_1996_a_20221028.csv`` ([MISE]), ``oil_price_monthy.xlsx`` ([IndexMundi]) and ``DatiCopertTrasportoStrada1990-2020.xlsx`` (ISPRA), ``employement_rate.csv`` (ISTAT). 

Interesting information is extracted. To obtain the merged dataset run:
``$ Rscript prepare_data.R ``


[//]: # (HyperLinks)

[MISE]: https://dgsaie.mise.gov.it/prezzi_carburanti_mensili.php?lang=en_US
[IndexMundi]: https://www.indexmundi.com/commodities/?commodity=crude-oil&months=360
[ISPRA]: http://emissioni.sina.isprambiente.it/serie-storiche-emissioni/
[ISTAT]: http://dati.istat.it/Index.aspx?DataSetCode=DCCV_TAXOCCU1#
