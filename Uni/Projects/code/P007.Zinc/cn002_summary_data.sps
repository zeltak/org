
GET DATA
  /TYPE=XLS
  /FILE=
    'C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.1.Raw_data\XLS\All Data Summary.xls'
  /SHEET=name 'Data'
  /CELLRANGE=full
  /READNAMES=on
  /ASSUMEDSTRWIDTH=32767.
DATASET NAME DataSet1 WINDOW=FRONT.


* delete uneeded cases (non 0,1 co indicators)


FILTER OFF.
USE ALL.
SELECT IF (CO2indicator    <=   1).
EXECUTE.


DATASET ACTIVATE DataSet1.
RECODE Crop ('Barley'=1) ('Corn '=2) ('Field peas'=3) ('Potato'=4) ('Rice'=5) ('Sorghum '=6) 
    ('soybean'=7) ('Soybean'=7) ('Wheat'=8) ('wheat'=8) INTO crop_type.
EXECUTE.




 * create ambient/elevated binary variable

RECODE CO2inthissamplequal ('aCO2'=0) ('eCO2'=1) INTO CO.
VARIABLE LABELS  CO 'CO'.
EXECUTE.

*transform all relevant covariates


RENAME VARIABLES Year.of.study=year

RECODE Waterqualitative ('Dry'=0) ('Wet'=1) ('NA'=SYSMIS) INTO iregg.
EXECUTE.

AUTORECODE VARIABLES=Cultivar 
  /INTO cultiv
  /PRINT.

RECODE cultiv (12=SYSMIS).
EXECUTE.

COMPUTE nitro=NitrogenApplicationkgNha.
EXECUTE.

COMPUTE date=year.
EXECUTE.



recode PairCount (convert) into pairnumber.
execute.


*create zinv ppm variable


recode Zn.appm (convert) into Zn.appm2.
execute.

recode Zn.eppm (convert) into Zn.eppm2.
execute.


DATASET ACTIVATE DataSet1.
COMPUTE zinc_ppm=Zn.appm2.
EXECUTE.

IF  (MISSING(zinc_ppm) =1) zinc_ppm=Zn.eppm2.
EXECUTE.


*create iron ppm variable

recode Fe.appm (convert) into Fe.appm2.
execute.

recode Fe.eppm (convert) into Fe.eppm2.
execute.


DATASET ACTIVATE DataSet1.
COMPUTE iron_ppm=Fe.appm2.
EXECUTE.

IF  (MISSING(iron_ppm) =1) iron_ppm=Fe.eppm2.
EXECUTE.



DELETE VARIABLES Fe.eppm2 Fe.appm2 Zn.appm2 Zn.eppm2


* deal with replicate variable
**manually delete unpaired data at the end so its missing


recode Znreplicates (convert) into Znreplicates2.
execute.
SHIFT VALUES VARIABLE=Znreplicates2 RESULT=Znreplicates_lag_1 LEAD=1.
IF  (MISSING(Znreplicates_lag_1) =1) Znreplicates_lag_1=Znreplicates2.
EXECUTE.





SAVE TRANSLATE OUTFILE=
    'C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\expo'+
    'rt to R\all_summary.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.


