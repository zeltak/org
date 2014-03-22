GET DATA
  /TYPE=XLS
  /FILE=
    'C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.1.Raw_data\XLS\Original Raw Data Final v4_phytate.xls'
  /SHEET=name 'Raw data less "dup" data'
  /CELLRANGE=full
  /READNAMES=on
  /ASSUMEDSTRWIDTH=32767.
DATASET NAME DataSet1 WINDOW=FRONT.







RECODE CO2inthissample ('aCO2'=0) ('eCO2'=1) INTO CO.
VARIABLE LABELS  CO 'CO'.
EXECUTE.

AUTORECODE VARIABLES=Crop 
  /INTO crop_type
  /PRINT.

RENAME VARIABLES Year.of.study=year

COMPUTE nitro_cont=NitrogenApplicationquantitativekgNha.
EXECUTE.

RECODE NitrogenApplicationquantitativekgNha (0=0) (ELSE=1) INTO nitro.
EXECUTE.


RECODE Waterqualitative ('Dry'=0) ('Wet'=1) ('NA'=SYSMIS) INTO iregg.
EXECUTE.



AUTORECODE VARIABLES=Study 
  /INTO study_area
  /PRINT.



AUTORECODE VARIABLES=Location 
  /INTO loc
  /PRINT.

AUTORECODE VARIABLES=Cultivar 
  /INTO cultiv
  /PRINT.

RECODE cultiv (12=SYSMIS).
EXECUTE.



SAVE TRANSLATE OUTFILE=
    'C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\export to R\all_orig.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.




