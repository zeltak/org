
GET DATA
  /TYPE=TXT
  /FILE=
    "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\"+
    "mod1\T2003.csv"
  /DELCASE=LINE
  /DELIMITERS=","
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  SID A8
  DATE ADATE10
  TMEAN F2.0
  WMEAN F3.0
  TMIN F3.0
  X F9.5
  Y F8.5
  D F2.0
  M F1.0
  C F4.0
  elevation F3.0
  emis_scale F5.3
  windsp_krig F11.8
  ndvi F7.4
  per_built F4.2
  st_faren F6.3.
CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.





DATASET ACTIVATE DataSet1.
RECODE M (1 thru 4=0) (5 thru 10=1) (11 thru 12=0) INTO season.
VARIABLE LABELS  season 'season'.
EXECUTE.

DATASET ACTIVATE DataSet1.
RECODE pop_den (0 thru 1808=0) (1808 thru 11111111111111111111=1) INTO popd_bin.
VARIABLE LABELS  popd_bin 'popd_bin'.
EXECUTE.


DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(uniform(1)<=.10).
VARIABLE LABEL filter_$ 'Approximately 10% of the cases (SAMPLE)'.
FORMAT filter_$ (f1.0).
FILTER  BY filter_$.
EXECUTE.


DATASET COPY  T2003_10_s1.
DATASET ACTIVATE  T2003_10_s1.
FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 1).
EXECUTE.

DATASET ACTIVATE T2003_10_s1.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_10_s1.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.

DATASET ACTIVATE DataSet1.


FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 0).
EXECUTE.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_90_s1.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.




  



GET DATA
  /TYPE=TXT
  /FILE=
    "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\"+
    "mod1\T2003.csv"
  /DELCASE=LINE
  /DELIMITERS=","
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  SID A8
  DATE ADATE10
  TMEAN F2.0
  WMEAN F3.0
  TMIN F3.0
  X F9.5
  Y F8.5
  D F2.0
  M F1.0
  C F4.0
  elevation F3.0
  emis_scale F5.3
  windsp_krig F11.8
  ndvi F7.4
  per_built F4.2
  st_faren F6.3.
CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.





DATASET ACTIVATE DataSet1.
RECODE M (1 thru 4=0) (5 thru 10=1) (11 thru 12=0) INTO season.
VARIABLE LABELS  season 'season'.
EXECUTE.

DATASET ACTIVATE DataSet1.
RECODE pop_den (0 thru 1808=0) (1808 thru 11111111111111111111=1) INTO popd_bin.
VARIABLE LABELS  popd_bin 'popd_bin'.
EXECUTE.


DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(uniform(1)<=.10).
VARIABLE LABEL filter_$ 'Approximately 10% of the cases (SAMPLE)'.
FORMAT filter_$ (f1.0).
FILTER  BY filter_$.
EXECUTE.


DATASET COPY  T2003_10_s2.
DATASET ACTIVATE  T2003_10_s2.
FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 1).
EXECUTE.

DATASET ACTIVATE T2003_10_s2.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_10_s2.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.

DATASET ACTIVATE DataSet1.


FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 0).
EXECUTE.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_90_s2.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.








GET DATA
  /TYPE=TXT
  /FILE=
    "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\"+
    "mod1\T2003.csv"
  /DELCASE=LINE
  /DELIMITERS=","
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  SID A8
  DATE ADATE10
  TMEAN F2.0
  WMEAN F3.0
  TMIN F3.0
  X F9.5
  Y F8.5
  D F2.0
  M F1.0
  C F4.0
  elevation F3.0
  emis_scale F5.3
  windsp_krig F11.8
  ndvi F7.4
  per_built F4.2
  st_faren F6.3.
CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.





DATASET ACTIVATE DataSet1.
RECODE M (1 thru 4=0) (5 thru 10=1) (11 thru 12=0) INTO season.
VARIABLE LABELS  season 'season'.
EXECUTE.

DATASET ACTIVATE DataSet1.
RECODE pop_den (0 thru 1808=0) (1808 thru 11111111111111111111=1) INTO popd_bin.
VARIABLE LABELS  popd_bin 'popd_bin'.
EXECUTE.


DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(uniform(1)<=.10).
VARIABLE LABEL filter_$ 'Approximately 10% of the cases (SAMPLE)'.
FORMAT filter_$ (f1.0).
FILTER  BY filter_$.
EXECUTE.


DATASET COPY  T2003_10_s3.
DATASET ACTIVATE  T2003_10_s3.
FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 1).
EXECUTE.

DATASET ACTIVATE T2003_10_s3.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_10_s3.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.

DATASET ACTIVATE DataSet1.


FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 0).
EXECUTE.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_90_s3.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.








GET DATA
  /TYPE=TXT
  /FILE=
    "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\"+
    "mod1\T2003.csv"
  /DELCASE=LINE
  /DELIMITERS=","
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  SID A8
  DATE ADATE10
  TMEAN F2.0
  WMEAN F3.0
  TMIN F3.0
  X F9.5
  Y F8.5
  D F2.0
  M F1.0
  C F4.0
  elevation F3.0
  emis_scale F5.3
  windsp_krig F11.8
  ndvi F7.4
  per_built F4.2
  st_faren F6.3.
CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.





DATASET ACTIVATE DataSet1.
RECODE M (1 thru 4=0) (5 thru 10=1) (11 thru 12=0) INTO season.
VARIABLE LABELS  season 'season'.
EXECUTE.

DATASET ACTIVATE DataSet1.
RECODE pop_den (0 thru 1808=0) (1808 thru 11111111111111111111=1) INTO popd_bin.
VARIABLE LABELS  popd_bin 'popd_bin'.
EXECUTE.


DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(uniform(1)<=.10).
VARIABLE LABEL filter_$ 'Approximately 10% of the cases (SAMPLE)'.
FORMAT filter_$ (f1.0).
FILTER  BY filter_$.
EXECUTE.


DATASET COPY  T2003_10_s4.
DATASET ACTIVATE  T2003_10_s4.
FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 1).
EXECUTE.

DATASET ACTIVATE T2003_10_s4.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_10_s4.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.

DATASET ACTIVATE DataSet1.


FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 0).
EXECUTE.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_90_s4.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.








GET DATA
  /TYPE=TXT
  /FILE=
    "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\"+
    "mod1\T2003.csv"
  /DELCASE=LINE
  /DELIMITERS=","
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  SID A8
  DATE ADATE10
  TMEAN F2.0
  WMEAN F3.0
  TMIN F3.0
  X F9.5
  Y F8.5
  D F2.0
  M F1.0
  C F4.0
  elevation F3.0
  emis_scale F5.3
  windsp_krig F11.8
  ndvi F7.4
  per_built F4.2
  st_faren F6.3.
CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.





DATASET ACTIVATE DataSet1.
RECODE M (1 thru 4=0) (5 thru 10=1) (11 thru 12=0) INTO season.
VARIABLE LABELS  season 'season'.
EXECUTE.

DATASET ACTIVATE DataSet1.
RECODE pop_den (0 thru 1808=0) (1808 thru 11111111111111111111=1) INTO popd_bin.
VARIABLE LABELS  popd_bin 'popd_bin'.
EXECUTE.


DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(uniform(1)<=.10).
VARIABLE LABEL filter_$ 'Approximately 10% of the cases (SAMPLE)'.
FORMAT filter_$ (f1.0).
FILTER  BY filter_$.
EXECUTE.


DATASET COPY  T2003_10_s5.
DATASET ACTIVATE  T2003_10_s5.
FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 1).
EXECUTE.

DATASET ACTIVATE T2003_10_s5.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_10_s5.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.

DATASET ACTIVATE DataSet1.


FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 0).
EXECUTE.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_90_s5.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.








GET DATA
  /TYPE=TXT
  /FILE=
    "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\"+
    "mod1\T2003.csv"
  /DELCASE=LINE
  /DELIMITERS=","
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  SID A8
  DATE ADATE10
  TMEAN F2.0
  WMEAN F3.0
  TMIN F3.0
  X F9.5
  Y F8.5
  D F2.0
  M F1.0
  C F4.0
  elevation F3.0
  emis_scale F5.3
  windsp_krig F11.8
  ndvi F7.4
  per_built F4.2
  st_faren F6.3.
CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.





DATASET ACTIVATE DataSet1.
RECODE M (1 thru 4=0) (5 thru 10=1) (11 thru 12=0) INTO season.
VARIABLE LABELS  season 'season'.
EXECUTE.

DATASET ACTIVATE DataSet1.
RECODE pop_den (0 thru 1808=0) (1808 thru 11111111111111111111=1) INTO popd_bin.
VARIABLE LABELS  popd_bin 'popd_bin'.
EXECUTE.


DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(uniform(1)<=.10).
VARIABLE LABEL filter_$ 'Approximately 10% of the cases (SAMPLE)'.
FORMAT filter_$ (f1.0).
FILTER  BY filter_$.
EXECUTE.


DATASET COPY  T2003_10_s6.
DATASET ACTIVATE  T2003_10_s6.
FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 1).
EXECUTE.

DATASET ACTIVATE T2003_10_s6.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_10_s6.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.

DATASET ACTIVATE DataSet1.


FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 0).
EXECUTE.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_90_s6.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.








GET DATA
  /TYPE=TXT
  /FILE=
    "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\"+
    "mod1\T2003.csv"
  /DELCASE=LINE
  /DELIMITERS=","
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  SID A8
  DATE ADATE10
  TMEAN F2.0
  WMEAN F3.0
  TMIN F3.0
  X F9.5
  Y F8.5
  D F2.0
  M F1.0
  C F4.0
  elevation F3.0
  emis_scale F5.3
  windsp_krig F11.8
  ndvi F7.4
  per_built F4.2
  st_faren F6.3.
CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.





DATASET ACTIVATE DataSet1.
RECODE M (1 thru 4=0) (5 thru 10=1) (11 thru 12=0) INTO season.
VARIABLE LABELS  season 'season'.
EXECUTE.

DATASET ACTIVATE DataSet1.
RECODE pop_den (0 thru 1808=0) (1808 thru 11111111111111111111=1) INTO popd_bin.
VARIABLE LABELS  popd_bin 'popd_bin'.
EXECUTE.


DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(uniform(1)<=.10).
VARIABLE LABEL filter_$ 'Approximately 10% of the cases (SAMPLE)'.
FORMAT filter_$ (f1.0).
FILTER  BY filter_$.
EXECUTE.


DATASET COPY  T2003_10_s7.
DATASET ACTIVATE  T2003_10_s7.
FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 1).
EXECUTE.

DATASET ACTIVATE T2003_10_s7.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_10_s7.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.

DATASET ACTIVATE DataSet1.


FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 0).
EXECUTE.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_90_s7.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.








GET DATA
  /TYPE=TXT
  /FILE=
    "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\"+
    "mod1\T2003.csv"
  /DELCASE=LINE
  /DELIMITERS=","
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  SID A8
  DATE ADATE10
  TMEAN F2.0
  WMEAN F3.0
  TMIN F3.0
  X F9.5
  Y F8.5
  D F2.0
  M F1.0
  C F4.0
  elevation F3.0
  emis_scale F5.3
  windsp_krig F11.8
  ndvi F7.4
  per_built F4.2
  st_faren F6.3.
CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.





DATASET ACTIVATE DataSet1.
RECODE M (1 thru 4=0) (5 thru 10=1) (11 thru 12=0) INTO season.
VARIABLE LABELS  season 'season'.
EXECUTE.

DATASET ACTIVATE DataSet1.
RECODE pop_den (0 thru 1808=0) (1808 thru 11111111111111111111=1) INTO popd_bin.
VARIABLE LABELS  popd_bin 'popd_bin'.
EXECUTE.


DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(uniform(1)<=.10).
VARIABLE LABEL filter_$ 'Approximately 10% of the cases (SAMPLE)'.
FORMAT filter_$ (f1.0).
FILTER  BY filter_$.
EXECUTE.


DATASET COPY  T2003_10_s8.
DATASET ACTIVATE  T2003_10_s8.
FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 1).
EXECUTE.

DATASET ACTIVATE T2003_10_s8.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_10_s8.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.

DATASET ACTIVATE DataSet1.


FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 0).
EXECUTE.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_90_s8.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.








GET DATA
  /TYPE=TXT
  /FILE=
    "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\"+
    "mod1\T2003.csv"
  /DELCASE=LINE
  /DELIMITERS=","
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  SID A8
  DATE ADATE10
  TMEAN F2.0
  WMEAN F3.0
  TMIN F3.0
  X F9.5
  Y F8.5
  D F2.0
  M F1.0
  C F4.0
  elevation F3.0
  emis_scale F5.3
  windsp_krig F11.8
  ndvi F7.4
  per_built F4.2
  st_faren F6.3.
CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.





DATASET ACTIVATE DataSet1.
RECODE M (1 thru 4=0) (5 thru 10=1) (11 thru 12=0) INTO season.
VARIABLE LABELS  season 'season'.
EXECUTE.

DATASET ACTIVATE DataSet1.
RECODE pop_den (0 thru 1808=0) (1808 thru 11111111111111111111=1) INTO popd_bin.
VARIABLE LABELS  popd_bin 'popd_bin'.
EXECUTE.


DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(uniform(1)<=.10).
VARIABLE LABEL filter_$ 'Approximately 10% of the cases (SAMPLE)'.
FORMAT filter_$ (f1.0).
FILTER  BY filter_$.
EXECUTE.


DATASET COPY  T2003_10_s9.
DATASET ACTIVATE  T2003_10_s9.
FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 1).
EXECUTE.

DATASET ACTIVATE T2003_10_s9.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_10_s9.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.

DATASET ACTIVATE DataSet1.


FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 0).
EXECUTE.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_90_s9.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.








GET DATA
  /TYPE=TXT
  /FILE=
    "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\"+
    "mod1\T2003.csv"
  /DELCASE=LINE
  /DELIMITERS=","
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  SID A8
  DATE ADATE10
  TMEAN F2.0
  WMEAN F3.0
  TMIN F3.0
  X F9.5
  Y F8.5
  D F2.0
  M F1.0
  C F4.0
  elevation F3.0
  emis_scale F5.3
  windsp_krig F11.8
  ndvi F7.4
  per_built F4.2
  st_faren F6.3.
CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.





DATASET ACTIVATE DataSet1.
RECODE M (1 thru 4=0) (5 thru 10=1) (11 thru 12=0) INTO season.
VARIABLE LABELS  season 'season'.
EXECUTE.

DATASET ACTIVATE DataSet1.
RECODE pop_den (0 thru 1808=0) (1808 thru 11111111111111111111=1) INTO popd_bin.
VARIABLE LABELS  popd_bin 'popd_bin'.
EXECUTE.


DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(uniform(1)<=.10).
VARIABLE LABEL filter_$ 'Approximately 10% of the cases (SAMPLE)'.
FORMAT filter_$ (f1.0).
FILTER  BY filter_$.
EXECUTE.


DATASET COPY  T2003_10_s10.
DATASET ACTIVATE  T2003_10_s10.
FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 1).
EXECUTE.

DATASET ACTIVATE T2003_10_s10.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_10_s10.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.

DATASET ACTIVATE DataSet1.


FILTER OFF.
USE ALL.
SELECT IF (filter_$ = 0).
EXECUTE.

SAVE TRANSLATE OUTFILE='c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.3.TEMP_MODELS\3.1.1.4.Work\3.Analysis\mod1-CV\T2003_90_s10.csv'
  /TYPE=CSV
  /MAP
  /REPLACE
  /FIELDNAMES
  /CELLS=VALUES.





  
