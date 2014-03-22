data T1;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2003\TileName-00v00-part1.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

data T2;
 infile 'f:\Uni\Projects\P031_MIAC_MEXICO\1_Raw_data\modis\Per year\T2003\TileName-00v00-part1.txt' dlm='09'X dsd truncover FIRSTOBS=2;
  input Lat	Long	AOD	Q_C	Reference;
run;

proc summary nway data=T1;
class latitude longitude;
var day;
output out=OUTT1 mean=day;
run; 



PROC EXPORT DATA= OUTT1 
            OUTFILE= "c:\Users\ekloog\Documents\tmp\OUTDATAOUTT1OUTT1.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 



data Tall;
set t1 t2 t3 t4;
run; 

/*so GIS dosent read numeic vars as string*/
proc sort data = Tall; by descending day   ;run;  





PROC EXPORT DATA=  Tall 
            OUTFILE= "c:\Users\ekloog\Documents\tmp\itday1.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
  

