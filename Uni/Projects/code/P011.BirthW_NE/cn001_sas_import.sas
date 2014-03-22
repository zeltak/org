libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.11.BirthW_NE\3.1.11.1.Raw_data\Clean_00-08_MDPH_BW_Data\' ;


data work.newbirths;
set aod.birth00_03_3_2_12 aod.birth04_09_3_2_12;
if byob=2009 then delete;
run; 





/*Fix wrong Lat-long in 2003*/

data work.newbirths2003;
set work.newbirths;
where byob=2003;
run; 

data newbirths2003;
set newbirths2003;
lat1=long;
long1=lat;
drop lat long;
run; 

data newbirths2003;
set newbirths2003;
lat=lat1;
long=long1;
drop lat1 long1;
run; 




PROC EXPORT DATA= work.newbirths
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.11.BirthW_NE\3.1.11.4.Work\2.Gather_data\FN001_BW_meta_Final\births_guid_meta0008.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 
 

PROC EXPORT DATA= work.newbirths2003
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.11.BirthW_NE\3.1.11.4.Work\2.Gather_data\FN001_BW_meta_Final\births_guid_meta2003.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
