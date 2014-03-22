
libname mods 'P:\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;

/*glong= round(xx,0.00001);*/
/*glat= round(yy,0.00001);*/

data fin2011;
set mods.Fintmpc_2011;
if glong < -71.5 then delete;
if glong > -70.5 then delete;
if glat > 43 then delete;
if glat < 42.00 then delete;
run; 

PROC EXPORT DATA= fin2003 
            OUTFILE= "D:\Users\zeltak\Documents\tmp\fin2003.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


PROC EXPORT DATA= fin2011 
            OUTFILE= "P:\P020_Temprature_NE_MIA\4.Results\paper\boston_p2011.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  





PROC EXPORT DATA= m3_cor_resxy
            OUTFILE= "P:\P020_Temprature_NE_MIA\4.Results\paper\mod3pred_p2011.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  
