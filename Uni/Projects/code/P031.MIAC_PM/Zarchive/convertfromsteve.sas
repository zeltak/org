libname aod 'Z:\Projects\P031_MIAC_PM\3.Work\2.Gather_data\FN40_steve_clean' ;




PROC EXPORT DATA= aod.Mod3best_03nmnodup
            OUTFILE= "Z:\Projects\P031_MIAC_PM\3.Work\2.Gather_data\FN40_steve_clean\finalprPM03.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;


							 
PROC EXPORT DATA= aod.Mod3best_05nmnodup
            OUTFILE= "Z:\Projects\P031_MIAC_PM\3.Work\2.Gather_data\FN40_steve_clean\finalprPM05.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;



 
PROC EXPORT DATA= aod.Mod3best_04nmnodup
            OUTFILE= "Z:\Projects\P031_MIAC_PM\3.Work\2.Gather_data\FN40_steve_clean\finalprPM04.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
	


 
PROC EXPORT DATA= aod.Mod3best_06nmnodup
            OUTFILE= "Z:\Projects\P031_MIAC_PM\3.Work\2.Gather_data\FN40_steve_clean\finalprPM06.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  


 
PROC EXPORT DATA= aod.Mod3best_07nmnodup
            OUTFILE= "Z:\Projects\P031_MIAC_PM\3.Work\2.Gather_data\FN40_steve_clean\finalprPM07.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  


 
PROC EXPORT DATA= aod.Mod3best_08nmnodup
            OUTFILE= "Z:\Projects\P031_MIAC_PM\3.Work\2.Gather_data\FN40_steve_clean\finalprPM08.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  


 
PROC EXPORT DATA= aod.Mod3best_09nmnodup
            OUTFILE= "Z:\Projects\P031_MIAC_PM\3.Work\2.Gather_data\FN40_steve_clean\finalprPM09.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
		
 
PROC EXPORT DATA= aod.Mod3best_10nmnodup
            OUTFILE= "Z:\Projects\P031_MIAC_PM\3.Work\2.Gather_data\FN40_steve_clean\finalprPM10.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN; 


PROC EXPORT DATA= aod.Mod3best_11nmnodup
            OUTFILE= "Z:\Projects\P031_MIAC_PM\3.Work\2.Gather_data\FN40_steve_clean\finalprPM11.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;


/*PROC EXPORT DATA= aod.Mod3best_03nmsum*/
/*            OUTFILE= "Z:\Projects\P031_MIAC_PM\3.Work\2.Gather_data\FN40_steve_clean\finalprPM03.csv" */
/*			            DBMS=CSV REPLACE;*/
/*						     PUTNAMES=YES;*/
/*							 RUN; */
/**/
/**/
/*PROC EXPORT DATA= aod.Mod3best_05nmsum*/
/*            OUTFILE= "Z:\Projects\P031_MIAC_PM\3.Work\2.Gather_data\FN40_steve_clean\finalprPM05.csv" */
/*			            DBMS=CSV REPLACE;*/
/*						     PUTNAMES=YES;*/
/*							 RUN; */
/**/
/**/
/*PROC EXPORT DATA= aod.Mod3best_11nmsum*/
/*            OUTFILE= "Z:\Projects\P031_MIAC_PM\3.Work\2.Gather_data\FN40_steve_clean\finalprPM11.csv" */
/*			            DBMS=CSV REPLACE;*/
/*						     PUTNAMES=YES;*/
/*							 RUN; */
