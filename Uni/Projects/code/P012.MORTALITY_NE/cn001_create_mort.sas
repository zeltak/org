

PROC IMPORT OUT= WORK.m00
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.1.Raw_data\1.mortality\M00.DBF" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.m01
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.1.Raw_data\1.mortality\M01.DBF" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.m02
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.1.Raw_data\1.mortality\M02.DBF" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.m03
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.1.Raw_data\1.mortality\M03.DBF" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


PROC IMPORT OUT= WORK.m04
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.1.Raw_data\1.mortality\M04.DBF" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.m05
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.1.Raw_data\1.mortality\M05.DBF" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.m06
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.1.Raw_data\1.mortality\M06.DBF" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.m07
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.1.Raw_data\1.mortality\M07.DBF" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= WORK.m08
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.1.Raw_data\1.mortality\M08.DBF" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
/*PROC IMPORT OUT= m09*/
/*  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.1.Raw_data\1.mortality\M09.csv" */
/*    DBMS=CSV REPLACE;*/
/*	  GETNAMES=YES;*/
/*	    DATAROW=2; */
/*		RUN;*/
/*		 */

data allmort;
set m00 m01 m02 m03 m04 m05 m06 m07 m08 ;
/*where PLACDTH = 5 or  PLACDTH= 6 ;*/
run; 


PROC EXPORT DATA= allmort
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.12.MORTALITY_NE\3.1.10.4.Work\2.Gather_data\FN00_Cases\mort0008.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 
