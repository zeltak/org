options mprint;
%macro import(year=);

PROC IMPORT OUT= WORK.met&year
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met&year..dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

proc summary nway data=met&year;
class station;
var x y;
output out=OUT&year mean=x y;
run; 

PROC EXPORT DATA= OUT&year
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\XY&year..dbf" 
			            DBMS=DBF REPLACE;
						RUN
						 


%MEND ;

%import(year=2000);
%import(year=2001);
%import(year=2002);
%import(year=2003);
%import(year=2004);
%import(year=2005);
%import(year=2006);
%import(year=2007);
%import(year=2008); 
%import(year=2009); 
%import(year=2010); 
%import(year=2011); 
