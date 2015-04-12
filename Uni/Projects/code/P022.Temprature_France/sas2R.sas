libname aod 'Z:\Projects\P022_Temprature_France\3.work\stage1' ;
libname aod3 'Z:\Projects\P022_Temprature_France\3.work\cn035_stage3-final' ;


options mprint;
%macro import(year=);

PROC EXPORT DATA= aod.Ep_final_s1_&year
            OUTFILE= "Z:\Projects\P022_Temprature_France\3.work\cn099.R\mod1.&year..csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							 	

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
	
	

options mprint;
%macro import(year=);

PROC EXPORT DATA= aod3.Clean_final&year
            OUTFILE= "Z:\Projects\P022_Temprature_France\3.work\cn099.R\mod3.&year..csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							 	

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
	
	
