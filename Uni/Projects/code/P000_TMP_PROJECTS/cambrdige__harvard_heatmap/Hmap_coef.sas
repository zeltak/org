libname mods 'Z:\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;
libname hm 'Z:\Projects\P000_TMP_PROJECTS\cambridge_harvard_heatmap\' ;
	


options mprint;
%macro import(year=);
	
proc mixed data = mods.mod1_&year  method=reml;
class date ;
   model tempc = Ntckin elev purban NDVI / s outpred=hm.pdataA_&year;
    random int Ntckin/ sub = date s ;
	 ods output  SolutionF =  hm.SolutionF&year;
    ods output  SolutionR =  hm.SolutionR&year;
	run;

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

