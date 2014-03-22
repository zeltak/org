libname zn 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\' ;



 

PROC IMPORT OUT= WORK.zinc
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\export to SAS\zinc.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



/*C3: weighted average for barley, rice, and wheat >>>>>>> cgroup3*/
/*C4: weighted average for corn and sorghum cgroup1*/
/*Legumes: weighted average for soy and peas. cgroup2*/

/**/
/*1:corn*/
/*2:peas*/
/*3:rice*/
/*4:sorghum*/
/*5:soy*/
/*6:wheat*/




data zinc;
set zinc;
if Waterquali="Dry" then water=0;
if Waterquali="Wet" then water=1;
if NitrogenAp="Low" then nitrolevel=0;
if NitrogenAp="Medium" then nitrolevel=1;
if NitrogenAp="High" then nitrolevel=2;
if SowingTimi="TOS 1" then stime=1;
if SowingTimi="TOS 2" then stime=2 ;
else if SowingTimi ne "TOS 2" or SowingTimi ne "TOS 1"  then stime=.;
lzinc= log(znppm);
liron= log(feppm);
lphy= log(PhMeanmgg);
if paircount=389 then delete;
if paircount=. then delete;
if crop_type=1 or crop_type=4 then cgroup=1;
if crop_type=2 or crop_type=5 then cgroup=2;
if crop_type=3 or crop_type=6 then cgroup=3;
if crop_type=1 then c1dumm=0;
else if crop_type=2 then c1dumm=1;
else if crop_type=3 then c1dumm=2;
else if crop_type=4 then c1dumm=3;
else if crop_type=5 then c1dumm=4;
else if crop_type=6 then c1dumm=5;
run; 


proc freq data=zinc;
table crop_type/ list;
run; 

proc means data=zinc n  nmiss;
run;	


proc sort data = zinc; by crop co   ;run; 


proc means data=zinc n min max mean std nmiss;
var Znppm Feppm PhMeanmgg Mnppm Cuppm Sppm water nitrolevel ;
by crop co;
output out=zn.raw_disc ;
run; 


data zinc_hors;
set zinc;
where Location ="Horsham, Australia";
run; 

data zinc_hors_2010;
set zinc_hors;
where year=2010;
run; 


data zinc_hors_2009;
set zinc_hors;
where year=2009;
run; 


data zinc_hors_2008;
set zinc_hors;
where year=2008;
run; 


proc sort data = zinc_hors_2010; by paircount   ;run; 


data zinc_hors_2008_raw;
set zinc_hors_2008;
keep  year paircount PhMeanmgg CO;
run; 

PROC EXPORT DATA= zinc_hors_2008_raw
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\wheat\horshram_08_10_raw\hs08_raw.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN;


data zinc_hors_2009_raw;
set zinc_hors_2009;
keep  year paircount PhMeanmgg CO;
run; 

PROC EXPORT DATA= zinc_hors_2009_raw
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\wheat\horshram_08_10_raw\hs09_raw.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN;


data zinc_hors_2010_raw;
set zinc_hors_2010;
keep  year paircount PhMeanmgg CO;
run; 

PROC EXPORT DATA= zinc_hors_2010_raw
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\wheat\horshram_08_10_raw\hs10_raw.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN;





proc mixed data=zinc_hors_2008 method=reml;
class crop paircount cultivar ;
    model lphy =  CO  water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=physol_hors_08;
run;


proc mixed data=zinc_hors_2009 method=reml;
class crop paircount cultivar ;
    model lphy =  CO  water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=physol_hors_09;
run;


proc mixed data=zinc_hors_2010 method=reml;
class crop paircount cultivar ;
    model lphy =  CO  water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=physol_hors_10;
run;



proc mixed data=zinc_hors method=reml;
class crop paircount cultivar ;
    model lphy =  CO  water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=physol_hors_08_10;
run;


proc means data=zinc_hors_2008 n min max mean std nmiss;
var PhMeanmgg; 
run; 

proc means data=zinc_hors_2009 n min max mean std nmiss;
var PhMeanmgg; 
run; 

proc means data=zinc_hors_2010 n min max mean std nmiss;
var PhMeanmgg; 
run; 

proc means data=zinc_hors n min max mean std nmiss;
var PhMeanmgg; 
run; 



PROC EXPORT DATA= Physol_hors_08
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\wheat\horshram_08_10_raw\hs08.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN; 


PROC EXPORT DATA= Physol_hors_09
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\wheat\horshram_08_10_raw\hs09.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN; 


						
PROC EXPORT DATA= Physol_hors_10
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\wheat\horshram_08_10_raw\hs10.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN; 

						
PROC EXPORT DATA= Physol_hors_08_10
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\wheat\horshram_08_10_raw\hs08_10.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN; 
