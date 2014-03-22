

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
/*effect modification*/

data zinc_wheat;
set zinc;
where crop_type=6 ;
run; 

proc freq data=zinc_wheat;
table cultivar*co / list;
run; 

proc freq data=zinc_wheat;
table stime*co / list;
run; 


proc freq data=zinc_wheat;
table water*co / list;
run; 


proc freq data=zinc_wheat;
table nitro*co / list;
run; 



proc mixed data=zinc_wheat method=reml;
class crop paircount cultivar ;
    model lzinc =  CO CO*cultivar cultivar water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=zinc_wheat_cultiv;
run;


PROC EXPORT DATA= zinc_wheat_cultiv
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\wheat\zinc_wheat_cultiv.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN; 



proc mixed data=zinc_wheat method=reml;
class crop paircount cultivar ;
    model lzinc =  CO CO*stime stime  water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=zinc_wheat_stime;
run;

PROC EXPORT DATA= zinc_wheat_stime
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\wheat\zinc_wheat_stime.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN; 



proc mixed data=zinc_wheat method=reml;
class crop paircount cultivar ;
    model lzinc =  CO   water nitrolevel/ s cl;
     random int  / sub = paircount s;
	 where water=0;
		ods output solutionf=zinc_wheat_water0;
run;


PROC EXPORT DATA= zinc_wheat_water0
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\wheat\zinc_wheat_water0.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN; 


proc mixed data=zinc_wheat method=reml;
class crop paircount cultivar ;
    model lzinc =  CO   water nitrolevel/ s cl;
     random int  / sub = paircount s;
	 where water=1;
		ods output solutionf=zinc_wheat_water1;
run;

PROC EXPORT DATA= zinc_wheat_water1
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\wheat\zinc_wheat_water1.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN; 
