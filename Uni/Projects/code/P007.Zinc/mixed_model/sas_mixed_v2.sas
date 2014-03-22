
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
l_N= log(N);
l_P= log(P);
l_K= log(K);
l_S= log(Sppm);
l_B= log(Bppm);
l_Ca= log(Ca);
l_Mg= log(Mg);
l_Mn= log(Mnppm);
l_Cu= log(Cuppm);
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

PROC EXPORT DATA= zinc 
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\3.Analysis\R\raw." 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  


proc means data=zinc n  nmiss;
run;	


proc sort data = zinc; by crop co   ;run; 


proc means data=zinc n min max mean std nmiss;
var Znppm Feppm PhMeanmgg Mnppm Cuppm Sppm water nitrolevel ;
by crop co;
output out=zn.raw_disc ;
run; 



/*stratified zinc by crop*/

proc sort data = zinc ; by crop_type   ;run; 

proc mixed data=zinc method=reml;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s ;
     random int  / sub = paircount s;
		by crop_type;
		ods output solutionf=zn.zincsol;
run;

/*stratified zinc by group*/

proc sort data = zinc ; by cgroup   ;run; 

proc mixed data=zinc method=reml;
class crop paircount cultivar c1dumm ;
    model lzinc =  CO  water nitrolevel c1dumm/ s ;
     random int  / sub = paircount s;
		by cgroup;
		ods output solutionf=zn.zincsol_group;
run;



/*stratified iron by crop*/

proc sort data = zinc ; by crop_type   ;run; 

proc mixed data=zinc method=reml;
class crop paircount cultivar ;
    model liron =  CO  water nitrolevel/ s ;
     random int  / sub = paircount s;
		by crop_type;
		ods output solutionf=zn.ironsol;
run;


/*stratified iron by group*/

proc sort data = zinc ; by cgroup   ;run; 

proc mixed data=zinc method=reml;
class crop paircount cultivar c1dumm ;
    model liron =  CO  water nitrolevel c1dumm/ s ;
     random int  / sub = paircount s;
		by cgroup;
		ods output solutionf=zn.ironsol_group;
run;


/*stratified phytate by crop*/

proc sort data = zinc ; by crop_type   ;run; 

proc mixed data=zinc method=reml;
class crop paircount cultivar ;
    model lphy =  CO  water nitrolevel/ s ;
     random int  / sub = paircount s;
		by crop_type;
		ods output solutionf=zn.physol;
run;


/*stratified phytate by group*/

proc sort data = zinc ; by cgroup   ;run; 

proc mixed data=zinc method=reml;
class crop paircount cultivar c1dumm ;
    model lphy =  CO  water nitrolevel c1dumm/ s ;
     random int  / sub = paircount s;
		by cgroup;
		ods output solutionf=zn.physol_group;
run;



/*stratified N by crop*/

proc sort data = zinc ; by crop_type   ;run; 

proc mixed data=zinc method=reml;
class crop paircount cultivar ;
    model l_N =  CO  water nitrolevel/ s ;
     random int  / sub = paircount s;
		by crop_type;
		ods output solutionf=zn.zincsol;
run;

/*stratified N by group*/

proc sort data = zinc ; by cgroup   ;run; 

proc mixed data=zinc method=reml;
class crop paircount cultivar c1dumm ;
    model l_N =  CO  water nitrolevel c1dumm/ s ;
     random int  / sub = paircount s;
		by cgroup;
		ods output solutionf=zn.zincsol_group;
run;









































/*Australia data seperatly*/


data zinc_walp;
set zinc;
where Location ="Walpeup, Australia";
run; 

data zinc_hors;
set zinc;
where Location ="Horsham, Australia";
run; 

data zinc_aust;
set zinc_hors zinc_walp ;
run; 



/*Zinc breakdwon*/

proc mixed data=zinc_hors method=reml;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=zincsol_hors;
run;


proc mixed data=zinc_walp method=reml;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=zincsol_walp;
run;


proc mixed data=zinc_aust method=reml;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=zincsol_aust;
run;


/*iron breakdwon*/

proc mixed data=zinc_hors method=reml;
class crop paircount cultivar ;
    model liron =  CO  water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=ironsol_hors;
run;

proc mixed data=zinc_walp method=reml;
class crop paircount cultivar ;
    model liron =  CO  water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=ironsol_walp;
run;

proc mixed data=zinc_aust method=reml;
class crop paircount cultivar ;
    model liron =  CO  water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=ironsol_aust;
run;

/*phy breakdwon*/


proc mixed data=zinc_hors method=reml;
class crop paircount cultivar ;
    model lphy =  CO  water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=physol_hors;
run;

proc mixed data=zinc_walp method=reml;
class crop paircount cultivar ;
    model lphy =  CO  water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=physol_walp;
run;

proc mixed data=zinc_aust method=reml;
class crop paircount cultivar ;
    model lphy =  CO  water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=physol_aust;
run;


data Table_wheat;
set  zincsol_hors zincsol_walp zincsol_aust ironsol_hors ironsol_walp ironsol_aust physol_hors physol_walp physol_aust;
run; 

PROC EXPORT DATA= Table_wheat
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\wheat\wheat_table.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN; 



/*effect modification*/

proc mixed data=zinc_aust method=reml;
class crop paircount cultivar ;
    model lzinc =  CO  cultivar water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=zincsol_aust_emod1;
run;




proc mixed data=zinc_aust method=reml;
class crop paircount cultivar ;
    model lzinc =  CO CO*cultivar cultivar water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=zincsol_aust_emod2;
run;



proc mixed data=zinc_aust method=reml;
class crop paircount  ;
    model lzinc =  CO CO*water  water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=zincsol_aust_emod3;
run;





proc mixed data=zinc_aust method=reml;
class crop paircount  ;
    model lzinc =  CO CO*nitrolevel water nitrolevel/ s cl;
     random int  / sub = paircount s;
		ods output solutionf=zincsol_aust_emod4;
run;


data effmod;
set zincsol_aust_emod1 zincsol_aust_emod2 zincsol_aust_emod3 zincsol_aust_emod4;
run; 


PROC EXPORT DATA= effmod
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\wheat\effmod.xls" 
			            DBMS=EXCEL5 REPLACE;
						RUN; 





/*FULL DATASET (SUMMARY)#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

libname zn 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\' ;



PROC IMPORT OUT= WORK.zincsum
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\export to SAS\zincsum.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



/*FACE subset*/
/**/
/**/
/*data zincsum;*/
/*set zincsum;*/
/*where Method="FACE";*/
/*run; */




/*C3: weighted average for barley, rice, and wheat potato>>>>>>> cgroup3*/
/*C4: weighted average for corn and sorghum cgroup1*/
/*Legumes: weighted average for soy and peas. cgroup2*/



/*1:baley*/
/*2:corn*/
/*3:peas*/
/*4:potato*/
/*5:rice*/
/*6:sorghum*/
/*7:soy*/
/*8:wheat*/



data zincsum;
set zincsum;
if Waterquali="Dry" then water=0;
if Waterquali="Wet" then water=1;
if Waterquali="Extra wet" then water=1;
if NitrogenAp="Low" then nitrolevel=0;
if NitrogenAp="Medium" then nitrolevel=1;
if NitrogenAp="High" then nitrolevel=2;
if SowingTimi="TOS 1" then stime=1;
if SowingTimi="TOS 2" then stime=2 ;
else if SowingTimi ne "TOS 2" or SowingTimi ne "TOS 1"  then stime=.;
lzinc= log(zinc_ppm);
liron= log(iron_ppm);
if lzinc=. then delete;
if liron=. then delete;
wgt= Znreplica10;
if crop_type=2 or crop_type=6 then cgroup=1;
if crop_type=7 or crop_type=3 then cgroup=2;
if crop_type=1 or crop_type=4 or crop_type=5 or crop_type=8 then cgroup=3;
if crop_type=1 then c1dumm=0;
else if crop_type=2 then c1dumm=1;
else if crop_type=3 then c1dumm=2;
else if crop_type=4 then c1dumm=3;
else if crop_type=5 then c1dumm=4;
else if crop_type=6 then c1dumm=5;
else if crop_type=7 then c1dumm=6;
else if crop_type=8 then c1dumm=7;
run; 





proc means data=zincsum n  nmiss;
run;	


proc sort data = zincsum; by crop co   ;run; 


proc means data=zincsum n min max mean std nmiss;
var zinc_ppm iron_ppm  water nitrolevel ;
by crop co;
output out=zn.summury_disc ;
run; 



/*stratified zinc by crop*/

proc sort data = zincsum ; by crop_type   ;run; 

proc mixed data=zincsum method=reml;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s ;
     random int  / sub = paircount s;
		by crop_type;
		ods output solutionf=zn.zincsumsol;

run;


proc sort data = zincsum ; by cgroup   ;run; 

proc mixed data=zincsum method=reml;
class crop paircount cultivar c1dumm ;
    model lzinc =  CO  water nitrolevel c1dumm/ s ;
     random int  / sub = paircount s;
		by cgroup;
		ods output solutionf=zn.zincsum_sol_group;
run;





/*stratified iron by crop*/

proc sort data = zincsum ; by crop_type   ;run; 

proc mixed data=zincsum method=reml;
class crop paircount cultivar ;
    model liron =  CO  water nitrolevel/ s ;
     random int  / sub = paircount s;
		by crop_type;
		ods output solutionf=zn.ironsumsol;
run;


/*stratified iron by group*/

proc sort data = zincsum ; by cgroup   ;run; 

proc mixed data=zincsum method=reml;
class crop paircount cultivar c1dumm ;
    model liron =  CO  water nitrolevel c1dumm/ s ;
     random int  / sub = paircount s;
		by cgroup;
		ods output solutionf=zn.ironsum_sol_group;
run;
