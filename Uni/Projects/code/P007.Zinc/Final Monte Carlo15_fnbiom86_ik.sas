ods listing close;*to suppress the output printing;

proc printto log="f:\Uni\Projects\3.1.7.Zinc\3.1.7.5.Results\SAS\filename.log"; run;


libname zn 'f:\Uni\Projects\3.1.7.Zinc\3.1.7.5.Results\SAS\' ;




PROC IMPORT OUT= WORK.zinc
            DATAFILE= "f:\Uni\Projects\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\export to SAS\zinc.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



/*C3a: weighted average for barley, rice, and wheat >>>>>>> cgroup3*/
/*C3b: weighted average for barley and wheat >>>>>>> cgroup3*/
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
if Waterquali="Dry"    then water=0;
if Waterquali="Wet"    then water=1;
if NitrogenAp="Low"    then nitrolevel=0;
if NitrogenAp="Medium" then nitrolevel=1;
if NitrogenAp="High"   then nitrolevel=2;
if SowingTimi="TOS 1"  then stime=1;
if SowingTimi="TOS 2"  then stime=2 ;
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

if crop_type=1 or crop_type=4 then cgroup_1 = 1; else cgroup_1 = 0;
if crop_type=2 or crop_type=5 then cgroup_2 = 2; else cgroup_2 = 0;
if crop_type=3 or crop_type=6 then cgroup_3 = 3; else cgroup_3 = 0;
if crop_type=6                then cgroup_4 = 4; else cgroup_4 = 0;

/*if crop_type=1 then c1dumm=0;*/
/*else if crop_type=2 then c1dumm=1;*/
/*else if crop_type=3 then c1dumm=2;*/
/*else if crop_type=4 then c1dumm=3;*/
/*else if crop_type=5 then c1dumm=4;*/
/*else if crop_type=6 then c1dumm=5;*/
run; 


/********************************************/
/*** Results for Simulation Zinc. by CROP ***/
/********************************************/

proc sort data = zinc ; by crop_type   ;run; 



ods select none;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;

proc mixed data=zinc method=reml covtest;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		by crop_type;
run; quit;
ods select all;





proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = crop_type Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = crop_type Slope Sl_SE Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
   Sl_SE = Stderr;
run; 

data zn.NObs;
 set zn.NObs;
  keep crop_type N;
   if Label = "Number of Observations Read";
run;

data zn.CovParms(keep = crop_type CovParm Cov);
 set zn.CovParms;
  Cov = Estimate;
run;

proc sort data = zn.CovParms; by CovParm; run;


data zn.Rand_Int(keep = crop_type Rand_Int); set zn.CovParms;
  where CovParm = "Intercept";
   Rand_Int = Cov;
run; 

data zn.Residual(keep = crop_type Residual); set zn.CovParms;
  where CovParm = "Residual";
   Residual = Cov;
run; 

data zn.param_simu_crop_zinc;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
  by crop_type;
    Outcome = "Zinc";
run;

/******************************************/
/*** Results for Simulation Phy by CROP ***/
/******************************************/

proc sort data = zinc ; by crop_type   ;run; 



proc mixed data=Zinc method=reml covtest;
class crop paircount cultivar ;
    model lphy =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		by crop_type;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;
run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = crop_type Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = crop_type Slope Sl_SE Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
   Sl_SE = Stderr;
run; 

data zn.NObs;
 set zn.NObs;
  keep crop_type N;
   if Label = "Number of Observations Read";
run;

data zn.CovParms(keep = crop_type CovParm Cov);
 set zn.CovParms;
  Cov = Estimate;
run;

proc sort data = zn.CovParms; by CovParm; run;


data zn.Rand_Int(keep = crop_type Rand_Int); set zn.CovParms;
  where CovParm = "Intercept";
   Rand_Int = Cov;
run; 

data zn.Residual(keep = crop_type Residual); set zn.CovParms;
  where CovParm = "Residual";
   Residual = Cov;
run; 

data zn.param_simu_crop_PHY;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
  by crop_type;
    Outcome = "Phy";
run;




/**********************************************/
/*** Results for Simulation Zinc. by CGROUP ***/
/**********************************************/


proc mixed data=zinc method=reml covtest;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;

		where cgroup_1 = 1;
run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = Slope Sl_SE Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
   Sl_SE = Stderr;
run; 

data zn.NObs;
 set zn.NObs;
  keep N;
   if Label = "Number of Observations Read";
run;

data zn.CovParms(keep = CovParm Cov);
 set zn.CovParms;
  Cov = Estimate;
run;

proc sort data = zn.CovParms; by CovParm; run;


data zn.Rand_Int(keep = Rand_Int); set zn.CovParms;
  where CovParm = "Intercept";
   Rand_Int = Cov;
run; 

data zn.Residual(keep = Residual); set zn.CovParms;
  where CovParm = "Residual";
   Residual = Cov;
run; 

data zn.param_simu_group1_zinc;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
  Outcome = "Zinc";
  Group = 1;
run;


proc mixed data=zinc method=reml covtest;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;

		where cgroup_2 = 2;
run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = Slope Sl_SE Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
   Sl_SE = Stderr;
run; 

data zn.NObs;
 set zn.NObs;
  keep N;
   if Label = "Number of Observations Read";
run;

data zn.CovParms(keep = CovParm Cov);
 set zn.CovParms;
  Cov = Estimate;
run;

proc sort data = zn.CovParms; by CovParm; run;


data zn.Rand_Int(keep = Rand_Int); set zn.CovParms;
  where CovParm = "Intercept";
   Rand_Int = Cov;
run; 

data zn.Residual(keep = Residual); set zn.CovParms;
  where CovParm = "Residual";
   Residual = Cov;
run; 

data zn.param_simu_group2_zinc;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
  Outcome = "Zinc";
  Group = 2;
run;



proc mixed data=zinc method=reml covtest;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;

		where cgroup_3 = 3;
run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = Slope Sl_SE Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
   Sl_SE = Stderr;
run; 

data zn.NObs;
 set zn.NObs;
  keep N;
   if Label = "Number of Observations Read";
run;

data zn.CovParms(keep = CovParm Cov);
 set zn.CovParms;
  Cov = Estimate;
run;

proc sort data = zn.CovParms; by CovParm; run;


data zn.Rand_Int(keep = Rand_Int); set zn.CovParms;
  where CovParm = "Intercept";
   Rand_Int = Cov;
run; 

data zn.Residual(keep = Residual); set zn.CovParms;
  where CovParm = "Residual";
   Residual = Cov;
run; 

data zn.param_simu_group3_zinc;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
  Outcome = "Zinc";
  Group = 3;
run;



proc mixed data=zinc method=reml covtest;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;

		where cgroup_4 = 4;
run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = Slope Sl_SE Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
   Sl_SE = Stderr;
run; 

data zn.NObs;
 set zn.NObs;
  keep N;
   if Label = "Number of Observations Read";
run;

data zn.CovParms(keep = CovParm Cov);
 set zn.CovParms;
  Cov = Estimate;
run;

proc sort data = zn.CovParms; by CovParm; run;


data zn.Rand_Int(keep = Rand_Int); set zn.CovParms;
  where CovParm = "Intercept";
   Rand_Int = Cov;
run; 

data zn.Residual(keep = Residual); set zn.CovParms;
  where CovParm = "Residual";
   Residual = Cov;
run; 

data zn.param_simu_group4_zinc;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
  Outcome = "Zinc";
  Group = 4;
run;


/*phytate*/
/*phytate*/
/*phytate*/
/*phytate*/
/*phytate*/


proc mixed data=zinc method=reml covtest;
class crop paircount cultivar ;
    model lphy =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;

		where cgroup_1 = 1;
run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = Slope Sl_SE Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
   Sl_SE = Stderr;
run; 

data zn.NObs;
 set zn.NObs;
  keep N;
   if Label = "Number of Observations Read";
run;

data zn.CovParms(keep = CovParm Cov);
 set zn.CovParms;
  Cov = Estimate;
run;

proc sort data = zn.CovParms; by CovParm; run;


data zn.Rand_Int(keep = Rand_Int); set zn.CovParms;
  where CovParm = "Intercept";
   Rand_Int = Cov;
run; 

data zn.Residual(keep = Residual); set zn.CovParms;
  where CovParm = "Residual";
   Residual = Cov;
run; 

data zn.param_simu_group1_phy;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
  Outcome = "Phy";
  Group = 1;
run;


proc mixed data=zinc method=reml covtest;
class crop paircount cultivar ;
    model lphy =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;

		where cgroup_2 = 2;
run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = Slope Sl_SE Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
   Sl_SE = Stderr;
run; 

data zn.NObs;
 set zn.NObs;
  keep N;
   if Label = "Number of Observations Read";
run;

data zn.CovParms(keep = CovParm Cov);
 set zn.CovParms;
  Cov = Estimate;
run;

proc sort data = zn.CovParms; by CovParm; run;


data zn.Rand_Int(keep = Rand_Int); set zn.CovParms;
  where CovParm = "Intercept";
   Rand_Int = Cov;
run; 

data zn.Residual(keep = Residual); set zn.CovParms;
  where CovParm = "Residual";
   Residual = Cov;
run; 

data zn.param_simu_group2_phy;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
  Outcome = "Phy";
  Group = 2;
run;



proc mixed data=zinc method=reml covtest;
class crop paircount cultivar ;
    model lphy =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;

		where cgroup_3 = 3;
run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = Slope Sl_SE Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
   Sl_SE = Stderr;
run; 

data zn.NObs;
 set zn.NObs;
  keep N;
   if Label = "Number of Observations Read";
run;

data zn.CovParms(keep = CovParm Cov);
 set zn.CovParms;
  Cov = Estimate;
run;

proc sort data = zn.CovParms; by CovParm; run;


data zn.Rand_Int(keep = Rand_Int); set zn.CovParms;
  where CovParm = "Intercept";
   Rand_Int = Cov;
run; 

data zn.Residual(keep = Residual); set zn.CovParms;
  where CovParm = "Residual";
   Residual = Cov;
run; 

data zn.param_simu_group3_phy;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
  Outcome = "Phy";
  Group = 3;
run;



proc mixed data=zinc method=reml covtest;
class crop paircount cultivar ;
    model lphy =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;

		where cgroup_4 = 4;
run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = Slope Sl_SE Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
   Sl_SE = Stderr;
run; 

data zn.NObs;
 set zn.NObs;
  keep N;
   if Label = "Number of Observations Read";
run;

data zn.CovParms(keep = CovParm Cov);
 set zn.CovParms;
  Cov = Estimate;
run;

proc sort data = zn.CovParms; by CovParm; run;


data zn.Rand_Int(keep = Rand_Int); set zn.CovParms;
  where CovParm = "Intercept";
   Rand_Int = Cov;
run; 

data zn.Residual(keep = Residual); set zn.CovParms;
  where CovParm = "Residual";
   Residual = Cov;
run; 

data zn.param_simu_group4_phy;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
  Outcome = "Phy";
  Group = 4;
run;





/*###sum data*/
/*###sum data*/
/*###sum data*/
/*###sum data*/

/*###sum data*/
/*###sum data*/
/*###sum data*/
/*###sum data*/

/*###sum data*/
/*###sum data*/
/*###sum data*/
/*###sum data*/


/*###sum data*/
/*###sum data*/
/*###sum data*/
/*###sum data*/



PROC IMPORT OUT= WORK.zincsum
            DATAFILE= "f:\Uni\Projects\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\export to SAS\zincsum.dbf" 
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
wgt= Znreplica10;
if crop_type=2 or crop_type=6 then cgroup_1 = 1; else cgroup_1 = 0;
if crop_type=7 or crop_type=3 then cgroup_2 = 2; else cgroup_2 = 0;
if crop_type=1 or crop_type=4  or crop_type=8 then cgroup_3 = 3; else cgroup_3 = 0;
if crop_type=1  or crop_type=8   then cgroup_4 = 4; else cgroup_4 = 0;
run; 



/*************************************************/
/*** Results for Simulation Zinc (sum) by CROP ***/
/*************************************************/

proc sort data = zincsum ; by crop_type   ;run; 



proc mixed data=Zincsum method=reml covtest;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		by crop_type;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;
run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = crop_type Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = crop_type Slope Sl_SE Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
   Sl_SE = Stderr;
run; 

data zn.NObs;
 set zn.NObs;
  keep crop_type N;
   if Label = "Number of Observations Read";
run;

data zn.CovParms(keep = crop_type CovParm Cov);
 set zn.CovParms;
  Cov = Estimate;
run;

proc sort data = zn.CovParms; by CovParm; run;


data zn.Rand_Int(keep = crop_type Rand_Int); set zn.CovParms;
  where CovParm = "Intercept";
   Rand_Int = Cov;
   Sl_SE = Stderr;
run; 

data zn.Residual(keep = crop_type Residual); set zn.CovParms;
  where CovParm = "Residual";
   Residual = Cov;
run; 

data zn.param_sum_simu_crop_zinc;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
  by crop_type;
    Outcome = "Zinc (sum)";
run;


/***************************************************/
/*** Results for Simulation Zinc (sum) by CGROUP ***/
/***************************************************/



proc mixed data=Zincsum method=reml covtest;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;

		where cgroup_1 = 1;

run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = cgroup Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = cgroup Slope Sl_SE Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
   Sl_SE = Stderr;
run; 

data zn.NObs;
 set zn.NObs;
  keep cgroup N;
   if Label = "Number of Observations Read";
run;

data zn.CovParms(keep = cgroup CovParm Cov);
 set zn.CovParms;
  Cov = Estimate;
run;

proc sort data = zn.CovParms; by CovParm; run;


data zn.Rand_Int(keep = cgroup Rand_Int); set zn.CovParms;
  where CovParm = "Intercept";
   Rand_Int = Cov;
run; 

data zn.Residual(keep = cgroup Residual); set zn.CovParms;
  where CovParm = "Residual";
   Residual = Cov;
run; 

data zn.param_sum_simu_group1_zinc;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
    Outcome = "Zinc (sum)";
	  Group = 1;
run;



proc mixed data=Zincsum method=reml covtest;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;

		where cgroup_2 = 2;

run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = cgroup Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = cgroup Slope Sl_SE Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
   Sl_SE = Stderr;
run; 

data zn.NObs;
 set zn.NObs;
  keep cgroup N;
   if Label = "Number of Observations Read";
run;

data zn.CovParms(keep = cgroup CovParm Cov);
 set zn.CovParms;
  Cov = Estimate;
run;

proc sort data = zn.CovParms; by CovParm; run;


data zn.Rand_Int(keep = cgroup Rand_Int); set zn.CovParms;
  where CovParm = "Intercept";
   Rand_Int = Cov;
run; 

data zn.Residual(keep = cgroup Residual); set zn.CovParms;
  where CovParm = "Residual";
   Residual = Cov;
run; 

data zn.param_sum_simu_group2_zinc;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
    Outcome = "Zinc (sum)";
	  Group = 2;
run;



proc mixed data=Zincsum method=reml covtest;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;

		where cgroup_3 = 3;

run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = cgroup Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = cgroup Slope Sl_SE Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
   Sl_SE = Stderr;
run; 

data zn.NObs;
 set zn.NObs;
  keep cgroup N;
   if Label = "Number of Observations Read";
run;

data zn.CovParms(keep = cgroup CovParm Cov);
 set zn.CovParms;
  Cov = Estimate;
run;

proc sort data = zn.CovParms; by CovParm; run;


data zn.Rand_Int(keep = cgroup Rand_Int); set zn.CovParms;
  where CovParm = "Intercept";
   Rand_Int = Cov;
run; 

data zn.Residual(keep = cgroup Residual); set zn.CovParms;
  where CovParm = "Residual";
   Residual = Cov;
run; 

data zn.param_sum_simu_group3_zinc;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
    Outcome = "Zinc (sum)";
	  Group = 3;
run;


proc mixed data=Zincsum method=reml covtest;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;

		where cgroup_4 = 4;

run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = cgroup Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = cgroup Slope Sl_SE Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
   Sl_SE = Stderr;
run; 

data zn.NObs;
 set zn.NObs;
  keep cgroup N;
   if Label = "Number of Observations Read";
run;

data zn.CovParms(keep = cgroup CovParm Cov);
 set zn.CovParms;
  Cov = Estimate;
run;

proc sort data = zn.CovParms; by CovParm; run;


data zn.Rand_Int(keep = cgroup Rand_Int); set zn.CovParms;
  where CovParm = "Intercept";
   Rand_Int = Cov;
run; 

data zn.Residual(keep = cgroup Residual); set zn.CovParms;
  where CovParm = "Residual";
   Residual = Cov;
run; 

data zn.param_sum_simu_group4_zinc;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
    Outcome = "Zinc (sum)";
	  Group = 4;
run;


data Zn.Final_Study_1;
 set Zn.Param_simu_crop_zinc 
     Zn.Param_simu_crop_phy
     zn.Param_simu_group1_zinc
	 zn.Param_simu_group2_zinc
	 zn.Param_simu_group3_zinc
	 zn.Param_simu_group4_zinc
     zn.Param_simu_group1_phy
	 zn.Param_simu_group2_phy
	 zn.Param_simu_group3_phy
	 zn.Param_simu_group4_phy

	 Zn.Param_sum_simu_crop_zinc 
	 zn.Param_sum_simu_group1_zinc
	 zn.Param_sum_simu_group2_zinc
	 zn.Param_sum_simu_group3_zinc
	 zn.Param_sum_simu_group4_zinc;
  Model = _n_;
run;


libname zn2 'f:\sast\zinc\' ;
OPTIONS nofmterr;

****libname Zn change for Ryan;
/*libname zn2 'C:\Users\Ryan\Documents\FAO FBS 2011\Myers CO2' ;
OPTIONS nofmterr;
libname zn 'C:\Users\Ryan\Documents\FAO FBS 2011\Myers CO2' ;*/


data Zn2.Final_Study_1;
set zn.Final_Study_1;
 if Probt <= 0.2 then Check = "OK"; else Check = "NO";
run; 

/*** If you want to change the cut-off for Phy ***/

data Zn2.Final_Study_1;
 set Zn2.Final_Study_1;
   if _n_ = 8  then Check = "NO";
   if _n_ = 18 then Check = "NO";
   if _n_ = 19 then Check = "NO";
   if _n_ = 20 then Check = "NO";
run;

 
/*** Start ***/

%macro loop;

  %do i = 1 %to 1000;

data zn2.Final_study_1_&i;
 set zn2.Final_study_1;
  Sim_slope = rand('NORMAL', Slope, Sl_SE);
  
run;


PROC IMPORT OUT= Crop_Name
  DATAFILE= "c:\Users\ekloog\Documents\My Dropbox\Myers CO2 UCDavis HSPH\faren_coded.xls" 
    DBMS=xls REPLACE;
	  GETNAMES=YES;
			RUN;

/*proc import datafile = 'C:\Users\Ryan\Documents\FAO FBS 2011\Myers CO2\faren_coded.XLS'
out=crop_name
dbms=excel replace;
sheet = 'crop_name';
run;*/

******************************Do Itai and Francesco need to do this?***********************************************;
	*Ryan : needed to make model_X numeric variable after import;	
***We are now working exclusively with model 86; *this is complete data set (summary) without rice; 
data zn2.Crop_Name;
set work.Crop_Name;
  if Sum_Zn_Mod_NR = . then delete;
   Model_1 = Phy_Model_WR*1 ;
   Model_2 = Zinc_Model_WR*1 ;
   Model_3 = Phy_Model_NR*1 ;
   Model_4 = Zinc_Model_NR*1 ;
   Model_5 = Sum_Zn_Mod_WR*1 ;
   Model_6 = Sum_Zn_Mod_NR*1 ;
   Model_7 = Phy_Model_WR*1 ;
   Model_8 = Phy_Model_NR*1 ;
run; 

data zn2.Final_study_1_&i;
set zn2.Final_study_1_&i;
   Model_1 = Model;
   Model_2 = Model;
   Model_3 = Model;
   Model_4 = Model;
   Model_5 = Model;
   Model_6 = Model;
   Model_7 = Model;
   Model_8 = Model;
run; 




/*** Model 6: Zinc (Without Rice) for Item or Group ***/

proc sort data = zn2.Final_study_1_&i; by Model_6; run;
proc sort data = zn2.Crop_Name;        by Model_6; run;

data zn2.Model_6_&i(drop = Model_1--Model_5 Model_7--Model_8);
 merge zn2.Crop_Name (in=a) zn2.Final_study_1_&i(in=b);
  by Model_6;
   if a and b;
run;

data zn2.Model_6_&i;
 set zn2.Model_6_&i;
  count = _n_ ;
run;


/*** Model 8: Phy (Without Rice) for Item or Group, this is the same as Model_3 ***/

proc sort data = zn2.Final_study_1_&i; by Model_8; run;
proc sort data = zn2.Crop_Name;        by Model_8; run;

data zn2.Model_8_&i(drop = Model_1--Model_7);
 merge zn2.Crop_Name (in=a) zn2.Final_study_1_&i(in=b);
  by Model_8;
   if a and b;
run;

data zn2.Model_8_&i;
 set zn2.Model_8_&i;
  count = _n_ ;
run;


proc datasets lib = Zn2;
 delete Final_study_1_&i;  
run;

%end;

%mend;

%loop;



%macro picked;

%do i = 1 %to 1000;

/** Without Rice Mod 86 ***/

/** Phy ***/

data zn2.model_8_&i(keep = ItemCode1 Item kcalrczn kcalrcphyt Raw_G4_Phy_Pct kcalrcphy_rg4m);
 set zn2.model_8_&i;
  Raw_G4_Phy_Pct = ((exp(Sim_slope)-1)*100);

   if Check = "OK" then kcalrcphy_rg4m = kcalrcphyt*((100 + Raw_G4_Phy_Pct)/100);
   if Check = "NO" then kcalrcphy_rg4m = kcalrcphyt;

run;

**Zn mod 6;
data zn2.model_6_&i(keep = ItemCode1 Item kcalrczn kcalrcphyt sum_G4_Zn_Pct kcalrczn_sumg4m);
 set zn2.model_6_&i;
  sum_G4_Zn_Pct = ((exp(Sim_slope)-1)*100);

   if Check = "OK" then kcalrczn_sumg4m = kcalrczn*((100+sum_G4_Zn_Pct)/100);
   if Check = "NO" then kcalrczn_sumg4m = kcalrczn;

run;

proc sort data = zn2.model_8_&i; by ItemCode1 ;run;
proc sort data = zn2.model_6_&i; by ItemCode1 ;run;

data zn2.Ran_Mod_86_&i;
merge zn2.model_8_&i(in=a) zn2.model_6_&i(in=b);
  by ItemCode1;
run;  

data zn2.Ran_Mod_86_&i(rename = (kcalrcphy_rg4m = kcalrcphy_sumg4m));
set zn2.Ran_Mod_86_&i;
 run;


proc datasets lib = ZN2;   delete model_8_&i model_6_&i; run; 

%end;

%MEND ;

%picked;



*********************;



*Import necessary datasets for the analysis;
*Import FAOFBS_T;

proc import datafile = 'f:\Uni\Projects\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\Myers CO2 UCDavis HSPH\FAOFBS_T_121004.dta'
 OUT= faofbs_t
  dbms = stata REPLACE;
run;


*Changed data path for Ryan;
/*proc import datafile = 'C:\Users\Ryan\Documents\FAO FBS 2011\Myers CO2\FAOFBS_T_121004.dta'
 OUT= faofbs_t
  dbms = stata REPLACE;
run;*/

proc sort data = faofbs_t; by ItemCode1  ;run; 


/*some sidcriptives to check the code*/

/*proc print data = Final_mod_12_check;*/
/* where Item = "Wheat" and Iteration in (1,2,3);*/
/*  var Country name_of_former_variable_1 mgZn_100kcal mgPhyt_100kcal kcalrczn--Iteration;*/
/*run; */
/*quit;*/
/**/
/**/
/*data xc;*/
/* set faofbs_t;*/
/*  if country_1 in ("Poland" , "Portugal");*/
/*run;*/
/**/
/**/
/**/
/*proc print data = xc;*/
/* where Item = "Wheat" ;*/
/*  var Country name_of_former_variable_1 kcal_cap_d ;*/
/*run; */
/*quit;*/
/**/
/**/
/**/
/*data yy;*/
/* set zn2.Results_Mod12_1;*/
/*  if country_1 in ("Poland" , "Portugal");*/
/*run;*/
/**/
/**/
/**/
/*proc print data = yy;*/
/* where Item = "Wheat" ;*/
/*  var Country name_of_former_variable_1 kcalrczn_rg3m;*/
/*run; */
/*quit;*/


PROC IMPORT OUT= znpymis
  DATAFILE= "f:\Uni\Projects\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\sup.data\zn_phy_missing.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
	
*Change path for Ryan;
/*PROC IMPORT OUT= znpymis
  DATAFILE= "C:\Users\Ryan\Documents\FAO FBS 2011\Myers CO2\zn_phy_missing.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;*/
	
 


options mprint;
%macro picked;

%do i = 1 %to 1000;



	*Results of Mod 86;

proc sort data = faofbs_t;           by ItemCode1 ;run;
proc sort data = zn2.Ran_mod_86_&i;   by ItemCode1 ;run;

data zn2.Results_Mod86_&i(drop=kcalrczn kcalrcphyt);
merge faofbs_t(in=a) zn2.Ran_mod_86_&i(in=b);
  by ItemCode1;
    if a;
run; 


proc sort data = zn2.Results_Mod86_&i; by ItemCode1   ;run;
proc sort data = znpymis ; by ItemCode1 ;run;

data zn2.Results_Mod86_&i;
merge zn2.Results_Mod86_&i(in=a) znpymis (in=b)  ;
  by ItemCode1;
    if a;
	run; 


/*** Replace Original values of kcalrczn and kcalrcphy when the adj is missing ****/


data zn2.Results_Mod86_&i;
 set zn2.Results_Mod86_&i;
  if kcalrczn_sumg4m   = . then kcalrczn_sumg4m  = kcalrczn;
  if kcalrcphy_sumg4m  = . then kcalrcphy_sumg4m = kcalrcphyt;
run;


%end;

%MEND ;

%picked;

*********************Now we need to calculate Zn and phytate intake based on extraction and processing estimates;

options mprint;
%macro picked;
%do i = 1 %to 1000;
*********************Now we need to calculate Zn and phytate intake based on extraction and processing estimates;

*need to do these calculations using Myers data (kcalrczn_sumg4m and kcalrcphy_rg3m based on CO2 changes);
data zn2.faofbs_s4_krw_&i;
set zn2.Results_Mod86_&i;

  iteration = &i;
  if itemcode1 = 25112 then do;

  mgZn_100kcal = (kcalrczn_sumg4m)*(Extract1)*(Extr_Zn) + (kcalrczn_sumg4m)*(1-Extract1); 
  mgPhyt_100kcal = (kcalrcphy_sumg4m)*(Extract1)*(Extr_Phyt)*(Ferment1)*(Ferm_Phyt) + (kcalrcphy_sumg4m)*(Extract1)*(Extr_Phyt)*(1-Ferment1) +
                   (kcalrcphy_sumg4m)*(1-Extract1); 
  end; *Wheat; *Whole grain wheat is NEVER fermented;

  else if itemcode1 = 25170 or itemcode1 = 25180 or itemcode1 = 25141 then do;  

     mgZn_100kcal = (kcalrczn_sumg4m)*(Extract1)*(Extr_Zn) + (kcalrczn_sumg4m)*(1-Extract1); 
     mgPhyt_100kcal = (kcalrcphy_sumg4m)*(Extract1)*(Extr_Phyt)*(Ferment1)*(Ferm_Phyt) + (kcalrcphy_sumg4m)*(Extract1)*(Extr_Phyt)*(1-Ferment1) + 
                      (kcalrcphy_sumg4m)*(1-Extract1)*(Ferment1)*(Ferm_Phyt) + (kcalrcphy_sumg4m)*(1-Extract1)*(1-Ferment1); 
  end; *Millet and sorghum and maize; 

  *Both whole grain and extracted millet and sorghum can be fermented;
  else if itemcode1 = 25230 or itemcode1 = 25340 or itemcode1 = 25202 then do; 
          mgZn_100kcal = kcalrczn_sumg4m; mgPhyt_100kcal = (kcalrcphy_sumg4m)*(Ferment1)*(Ferm_Phyt) + (kcalrcphy_sumg4m)*(1-Ferment1); 
  end; *Cassava and other roots, there is no effect on zinc so only effects on phytate due to fermentation, there is no extraction that affects phytate;

  else if itemcode1 = 25460 or itemcode1 = 25470 or itemcode1 = 25490 or itemcode1 = 25550 then do; 
      mgZn_100kcal = kcalrczn_sumg4m; mgPhyt_100kcal = (kcalrcphy_sumg4m)*(Extr_phyt); 
  end; *This is for beans; 

  else do; mgZn_100kcal = kcalrczn_sumg4m; mgPhyt_100kcal = kcalrcphy_sumg4m; 
  end; *This is for cereals other and all other itemcodes that do not have extraction or processing estimates applied to them;
run;


%end;

%MEND ;

%picked;




/*** Combine Dataset by Iteration ***/

options mprint;
%macro picked;

%do i = 1 %to 1000;

proc append base=zn2.Final_Mod_86 data=zn2.Faofbs_s4_krw_&i;
run;


proc datasets lib = zn2; delete Faofbs_s4_krw_&i ;

run; 

%end;

%MEND ;

%picked;







OPTIONS nofmterr;

ods listing close;*to suppress the output printing;
/*options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;*/

options notes source source2 MLOGIC MPRINT MRECALL SYMBOLGEN errors=0; *suppresses LOG WINDOW printing;

****This is now for Mod 86;

proc sort data = zn2.Final_Mod_86; by itemcode1; run;

proc univariate data = zn2.Final_Mod_86;
class country_code name_of_former_variable_1;
 var mgZn_100kcal;
  output out = work.out1_Mod_86(keep = itemcode1 country_code name_of_former_variable_1 P_2_5 P_50 P_97_5) pctlpre=P_ pctlpts= 0 to 100 by 2.5;
   by itemcode1;
run;

data work.out1_Mod_86_zn(drop = P_2_5 P_50 P_97_5);
 set work.out1_Mod_86; 
  P_2_5_zn  = P_2_5;
  P_50_zn   = P_50; 
  P_97_5_zn = P_97_5;
run;


proc univariate data = zn2.Final_Mod_86;
class country_code name_of_former_variable_1;
 var mgPhyt_100kcal;
  output out = work.out1_Mod_86(keep = itemcode1 country_code name_of_former_variable_1 P_2_5 P_50 P_97_5) pctlpre=P_ pctlpts= 0 to 100 by 2.5;
   by itemcode1;
run;

data work.out1_Mod_86_phy(drop = P_2_5 P_50 P_97_5);
 set work.out1_Mod_86; 
  P_2_5_phy   = P_2_5;
  P_50_phy    = P_50; 
  P_97_5_phy  = P_97_5;
run;


proc sort data = work.out1_Mod_86_phy; by country_code name_of_former_variable_1 itemcode1 ;run;
proc sort data = work.out1_Mod_86_zn ; by country_code name_of_former_variable_1 itemcode1 ;run;

data work.Out1_Final_Mod86;
merge work.out1_Mod_86_zn(in=a) out1_Mod_86_phy (in=b)  ;
  by   country_code name_of_former_variable_1 itemcode1;
run;


data work.Variable(drop = iteration -- mgPhyt_100kcal);
 set zn2.Final_Mod_86;
  if iteration = 1;
run;


proc sort data = work.Variable;         by country_code name_of_former_variable_1 itemcode1 ;run;
proc sort data = work.Out1_Final_Mod86; by country_code name_of_former_variable_1 itemcode1 ;run;

data work.Out1_Final_Mod86;
merge work.Variable(in=a) work.Out1_Final_Mod86 (in=b)  ;
  by   country_code name_of_former_variable_1 itemcode1;
run;



*Import krw_nutrient excel spreadsheet;

proc import datafile = 'f:\Uni\Projects\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\Myers CO2 UCDavis HSPH\summ_ic_kcal.xls' 
 OUT = krw_nutrient
 dbms = xls REPLACE;
  sheet = krw_nutrient;
run;
       
proc import datafile = 'f:\Uni\Projects\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\Myers CO2 UCDavis HSPH\FAOFBS_wpp2010.xls'
 OUT= wpp2010_b
  dbms = xls REPLACE;
   sheet = wpp2010_b;
run;
proc import datafile = 'f:\Uni\Projects\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\Myers CO2 UCDavis HSPH\FAOFBS_wpp2010.xls'
 OUT= wpp2010_znreq
  dbms = xls REPLACE;
   sheet = wpp2010_znreq;
run;
*Changed paths for Ryan;

/*proc import datafile = 'C:\Users\Ryan\Documents\FAO FBS 2011\Ndsdata.comp\summ_ic_kcal.xls' 
 OUT = krw_nutrient
 dbms = xls REPLACE;
  sheet = krw_nutrient;
run;
       
proc import datafile = 'C:\Users\Ryan\Documents\FAO FBS 2011\Myers CO2\FAOFBS_wpp2010.xlsx'
 OUT= wpp2010_b
  dbms = xlsx REPLACE;
   sheet = wpp2010_b;
run;
proc import datafile = 'C:\Users\Ryan\Documents\FAO FBS 2011\Myers CO2\FAOFBS_wpp2010.xlsx'
 OUT= wpp2010_znreq
  dbms = xlsx REPLACE;
   sheet = wpp2010_znreq;
run;*/



data work.Out1_Final_Mod86;
set work.Out1_Final_Mod86;

if itemcode1 = 25112 then do;

  mgZn_100kcal_org = (kcalrczn)*(Extract1)*(Extr_Zn) + (kcalrczn)*(1-Extract1); 
  mgPhyt_100kcal_org = (kcalrcphyt)*(Extract1)*(Extr_Phyt)*(Ferment1)*(Ferm_Phyt) + (kcalrcphyt)*(Extract1)*(Extr_Phyt)*(1-Ferment1) +
                   (kcalrcphyt)*(1-Extract1); 
  end; *Wheat; *Whole grain wheat is NEVER fermented;

  else if itemcode1 = 25170 or itemcode1 = 25180 or itemcode1 = 25141 then do;  

     mgZn_100kcal_org = (kcalrczn)*(Extract1)*(Extr_Zn) + (kcalrczn)*(1-Extract1); 
     mgPhyt_100kcal_org = (kcalrcphyt)*(Extract1)*(Extr_Phyt)*(Ferment1)*(Ferm_Phyt) + (kcalrcphyt)*(Extract1)*(Extr_Phyt)*(1-Ferment1) + 
                      (kcalrcphyt)*(1-Extract1)*(Ferment1)*(Ferm_Phyt) + (kcalrcphyt)*(1-Extract1)*(1-Ferment1); 
  end; *Millet and sorghum and maize; 

  *Both whole grain and extracted millet and sorghum can be fermented;
  else if itemcode1 = 25230 or itemcode1 = 25340 or itemcode1 = 25202 then do; 
          mgZn_100kcal_org  = kcalrczn; mgPhyt_100kcal_org = (kcalrcphyt)*(Ferment1)*(Ferm_Phyt) + (kcalrcphyt)*(1-Ferment1); 
  end; *Cassava and other roots, there is no effect on zinc so only effects on phytate due to fermentation, there is no extraction that affects phytate;

  else if itemcode1 = 25460 or itemcode1 = 25470 or itemcode1 = 25490 or itemcode1 = 25550 then do; 
      mgZn_100kcal_org  = kcalrczn; mgPhyt_100kcal_org = (kcalrcphyt)*(Extr_phyt); 
  end; *This is for beans; 

  else do; mgZn_100kcal_org  = kcalrczn; mgPhyt_100kcal_org  = kcalrcphyt; 
  end; *This is for cereals other and all other itemcodes that do not have extraction or processing estimates applied to them;
run;


data work.faofbs_krw;
 set work.Out1_Final_Mod86;
 
  krw_mgZn_cd_25    = (P_2_5_zn   / 100 ) * kcal_cap_d;
  krw_mgZn_cap_d    = (P_50_zn / 100 ) * kcal_cap_d;
  krw_mgZn_cd_97_5  = (P_97_5_zn   / 100 ) * kcal_cap_d;
  
  krw_mgZn_cd_org = (mgZn_100kcal_org / 100)* kcal_cap_d;

  krw_mgPhyt_cd_25    = (P_2_5_phy / 100)  * kcal_cap_d;
  krw_mgPhyt_cap_d    = (P_50_phy / 100) * kcal_cap_d;
  krw_mgPhyt_cd_97_5  = (P_97_5_phy / 100)   * kcal_cap_d;

  krw_mgPhyt_cd_org = (mgPhyt_100kcal_org/ 100)* kcal_cap_d;

run;



*Create mean estimate for 2003-2007 FAOFBS data;
*2005 time frame, brackets dates from 2003-2007;


data work.faofbs_krw (rename =(name_of_former_variable_1 = year));
 set work.faofbs_krw ;
run; 

proc sort data=work.faofbs_krw;
by continent_code continent region_code region country_code country iso_code
   itemcode1 itemcodeB itemcode;
run; 

proc summary data=work.faofbs_krw;
 var  kcal_cap_d kcalrczn kcalrcphyt extract1 extract2 extr_zn extr_phyt ferment1 ferm_phyt

krw_mgZn_cd_25    krw_mgZn_cap_d    krw_mgZn_cd_97_5   krw_mgZn_cd_org
krw_mgPhyt_cd_25  krw_mgPhyt_cap_d  krw_mgPhyt_cd_97_5 krw_mgPhyt_cd_org 

P_2_5_zn  P_50_zn P_97_5_zn
P_2_5_phy P_50_phy P_97_5_phy;

  by continent_code continent region_code region country_code country iso_code itemcode1 itemcodeB itemcode;

where year IN ('y2003', 'y2004', 'y2005', 'y2006', 'y2007');

output out=faofbs_krw2005a mean = ;

run;

***Now transpose data to get foods as variables and kcal_cap_d, mgZn_cap_d and mgphyt_cap_d on separate lines, sorted by 
country;



proc sort data=work.faofbs_krw2005a;
by continent_code continent region_code region country_code country iso_code  ;
run;

*Transpose data ;
proc transpose data=work.faofbs_krw2005a out = work.faofbs_krw2005;
by continent_code continent region_code region country_code country iso_code ;
id itemcode1;
var krw_mgZn_cd_25    krw_mgZn_cap_d    krw_mgZn_cd_97_5   krw_mgZn_cd_org
    krw_mgPhyt_cd_25  krw_mgPhyt_cap_d  krw_mgPhyt_cd_97_5 krw_mgPhyt_cd_org kcal_cap_d;
run;
 
*Create calculated total of kcal_cap_d, mgZn_cap_d and mgphyt_cap_d based on individual food commodities;
data work.faofbs_krw2005;
set work.faofbs_krw2005;
calc_tot = SUM (OF _25112 -- _25202);
run;





proc sort data=work.faofbs_krw2005;
by continent_code continent region_code region country_code country iso_code  ;
run;

proc transpose data=work.faofbs_krw2005 out = work.faofbs_krwyear;
by continent_code continent region_code region country_code country iso_code ;
id _name_;
var calc_tot;
run;


*****Merging pop_total data (from work.wpp2010_b) with faofbs_year dataset;
*Because right now FAOFBS_krwyear Kcal, Zn and Phytate are calculated on a per capita basis and we want to know how much
zinc is in the national food supply according to these calculations;


proc sort data=work.faofbs_krwyear;
by country_code ;
run;

proc sort data=work.wpp2010_b ;
by country_code ;
run;

data work.faofbs_krwyear (rename = (_name_ = nut_cap_d));
merge work.faofbs_krwyear work.wpp2010_b (keep = country_code pop_tot1000 );
by country_code ;
run;
*Now we need to merge the population numbers with the faofbs_krwyear dataset from above;


proc sort data=work.wpp2010_znreq;
by  country_code  ;
run;

data work.faofbs_krwyear1;
merge work.faofbs_krwyear wpp2010_znreq (keep= country_code 
PrZn_mean iomprzn_mean krw_earzn_mean krw_iomearzn_mean);
by country_code ;
run;




*Calculation phytate:Zn ratios here now;
data work.faofbs_krwyear1;
set work.faofbs_krwyear1;

krw_P25_Zn   = (krw_mgPhyt_cd_25/660)/(krw_mgZn_cd_25 /65.4);
krw_P_Zn     = (krw_mgphyt_cap_d/660)/(krw_mgZn_cap_d/65.4);
krw_P97_5_Zn = (krw_mgPhyt_cd_97_5/660)/(krw_mgZn_cd_97_5  /65.4);

krw_P_Zn_org  = (krw_mgphyt_cd_org/660)/(krw_mgZn_cd_org/65.4);

run;

*** Application of Miller equation;

*Have kcal,zinc, and phytate values as a single observation;

/*Miller version 2010 (Hambidge et al. AJCN 2010;91 (suppl):1478s-83S*/

data work.faofbs_krwyear1;
set work.faofbs_krwyear1;
      amax2010 = .091;
      kr2010 = .033;
      kp2010 = .68;
run;

data work.faofbs_krwyear1;
set work.faofbs_krwyear1;

      krw_millernum2010_25  = amax2010+(krw_mgZn_cd_25/65.38)+kr2010*(1+(krw_mgPhyt_cd_25/660.08)/kp2010);
      krw_millernum2010     = amax2010+(krw_mgZn_cap_d/65.38)+kr2010*(1+(krw_mgphyt_cap_d/660.08)/kp2010);
      krw_millernum2010_97_5 = amax2010+(krw_mgZn_cd_97_5/65.38)+kr2010*(1+(krw_mgPhyt_cd_97_5/660.08)/kp2010);
      krw_millernum2010_org = amax2010+(krw_mgZn_cd_org/65.38)+kr2010*(1+(krw_mgphyt_cd_org/660.08)/kp2010);

      krw_absznmiller_25   = 65.38*0.5*(krw_millernum2010_25-SQRT(krw_millernum2010_25**2-4*amax2010*(krw_mgZn_cd_25/65.38)));
      krw_absznmiller      = 65.38*0.5*(krw_millernum2010-SQRT(krw_millernum2010**2-4*amax2010*(krw_mgZn_cap_d/65.38)));
      krw_absznmiller_97_5 = 65.38*0.5*(krw_millernum2010_97_5-SQRT(krw_millernum2010_97_5**2-4*amax2010*(krw_mgZn_cd_97_5/65.38)));
	  krw_absznmiller_org   = 65.38*0.5*(krw_millernum2010_org-SQRT(krw_millernum2010_org**2-4*amax2010*(krw_mgZn_cd_org/65.38)));


      krw_fazmiller_25   = krw_absznmiller_25/krw_mgZn_cd_25;
	  krw_fazmiller      = krw_absznmiller/krw_mgZn_cap_d;
      krw_fazmiller_97_5 = krw_absznmiller_97_5/krw_mgZn_cd_97_5;
      krw_fazmiller_org  = krw_absznmiller_org/krw_mgZn_cd_org;

run;



data work.faofbs_krwdef_iom;
set work.faofbs_krwyear1;

      krw_iomprzn_earpctmiller_25 = 100*krw_absznmiller_25/(iomprzn_mean);
      krw_iomprzn_earpctmiller = 100*krw_absznmiller/(iomprzn_mean);
      krw_iomprzn_earpctmiller_97_5 = 100*krw_absznmiller_97_5/(iomprzn_mean);
	  krw_iomprzn_earpctmiller_org = 100*krw_absznmiller_org/(iomprzn_mean);

      krw_iomprzn_pctdefmiller_25 = 100*probnorm((iomprzn_mean-krw_absznmiller_25)/(.25*krw_absznmiller_25));
      krw_iomprzn_pctdefmiller = 100*probnorm((iomprzn_mean-krw_absznmiller)/(.25*krw_absznmiller));
      krw_iomprzn_pctdefmiller_97_5 = 100*probnorm((iomprzn_mean-krw_absznmiller_97_5)/(.25*krw_absznmiller_97_5));
      krw_iomprzn_pctdefmiller_org = 100*probnorm((iomprzn_mean-krw_absznmiller_org)/(.25*krw_absznmiller_org));

run;




data zn2.faofbs_krwdef_tiom_Mod86;
set  work.faofbs_krwdef_iom;
run; 


