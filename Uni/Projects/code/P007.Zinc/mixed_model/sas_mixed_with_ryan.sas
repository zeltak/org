
libname zn 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.5.Results\SAS\' ;




PROC IMPORT OUT= WORK.zinc
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\export to SAS\zinc.dbf" 
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
if crop_type=6  then cgroup=4;
if crop_type=1 then c1dumm=0;
else if crop_type=2 then c1dumm=1;
else if crop_type=3 then c1dumm=2;
else if crop_type=4 then c1dumm=3;
else if crop_type=5 then c1dumm=4;
else if crop_type=6 then c1dumm=5;
run; 


/********************************************/
/*** Results for Simulation Zinc. by CROP ***/
/********************************************/

proc sort data = zinc ; by crop_type   ;run; 

ods trace on;

proc mixed data=zinc method=reml covtest;
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

data zn.Slope(keep = crop_type Slope Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
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

ods trace on;

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

data zn.Slope(keep = crop_type Slope Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
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

proc sort data = zinc ; by cgroup   ;run; 

ods trace on;

proc mixed data=zinc method=reml covtest;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		by cgroup;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;
run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = cgroup Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = cgroup Slope Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
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

data zn.param_simu_group_zinc;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
  by cgroup;
    Outcome = "Zinc";
run;

/********************************************/
/*** Results for Simulation Phy by CGROUP ***/
/********************************************/

proc sort data = zinc ; by cgroup   ;run; 

ods trace on;

proc mixed data=Zinc method=reml covtest;
class crop paircount cultivar ;
    model lphy =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		by cgroup;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;
run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = cgroup Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = cgroup Slope Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
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

data zn.param_simu_group_PHY;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
  by cgroup;
    Outcome = "Phy";
run;




/*###sum data*/



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
if crop_type=2 or crop_type=6 then cgroup=1;
if crop_type=7 or crop_type=3 then cgroup=2;
if crop_type=1 or crop_type=4 or crop_type=5 or crop_type=8 then cgroup=3;
if crop_type=1 or crop_type=4  or crop_type=8 then cgroup=4;
if crop_type=1 then c1dumm=0;
else if crop_type=2 then c1dumm=1;
else if crop_type=3 then c1dumm=2;
else if crop_type=4 then c1dumm=3;
else if crop_type=5 then c1dumm=4;
else if crop_type=6 then c1dumm=5;
else if crop_type=7 then c1dumm=6;
else if crop_type=8 then c1dumm=7;
run; 



/*************************************************/
/*** Results for Simulation Zinc (sum) by CROP ***/
/*************************************************/

proc sort data = zincsum ; by crop_type   ;run; 

ods trace on;

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

data zn.Slope(keep = crop_type Slope Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
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

data zn.param_sum_simu_crop_zinc;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
  by crop_type;
    Outcome = "Zinc (sum)";
run;


/***************************************************/
/*** Results for Simulation Zinc (sum) by CGROUP ***/
/***************************************************/

proc sort data = zincsum ; by cgroup   ;run; 

ods trace on;

proc mixed data=Zincsum method=reml covtest;
class crop paircount cultivar ;
    model lzinc =  CO  water nitrolevel/ s;
     random int  / sub = paircount s;
		by cgroup;
		ods output SolutionF = zn.SolutionF;
		ods output CovParms  = zn.CovParms;
	    ods output NObs      = zn.NObs;
run;

proc sort data = zn.SolutionF; by Effect; run;

data zn.Intercept(keep = cgroup Intercept); set zn.SolutionF;
  where Effect = "Intercept";
   Intercept = Estimate;
run; 

data zn.Slope(keep = cgroup Slope Probt); set zn.SolutionF;
  where Effect = "CO";
   Slope = Estimate;
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

data zn.param_sum_simu_group_zinc;
length Outcome $ 12;
 merge zn.Intercept zn.Slope zn.Rand_Int zn.Residual zn.NObs;
  by cgroup;
    Outcome = "Zinc (sum)";
run;

data Zn.Final_Study_1;
 set Zn.Param_simu_crop_zinc Zn.Param_simu_crop_phy   Zn.Param_simu_group_zinc Zn.Param_simu_group_phy Zn.Param_sum_simu_crop_zinc Zn.Param_sum_simu_group_zinc ;
  Model = _n_;
run;








PROC IMPORT OUT= Crop_Name
  DATAFILE= "c:\Users\ekloog\Documents\My Dropbox\Myers CO2 UCDavis HSPH\faren_coded.xls" 
    DBMS=xls REPLACE;
	  GETNAMES=YES;
			RUN;
		 
data Zn.Crop_Name;
set Crop_Name;
  if Sum_Zn_Mod_NR = . then delete;
   Model_1 = Phy_Model_WR ;
   Model_2 = Zinc_Model_WR;
   Model_3 = Phy_Model_NR;
   Model_4 = Zinc_Model_NR;
   Model_5 = Sum_Zn_Mod_WR;
   Model_6 = Sum_Zn_Mod_NR;
run; 

data Zn.Final_study_1;
set Zn.Final_study_1;
   Model_1 = Model;
   Model_2 = Model;
   Model_3 = Model;
   Model_4 = Model;
   Model_5 = Model;
   Model_6 = Model;
run; 



/*** Model 1: Phy (With Rice) for Item or Group ***/

proc sort data = Zn.Final_study_1; by Model_1; run;
proc sort data = Zn.Crop_Name;     by Model_1; run;

data Zn.Model_1(drop = Model_2--Model_6);
 merge Zn.Crop_Name (in=a) Zn.Final_study_1(in=b);
  by model_1;
   if a and b;
run;

data Zn.Model_1;
 set Zn.Model_1;
  count = _n_ ;
run;


/*** Model 2: Zinc (With Rice) for Item or Group ***/

proc sort data = Zn.Final_study_1; by Model_2; run;
proc sort data = Zn.Crop_Name;     by Model_2; run;

data Zn.Model_2(drop = Model_1 Model_3--Model_6);
 merge Zn.Crop_Name (in=a) Zn.Final_study_1(in=b);
  by Model_2;
   if a and b;
run;

data Zn.Model_2;
 set Zn.Model_2;
  count = _n_ ;
run;

/*** Model 3: Phy (Without Rice) for Item or Group ***/

proc sort data = Zn.Final_study_1; by Model_3; run;
proc sort data = Zn.Crop_Name;     by Model_3; run;

data Zn.Model_3(drop = Model_1--Model_2 Model_4--Model_6);
 merge Zn.Crop_Name (in=a) Zn.Final_study_1(in=b);
  by Model_3;
   if a and b;
run;

data Zn.Model_3;
 set Zn.Model_3;
  count = _n_ ;
run;


/*** Model 4: Zinc (Without Rice) for Item or Group ***/

proc sort data = Zn.Final_study_1; by Model_4; run;
proc sort data = Zn.Crop_Name;     by Model_4; run;

data Zn.Model_4(drop = Model_1--Model_3 Model_5--Model_6);
 merge Zn.Crop_Name (in=a) Zn.Final_study_1(in=b);
  by Model_4;
   if a and b;
run;

data Zn.Model_4;
 set Zn.Model_4;
  count = _n_ ;
run;

/*** Model 5: Zinc (Without Rice) for Item or Group ***/

proc sort data = Zn.Final_study_1; by Model_5; run;
proc sort data = Zn.Crop_Name;     by Model_5; run;

data Zn.Model_5(drop = Model_1--Model_4 Model_6);
 merge Zn.Crop_Name (in=a) Zn.Final_study_1(in=b);
  by Model_5;
   if a and b;
run;

data Zn.Model_5;
 set Zn.Model_5;
  count = _n_ ;
run;


/*** Model 6: Zinc (Without Rice) for Item or Group ***/

proc sort data = Zn.Final_study_1; by Model_6; run;
proc sort data = Zn.Crop_Name;     by Model_6; run;

data Zn.Model_6(drop = Model_1--Model_5);
 merge Zn.Crop_Name (in=a) Zn.Final_study_1(in=b);
  by Model_6;
   if a and b;
run;

data Zn.Model_6;
 set Zn.Model_6;
  count = _n_ ;
run;

ods listing close;*to suppress the output printing;
options nonotes nosource nosource2 ; *suppresses LOG WINDOW printing;


/* Generate Data */

options mprint;

%macro Model(Type =, obs =);

%do n = 1 %to &obs;

data ZN.Model&n(keep = ItemCodeB--kcalrcphyt Item iteration CO Y paircount); 
 set Zn.&type;
  if _n_ = &n;
   do iteration = 1 to 100;

     seed = 987624; /*seed for random # generator*/

       do paircount = 1 to N; /*N = subjects*/

 Gamma00 =  Intercept ; /*fixed intercept*/
 Gamma01 =  Slope;      /*fixed slope*/

 u0j = sqrt(Rand_Int)*rannor(seed); /*random intercept error term*/

 B0J = Gamma00 + u0j; /*random intercept*/

  do CO = 0 to 1; /*2 time points*/

  eij = sqrt(Residual)*rannor(seed); /* error term */

   Y = B0J + Gamma01*CO + eij; /* full equation */

  output;
 end;
 end;
 end;

run;

proc mixed data=ZN.Model&n;
class paircount;
 model Y = CO / solution;
  random intercept  / subject = paircount;
   ods output  SolutionF =  Sol_Mod&n;
    by iteration;
run;


data Sol_Mod&n(keep = Iteration Estimate StdErr);
 set Sol_Mod&n;
  if Effect = "CO";
run;

proc sort data = Sol_Mod&n;           by Iteration; run;
proc sort data = ZN.Model&n nodupkey; by Iteration; run;

data ZN.Model&n(drop = CO Y paircount);
 merge ZN.Model&n Sol_Mod&n;
  by Iteration;
run;

proc surveyselect data = ZN.Model&n
  Method = SRS n = 1 out = ZN.Model&n;
run;
quit;

proc append base=ZN.Radom_&Type data=ZN.Model&n;
run;

proc datasets lib = Work; delete Sol_Mod&n; run;
proc datasets lib = ZN;   delete Model&n;   run;

dm  'clear output';
dm  'clear log';

%end;

%mend;

%Model(Type = Model_1, obs = 20);
%Model(Type = Model_2, obs = 20);
%Model(Type = Model_3, obs = 20);
%Model(Type = Model_4, obs = 20);
%Model(Type = Model_5, obs = 25);
%Model(Type = Model_6, obs = 25);


data ZN.Radom_Model_1(drop = StdErr);
 set Zn.Model1 Zn.Model2 Zn.Model3 Zn.Model4 Zn.Model5 Zn.Model6 Zn.Model7 Zn.Model18 Zn.Model19
     Zn.Model10 Zn.Model11 Zn.Model12 Zn.Model13 Zn.Model14 Zn.Model15 Zn.Model16 Zn.Model17 Zn.Model18 Zn.Model19 Zn.Model20;

	  Raw_G3_Zn_Pct = ((exp(Estimate)-1)*100);
	  kcalrczn_rg3m = kcalrczn*((100+Raw_G3_Zn_Pct)/100);

run; 

proc datasets lib = ZN; 
 delete Model1 Model2 Model3 Model4 Model5 Model6 Model7 Model8 Model9 
        Model10 Model11 Model12 Model13 Model14 Model15 Model16 Model17 Model18 Model19 Model20;
run;
  


*Import necessary datasets;
*Import FAOFBS_T;
proc import datafile = 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\Myers CO2 UCDavis HSPH\FAOFBS_T_121004.dta'
OUT= faofbs_t
dbms = stata REPLACE;
run;
*Import krw_nutrient excel spreadsheet;
proc import datafile = 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\Myers CO2 UCDavis HSPH\summ_ic_kcal.xls' 
OUT = krw_nutrient
dbms = xls REPLACE;
sheet = krw_nutrient;
run;
       
proc import datafile = 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\Myers CO2 UCDavis HSPH\FAOFBS_wpp2010.xls'
OUT= wpp2010_b
dbms = xls REPLACE;
sheet = wpp2010_b;
run;
proc import datafile = 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\Myers CO2 UCDavis HSPH\FAOFBS_wpp2010.xls'
OUT= wpp2010_prb
dbms = xls REPLACE;
sheet = wpp2010_prb;
run;

proc sort data = faofbs_t;         by item; run;
proc sort data = zn.Radom_model_1; by item; run;

data check;
 merge faofbs_t(in=a) zn.Radom_model_1(in=b);
  by item;
    if b;
run; 


OPTIONS source notes source source2 ;




*********************Now we need to calculate Zn and phytate intake based on extraction and processing estimates;

*need to do these calculations using Myers data (kcalrczn and kcalrcphyt based on CO2 changes);
data work.faofbs_krw;
set check;
if itemcode1 = 25112 then do; mgZn_100kcal = (kcalrczn)*(Extract1)*(Extr_Zn) + (kcalrczn)*(1-Extract1); 
mgPhyt_100kcal = (kcalrcphyt)*(Extract1)*(Extr_Phyt)*(Ferment1)*(Ferm_Phyt) + (kcalrcphyt)*(Extract1)*(Extr_Phyt)*(1-Ferment1) 
+ (kcalrcphyt)*(1-Extract1); end; *Wheat; *Whole grain wheat is NEVER fermented;
else if itemcode1 = 25170 or itemcode1 = 25180 or itemcode1 = 25141 then do;  mgZn_100kcal = (kcalrczn)*(Extract1)*(Extr_Zn) + 
(kcalrczn)*(1-Extract1); mgPhyt_100kcal = (kcalrcphyt)*(Extract1)*(Extr_Phyt)*(Ferment1)*(Ferm_Phyt) + (kcalrcphyt)*(Extract1)*(Extr_Phyt)*(1-Ferment1) + 
(kcalrcphyt)*(1-Extract1)*(Ferment1)*(Ferm_Phyt) + (kcalrcphyt)*(1-Extract1)*(1-Ferment1); end; *Millet and sorghum and maize; 
*Both whole grain and extracted millet and sorghum can be fermented;
else if itemcode1 = 25230 or itemcode1 = 25340 or itemcode1 = 25202 then do; mgZn_100kcal = kcalrczn; mgPhyt_100kcal =
(kcalrcphyt)*(Ferment1)*(Ferm_Phyt) + (kcalrcphyt)*(1-Ferment1); end; *Cassava and other roots, there is no effect on zinc so only effects on phytate due to
fermentation, there is no extraction that affects phytate;
else if itemcode1 = 25460 or itemcode1 = 25470 or itemcode1 = 25490 or itemcode1 = 25550 then do; mgZn_100kcal = kcalrczn; mgPhyt_100kcal =
(kcalrcphyt)*(Extr_phyt); end; *This is for beans; 
else do; mgZn_100kcal = kcalrczn; mgPhyt_100kcal = kcalrcphyt; end; *This is for cereals other and all other itemcodes that do not have
extraction or processing estimates applied to them;
run;

*Calculate amt. of zinc and phytate received per capita per day per food commodity; 
*This is calculated by multiplying kcal per capita per day of food by mg Zn in 100kcal and dividing by 100kcal;

data work.faofbs_krw;
set work.faofbs_krw;
krw_mgZn_cap_d = (mgZn_100kcal / 100 ) * kcal_cap_d;
krw_mgPhyt_cap_d = (mgPhyt_100kcal / 100) *kcal_cap_d;
run;

*Create mean estimate for 2003-2007 FAOFBS data;
*2005 time frame, brackets dates from 2003-2007;
proc sort data=work.faofbs_krw;
by continent_code continent region_code region country_code country iso_code imputeregion imputecountry
itemcode1 itemcodeB itemcode item;
run;

proc summary data=work.faofbs_krw;
var  kcal_cap_d kcalrczn kcalrcphyt extract1 extract2 extr_zn extr_phyt ferment1 ferm_phyt mgzn_100kcal mgphyt_100kcal
krw_mgZn_cap_d krw_mgPhyt_cap_d ;
by continent_code continent region_code region country_code country iso_code imputeregion imputecountry
itemcode1 itemcodeB itemcode item;
where year IN ('y2003', 'y2004', 'y2005', 'y2006', 'y2007');
output out=faofbs_krw2005 mean = ;
run;

data work.faofbs_krw2005;
set work.faofbs_krw2005;
year = 'b2005';
run;


***Now transpose data to get foods as variables and kcal_cap_d, mgZn_cap_d and mgphyt_cap_d on separate lines, sorted by 
country;

proc sort data=work.faofbs_krw2005;
by continent_code continent region_code region country_code country iso_code imputeregion imputecountry year;
run;

*Transpose data ;
proc transpose data=work.faofbs_krw2005 out = work.faofbs_krw2005;
by continent_code continent region_code region country_code country iso_code imputeregion imputecountry year;
id itemcode1;
var kcal_cap_d krw_mgZn_cap_d krw_mgphyt_cap_d ;
run;
 *Add item labels to variables;

 data work.faofbs_krw2005;
 set work.faofbs_krw2005;
 label _1 = 'Alcohol, Non-Food'
_2=    'Alcoholic Beverages +'  _3  =  'Animal Fats +' _4   = 'Animal Products +' _8  =  'Aquatic Products, Other +'
_19   = 'Cereals - Excluding Beer +' _37  =  'Fish, Seafood +' _39  =  'Fruits - Excluding Wine +'
_41   = 'Grand Total +'  _51 =   'Meat +' _56 =   'Miscellaneous +' _61 =   'Oilcrops +' _81 =   'Pulses +'
_94 =   'Spices +' _96  =  'Starchy Roots +' _97 =   'Stimulants +' _98 =   'Sugar & Sweeteners +' _103  =  'Sugarcrops +'
_111 =   'Vegetable Oils +'  _112  =  'Vegetables +' _114   = 'Vegetal Products +' _25112 =   'Wheat' _25130  =  'Barley'
_25141 =    'Maize' _25150 =     'Rye' _25160 =   'Oats' _25170  =  'Millet' _25180 =   'Sorghum' _25201  =  'Cereals, Other'
_25202 = 'Cereals, Other, Teff' _25310  =  'Potatoes' _25320 =   'Cassava'  _25330 =   'Sweet Potatoes'  _25340  =  'Roots, Other'
_25350 =   'Yams' _25360 =   'Sugar Cane' _25370 =   'Sugar Beet' _25410 =   'Sugar, Non-Centrifugal'  _25420 =   'Sugar (Raw Equivalent)' 
_25430  =  'Sweeteners, Other' _25460 =   'Beans'  _25470 =   'Peas'  _25490 =   'Pulses, Other' _25510 =   'Tree Nuts'
_25550 =   'Soyabeans' _25560 =   'Groundnuts (Shelled Eq)' _25570 =   'Sunflowerseed' _25600  =  'Coconuts - Incl Copra'
_25610 =   'Sesameseed' _25630 =   'Olives' _25700  =  'Oilcrops, Other' _25710  =  'Soyabean Oil'  _25720  =  'Groundnut Oil'
_25730  =  'Sunflowerseed Oil' _25740 =   'Rape and Mustard Oil' _25750 =   'Cottonseed Oil' _25760  =  'Palmkernel Oil'
_25770 =   'Palm Oil' _25780 =   'Coconut Oil' _25790 =   'Sesameseed Oil' _25800 =  'Olive Oil' _25810  =  'Ricebran Oil'
_25820  =  'Maize Germ Oil' _25860  =  'Oilcrops Oil, Other' _26010  =  'Tomatoes'  _26020 =   'Onions' _26050 =   'Vegetables, Other'
_26110 =   'Oranges, Mandarines'    _26120 =    'Lemons, Limes' _26130  =  'Grapefruit' _26140  =  'Citrus, Other'
_26150 =   'Bananas' _26160 =   'Plantains' _26170 =   'Apples' _26180  =  'Pineapples' _26190 =   'Dates' _26200=    'Grapes'
_26250 =   'Fruits, Other' _26300=    'Coffee' _26330 =   'Cocoa Beans' _26350=    'Tea' _26400 =   'Pepper' _26410 =   'Pimento'
_26420  =  'Cloves' _26450 =   'Spices, Other' _26550 =   'Wine' _26560  =  'Beer' _26570  =  'Beverages, Fermented'
_26580 =   'Beverages, Alcoholic' _27310  =  'Bovine Meat' _27320  =  'Mutton & Goat Meat' _27330 =   'Pigmeat'
_27340 =   'Poultry Meat' _27350 =   'Meat, Other' _27360  =  'Offals, Edible' _27370  =  'Fats, Animals, Raw'
_27401 =   'Butter, Ghee' _27430 =   'Cream' _27440 =   'Eggs' _27450 =   'Honey' _27610  =  'Freshwater Fish'
_27620 =  'Demersal Fish' _27630 =   'Pelagic Fish' _27640 =    'Marine Fish, Other' _27650 =   'Crustaceans'
_27660 =   'Cephalopods' _27670 =   'Molluscs, Other'   _27690 =   'Aquatic Animals, Others'
_27750  =  'Aquatic Plants' _27810  =  'Fish, Body Oil' _27820   = 'Fish, Liver Oil' _28050  =  'Rice (Milled Equivalent)'
_28480  =  'Milk - Excluding Butter' _99970  =  'Palmkernals' _99980  =  'Cottonseed' _99990 =   'Rape and Mustard Seed';
run; *_27680 =   'Meat, Aquatic Mammals';

*Create calculated total of kcal_cap_d, mgZn_cap_d and mgphyt_cap_d based on individual food commodities;
data work.faofbs_krw2005;
set work.faofbs_krw2005;
calc_tot = SUM (OF _25112 -- _25202);
run;

proc sort data=work.faofbs_krw2005;
by continent_code continent region_code region country_code country iso_code imputeregion imputecountry year;
run;

proc transpose data=work.faofbs_krw2005 out = work.faofbs_krwyear;
by continent_code continent region_code region country_code country iso_code imputeregion imputecountry year;
id _name_;
var calc_tot;
run;


*****Merging pop_total data (from work.wpp2010_b) with faofbs_year dataset;
*Because right now FAOFBS_krwyear Kcal, Zn and Phytate are calculated on a per capita basis and we want to know how much
zinc is in the national food supply according to these calculations;


proc sort data=work.faofbs_krwyear;
by country_code year;
run;

proc sort data=work.wpp2010_b;
by country_code year;
run;

data work.faofbs_krwyear (rename = (_name_ = nut_cap_d));
merge work.faofbs_krwyear work.wpp2010_b (keep = country_code pop_tot1000 year);
by country_code year;
run;
*Now we need to merge the population numbers with the faofbs_krwyear dataset from above;

proc sort data=work.faofbs_krwyear;
by continent_code continent region_code region country_code country iso_code year;
run;

proc sort data=work.wpp2010_prb;
by continent_code continent region_code region country_code country iso_code year;
run;

data work.faofbs_krwyear;
merge work.faofbs_krwyear wpp2010_prb (keep=continent_code continent region_code region country_code country iso_code year 
PrZn_mean);
by continent_code continent region_code region country_code country iso_code year;
run;

*Calculation phytate:Zn ratios here now;
data work.faofbs_krwyear;
set work.faofbs_krwyear;
krw_P_Zn = (krw_mgphyt_cap_d/660)/(krw_mgZn_cap_d/65.4);
run;

   *** Application of Miller equation;

*Have kcal,zinc, and phytate values as a single observation;

/*Miller version 2010 (Hambidge et al. AJCN 2010;91 (suppl):1478s-83S*/
data work.faofbs_krwyear;
set work.faofbs_krwyear;
      amax2010 = .091;
      kr2010 = .033;
      kp2010 = .68;
	  run;

data work.faofbs_krwyear;
set work.faofbs_krwyear;
      krw_millernum2010 = amax2010+(krw_mgZn_cap_d/65.38)+kr2010*(1+(krw_mgphyt_cap_d/660.08)/kp2010);
      krw_absznmiller = 65.38*0.5*(krw_millernum2010-SQRT(krw_millernum2010**2-4*amax2010*(krw_mgZn_cap_d/65.38)));
      krw_fazmiller = krw_absznmiller/krw_mgZn_cap_d;
	run;



data work.faofbs_krwdef;
set work.faofbs_krwyear;
      krw_przn_earpctmiller = 100*krw_absznmiller/(przn_mean);
      krw_przn_pctdefmiller = 100*probnorm( (przn_mean-krw_absznmiller)/(.25*krw_absznmiller));
run;

*proc export data=work.faofbs_zndef
dbms = stata replace
label 
outfile = 'C:\Users\Ryan\Documents\FAO FBS 2011\FAOFBS_Zndef';
*run;




















