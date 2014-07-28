
*POST NATURE MANUSCRIPT ANALYSES;
*Total of 3 models (2 dataset outputs;
*E3 data (with IZiNCG physiological requirements) and without rice (IZiNCG and IOM Physiological zinc
requirements are modeled post Monte Carlo simulation);
*Program modified by Ryan 2014-04-30;

/*proc printto log="nul:"; run;
DM 'ODSRESULTS' CLEAR EDITOR; ODS HTML CLOSE;
DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;*/




libname znN 'Z:\Uni\Projects\P007.Zinc\3.1.7.4.Work\2.Gather_data\NATURE DATA\Files\' ;
libname x 'Z:\Uni\Projects\P007.Zinc\3.1.7.4.Work\2.Gather_data\NATURE DATA\final' ;
OPTIONS nofmterr;

PROC IMPORT OUT= work.Final_Study_N
  DATAFILE= "Z:\Uni\Projects\P007.Zinc\3.1.7.4.Work\2.Gather_data\NATURE DATA\Final_Study_1_nature.xlsx"
    DBMS=xlsx REPLACE;
	sheet = SAS_finalstudy;
	  GETNAMES=YES;
			RUN;

data work.Final_Study_N;
set work.Final_Study_N;
run;


/*** Start ***/

%macro loop;

  %do i = 1 %to 2;

data work.Final_Study_N_&i;
 set work.Final_Study_N;
  Sim_slope = rand('NORMAL', Slope, Sl_SE);
  
run;

/****Changed xls document name to faren_coded_2_nature to reflect update post Nature manuscript;
PROC IMPORT OUT= work.Crop_Name
  DATAFILE= "G:\Dropbox\Myers CO2 UCDavis HSPH\3.1.7.4.Work\2.Gather_data\NATURE DATA\faren_coded_2_nature.xls" 
    DBMS=xls REPLACE;
	sheet = crop_name_nature;
	  GETNAMES=YES;
			RUN;*/

****Changed xls document name to faren_coded_2_nature to reflect update post Nature manuscript;
PROC IMPORT OUT= Crop_Name
  DATAFILE= "C:\Users\Ryan\Dropbox\P007.Zinc\3.1.7.4.Work\2.Gather_data\NATURE DATA\faren_coded_2_nature.xls" 
    DBMS=xls REPLACE;
	sheet = crop_name_nature;
	  GETNAMES=YES;
			RUN;

data work.Crop_Name;
set work.Crop_Name;
if item = 'DUMMY' then delete;
run;

data work.Crop_Name (drop = keep);
set work.Crop_Name;
  if Keep = . then delete;
run;





/*Models with be as follows:
Zn_PFS_WR - primary + FACE + chamber data, with rice (E3)
Zn_PFS_NR - primary + FACE + chamber data, no rice (E3)
Phy_PFS_WR - primary data, with rice
Phy_PFS_NR - primary data, no rice*/




data work.Final_Study_N_&i;
set work.Final_Study_N_&i;
  Zn_PFS_WR = Model;
  Zn_PFS_NR = Model;
  Phy_PFS_WR = Model;
  Phy_PFS_NR = Model;
run; 


/*** Model Zn_PFS_WR***/

proc sort data = work.Final_Study_N_&i; by Zn_PFS_WR; run;
proc sort data = work.Crop_Name;        by Zn_PFS_WR; run;

data work.Zn_PFS_WR_&i(drop = Zn_Prim_WR--Zn_PF_NR Zn_PFS_NR--Phy_PFS_NRFV);
 merge work.Crop_Name (in=a) work.Final_Study_N_&i(in=b);
  by Zn_PFS_WR;
   if a and b;
run;

data work.Zn_PFS_WR_&i;
 set work.Zn_PFS_WR_&i;
  count = _n_ ;
run;

/*** Model Zn_PFS_NR***/

proc sort data = work.Final_Study_N_&i; by Zn_PFS_NR; run;
proc sort data = work.Crop_Name;        by Zn_PFS_NR; run;

data work.Zn_PFS_NR_&i(drop = Zn_Prim_WR--Zn_PFS_WR Zn_PFS_NRFV--Phy_PFS_NRFV);
 merge work.Crop_Name (in=a) work.Final_Study_N_&i(in=b);
  by Zn_PFS_NR;
   if a and b;
run;

data work.Zn_PFS_NR_&i;
 set work.Zn_PFS_NR_&i;
  count = _n_ ;
run;

/*** Model Phy_PFS_WR***/

proc sort data = work.Final_Study_N_&i; by Phy_PFS_WR; run;
proc sort data = work.Crop_Name;        by Phy_PFS_WR; run;

data work.Phy_PFS_WR_&i(drop = Zn_Prim_WR--Phy_PF_NR Phy_PFS_NR--Phy_PFS_NRFV);
 merge work.Crop_Name (in=a) work.Final_Study_N_&i(in=b);
  by Phy_PFS_WR;
   if a and b;
run;

data work.Phy_PFS_WR_&i;
 set work.Phy_PFS_WR_&i;
  count = _n_ ;
run;

/*** Model Phy_PFS_NR***/

proc sort data = work.Final_Study_N_&i; by Phy_PFS_NR; run;
proc sort data = work.Crop_Name;        by Phy_PFS_NR; run;

data work.Phy_PFS_NR_&i(drop = Zn_Prim_WR--Phy_PFS_WR Phy_PFS_NRFV);
 merge work.Crop_Name (in=a) work.Final_Study_N_&i(in=b);
  by Phy_PFS_NR;
   if a and b;
run;

data work.Phy_PFS_NR_&i;
 set work.Phy_PFS_NR_&i;
  count = _n_ ;
run;


proc datasets lib = work;
 delete Final_Study_N_&i;  
run;

%end;

%mend;

%loop;



%macro picked;

  %do i = 1 %to 2;

/** Primary + FACE+ chamber data, with rice (PFS_WR) **/

/* Zinc */

data work.Zn_PFS_WR_&i(keep = ItemCode1 Item kcalrczn kcalrcphyt PFS_WR_Zn_Pct kcalrczn_PFS_WR);
 set work.Zn_PFS_WR_&i;
  PFS_WR_Zn_Pct = ((exp(Sim_slope)-1)*100);

    if Check = "OK" then kcalrczn_PFS_WR = kcalrczn*((100+PFS_WR_Zn_Pct)/100);
    if Check = "NO" then kcalrczn_PFS_WR = kcalrczn;
run;

/*Phy */

data work.Phy_PFS_WR_&i(keep = ItemCode1 Item kcalrczn kcalrcphyt PFS_WR_Phy_Pct kcalrcphy_PFS_WR);
 set work.Phy_PFS_WR_&i;
  PFS_WR_Phy_Pct = ((exp(Sim_slope)-1)*100);

   if Check = "OK" then kcalrcphy_PFS_WR = kcalrcphyt*((100 + PFS_WR_Phy_Pct)/100);
   if Check = "NO" then kcalrcphy_PFS_WR = kcalrcphyt;
run;

/*Merge*/
proc sort data = work.Zn_PFS_WR_&i; by ItemCode1 ;run;
proc sort data = work.Phy_PFS_WR_&i; by ItemCode1 ;run;

data work.Ran_PFS_WR_&i;
merge work.Zn_PFS_WR_&i(in=a) work.Phy_PFS_WR_&i(in=b);
  by ItemCode1;
run;  

proc datasets lib = work;   delete Zn_PFS_WR_&i Phy_PFS_WR_&i; run; 





/** Primary data + FACE + Chamber data, no rice (PFS_NR) **/

/*Zinc */

data work.Zn_PFS_NR_&i(keep = ItemCode1 Item kcalrczn kcalrcphyt PFS_NR_Zn_Pct kcalrczn_PFS_NR);
 set work.Zn_PFS_NR_&i;
 PFS_NR_Zn_Pct = ((exp(Sim_slope)-1)*100);

    if Check = "OK" then kcalrczn_PFS_NR = kcalrczn*((100+PFS_NR_Zn_Pct)/100);
    if Check = "NO" then kcalrczn_PFS_NR = kcalrczn;
run;

/* Phy */

data work.Phy_PFS_NR_&i(keep = ItemCode1 Item kcalrczn kcalrcphyt PFS_NR_Phy_Pct kcalrcphy_PFS_NR);
 set work.Phy_PFS_NR_&i;
  PFS_NR_Phy_Pct = ((exp(Sim_slope)-1)*100);

   if Check = "OK" then kcalrcphy_PFS_NR = kcalrcphyt*((100 + PFS_NR_Phy_Pct)/100);
   if Check = "NO" then kcalrcphy_PFS_NR = kcalrcphyt;
run;

/*Merge*/
proc sort data = work.Zn_PFS_NR_&i; by ItemCode1 ;run;
proc sort data = work.Phy_PFS_NR_&i; by ItemCode1 ;run;

data work.Ran_PFS_NR_&i;
merge work.Zn_PFS_NR_&i(in=a) work.Phy_PFS_NR_&i(in=b);
  by ItemCode1;
run;  

proc datasets lib = work;   delete Zn_PFS_NR_&i Phy_PFS_NR_&i; run; 

%end;

%MEND ;

%picked;




*********************;
*Import necessary datasets for the analysis;
*Import FAOFBS_T;


/* import datafile = 'G:\Dropbox\Myers CO2 UCDavis HSPH\3.1.7.4.Work\2.Gather_data\NATURE DATA\FAOFBS_T_121004.dta'
 OUT= work.faofbs_t
  dbms = stata REPLACE;
run;*/


proc import datafile = "C:\Users\Ryan\Dropbox\P007.Zinc\3.1.7.4.Work\2.Gather_data\NATURE DATA\FAOFBS_T_121004.dta"
 OUT= work.faofbs_t
  dbms = stata REPLACE;
run;



proc sort data = work.faofbs_t; by ItemCode1  ;run; 



	
/*PROC IMPORT OUT= work.znpymis
  DATAFILE= "G:\Dropbox\Myers CO2 UCDavis HSPH\3.1.7.4.Work\2.Gather_data\NATURE DATA\zn_phy_missing.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;*/


		PROC IMPORT OUT= work.znpymis
  DATAFILE= "C:\Users\Ryan\Dropbox\P007.Zinc\3.1.7.4.Work\2.Gather_data\NATURE DATA\zn_phy_missing.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
	

options mprint;
%macro picked;

  %do i = 1 %to 2;


***Results of PFS_WR;
proc sort data = work.faofbs_t;           by ItemCode1 ;run;
proc sort data = work.Ran_PFS_WR_&i;   by ItemCode1 ;run;

data work.Results_PFS_WR_&i(drop=kcalrczn kcalrcphyt);
merge work.faofbs_t(in=a) work.Ran_PFS_WR_&i(in=b);
  by ItemCode1;
    if a;
run; 


proc sort data = work.Results_PFS_WR_&i; by ItemCode1   ;run;
proc sort data = work.znpymis ; by ItemCode1 ;run;

data work.Results_PFS_WR_&i;
merge work.Results_PFS_WR_&i(in=a) work.znpymis (in=b)  ;
  by ItemCode1;
    if a;
	run; 


***Results of PFS_NR;
proc sort data = work.faofbs_t;           by ItemCode1 ;run;
proc sort data = work.Ran_PFS_NR_&i;   by ItemCode1 ;run;

data work.Results_PFS_NR_&i(drop=kcalrczn kcalrcphyt);
merge work.faofbs_t(in=a) work.Ran_PFS_NR_&i(in=b);
  by ItemCode1;
    if a;
run; 


proc sort data = work.Results_PFS_NR_&i; by ItemCode1   ;run;
proc sort data = work.znpymis ; by ItemCode1 ;run;

data work.Results_PFS_NR_&i;
merge work.Results_PFS_NR_&i(in=a) work.znpymis (in=b)  ;
  by ItemCode1;
    if a;
	run; 



/*** Replace Original values of kcalrczn and kcalrcphy when the adj is missing ****/


data work.Results_PFS_WR_&i;
 set work.Results_PFS_WR_&i;
  if kcalrczn_PFS_WR   = . then kcalrczn_PFS_WR  = kcalrczn;
  if kcalrcphy_PFS_WR  = . then kcalrcphy_PFS_WR = kcalrcphyt;
run;

data work.Results_PFS_NR_&i;
 set work.Results_PFS_NR_&i;
  if kcalrczn_PFS_NR = . then kcalrczn_PFS_NR  = kcalrczn;
  if kcalrcphy_PFS_NR  = . then kcalrcphy_PFS_NR = kcalrcphyt;
run;


%end;

%MEND ;

%picked;



options mprint;

%macro picked;
  %do i = 1 %to 2;
*********************Now we need to calculate Zn and phytate intake based on extraction and processing estimates;

*need to do these calculations using Myers data (kcalrczn_PFS_WR and kcalrcphy_PFS_WR based on CO2 changes);
data work.faofbs_PFS_WR_krw_&i;
set work.Results_PFS_WR_&i;

  iteration = &i;
  if itemcode1 = 25112 then do;

  mgZn_100kcal = (kcalrczn_PFS_WR)*(Extract1)*(Extr_Zn) + (kcalrczn_PFS_WR)*(1-Extract1); 
  mgPhyt_100kcal = (kcalrcphy_PFS_WR)*(Extract1)*(Extr_Phyt)*(Ferment1)*(Ferm_Phyt) + (kcalrcphy_PFS_WR)*(Extract1)*(Extr_Phyt)*(1-Ferment1) +
                   (kcalrcphy_PFS_WR)*(1-Extract1); 
  end; *Wheat; *Whole grain wheat is NEVER fermented;

  else if itemcode1 = 25170 or itemcode1 = 25180 or itemcode1 = 25141 then do;  

     mgZn_100kcal = (kcalrczn_PFS_WR)*(Extract1)*(Extr_Zn) + (kcalrczn_PFS_WR)*(1-Extract1); 
     mgPhyt_100kcal = (kcalrcphy_PFS_WR)*(Extract1)*(Extr_Phyt)*(Ferment1)*(Ferm_Phyt) + (kcalrcphy_PFS_WR)*(Extract1)*(Extr_Phyt)*(1-Ferment1) + 
                      (kcalrcphy_PFS_WR)*(1-Extract1)*(Ferment1)*(Ferm_Phyt) + (kcalrcphy_PFS_WR)*(1-Extract1)*(1-Ferment1); 
  end; *Millet and sorghum and maize; 

  *Both whole grain and extracted millet and sorghum can be fermented;
  else if itemcode1 = 25230 or itemcode1 = 25340 or itemcode1 = 25202 then do; 
          mgZn_100kcal = kcalrczn_PFS_WR; mgPhyt_100kcal = (kcalrcphy_PFS_WR)*(Ferment1)*(Ferm_Phyt) + (kcalrcphy_PFS_WR)*(1-Ferment1); 
  end; *Cassava and other roots, there is no effect on zinc so only effects on phytate due to fermentation, there is no extraction that affects phytate;

  else if itemcode1 = 25460 or itemcode1 = 25470 or itemcode1 = 25490 or itemcode1 = 25550 then do; 
      mgZn_100kcal = kcalrczn_PFS_WR; mgPhyt_100kcal = (kcalrcphy_PFS_WR)*(Extr_phyt); 
  end; *This is for beans; 

  else do; mgZn_100kcal = kcalrczn_PFS_WR; mgPhyt_100kcal = kcalrcphy_PFS_WR; 
  end; *This is for cereals other and all other itemcodes that do not have extraction or processing estimates applied to them;
run;


%end;

%MEND ;

%picked;





options mprint;

%macro picked;
  %do i = 1 %to 2;
*********************Now we need to calculate Zn and phytate intake based on extraction and processing estimates;

*need to do these calculations using Myers data (kcalrczn_PFS_NR and kcalrcphy_PFS_NR based on CO2 changes);
data work.faofbs_PFS_NR_krw_&i;
set work.Results_PFS_NR_&i;

  iteration = &i;
  if itemcode1 = 25112 then do;

  mgZn_100kcal = (kcalrczn_PFS_NR)*(Extract1)*(Extr_Zn) + (kcalrczn_PFS_NR)*(1-Extract1); 
  mgPhyt_100kcal = (kcalrcphy_PFS_NR)*(Extract1)*(Extr_Phyt)*(Ferment1)*(Ferm_Phyt) + (kcalrcphy_PFS_NR)*(Extract1)*(Extr_Phyt)*(1-Ferment1) +
                   (kcalrcphy_PFS_NR)*(1-Extract1); 
  end; *Wheat; *Whole grain wheat is NEVER fermented;

  else if itemcode1 = 25170 or itemcode1 = 25180 or itemcode1 = 25141 then do;  

     mgZn_100kcal = (kcalrczn_PFS_NR)*(Extract1)*(Extr_Zn) + (kcalrczn_PFS_NR)*(1-Extract1); 
     mgPhyt_100kcal = (kcalrcphy_PFS_NR)*(Extract1)*(Extr_Phyt)*(Ferment1)*(Ferm_Phyt) + (kcalrcphy_PFS_NR)*(Extract1)*(Extr_Phyt)*(1-Ferment1) + 
                      (kcalrcphy_PFS_NR)*(1-Extract1)*(Ferment1)*(Ferm_Phyt) + (kcalrcphy_PFS_NR)*(1-Extract1)*(1-Ferment1); 
  end; *Millet and sorghum and maize; 

  *Both whole grain and extracted millet and sorghum can be fermented;
  else if itemcode1 = 25230 or itemcode1 = 25340 or itemcode1 = 25202 then do; 
          mgZn_100kcal = kcalrczn_PFS_NR; mgPhyt_100kcal = (kcalrcphy_PFS_NR)*(Ferment1)*(Ferm_Phyt) + (kcalrcphy_PFS_NR)*(1-Ferment1); 
  end; *Cassava and other roots, there is no effect on zinc so only effects on phytate due to fermentation, there is no extraction that affects phytate;

  else if itemcode1 = 25460 or itemcode1 = 25470 or itemcode1 = 25490 or itemcode1 = 25550 then do; 
      mgZn_100kcal = kcalrczn_PFS_NR; mgPhyt_100kcal = (kcalrcphy_PFS_NR)*(Extr_phyt); 
  end; *This is for beans; 

  else do; mgZn_100kcal = kcalrczn_PFS_NR; mgPhyt_100kcal = kcalrcphy_PFS_NR; 
  end; *This is for cereals other and all other itemcodes that do not have extraction or processing estimates applied to them;
run;


%end;

%MEND ;

%picked;


/*** Combine Dataset by Iteration ***/

options mprint;
%macro picked;

  %do i = 1 %to 2;


proc append base=work.Final_PFS_WR data=work.faofbs_PFS_WR_krw_&i; run;

proc append base=work.Final_PFS_NR data=work.faofbs_PFS_NR_krw_&i; run;


proc datasets lib = work; delete faofbs_PFS_WR_krw_&i faofbs_PFS_NR_krw_&i ; 
run; 


%end;

%MEND ;

%picked;






/*proc import datafile = 'G:\Dropbox\Myers CO2 UCDavis HSPH\3.1.7.4.Work\2.Gather_data\NATURE DATA\summ_ic_kcal.xls' 
 OUT = work.krw_nutrient
 dbms = xls REPLACE;
  sheet = krw_nutrient;
run;
       
proc import datafile = 'G:\Dropbox\Myers CO2 UCDavis HSPH\3.1.7.4.Work\2.Gather_data\NATURE DATA\FAOFBS_wpp2010.xlsx'
 OUT= work.wpp2010_b
  dbms = xlsx REPLACE;
   sheet = wpp2010_b;
run;
proc import datafile = 'G:\Dropbox\Myers CO2 UCDavis HSPH\3.1.7.4.Work\2.Gather_data\NATURE DATA\FAOFBS_wpp2010.xlsx'
 OUT= work.wpp2010_znreq
  dbms = xlsx REPLACE;
   sheet = wpp2010_znreq;
run;*/




proc import datafile = "C:\Users\Ryan\Dropbox\P007.Zinc\3.1.7.4.Work\2.Gather_data\NATURE DATA\summ_ic_kcal.xls"
 OUT = work.krw_nutrient
 dbms = xls REPLACE;
  sheet = krw_nutrient;
run;
       
proc import datafile = "C:\Users\Ryan\Dropbox\P007.Zinc\3.1.7.4.Work\2.Gather_data\NATURE DATA\FAOFBS_wpp2010.xlsx"
 OUT= work.wpp2010_b
  dbms = xlsx REPLACE;
   sheet = wpp2010_b;
run;
proc import datafile ="C:\Users\Ryan\Dropbox\P007.Zinc\3.1.7.4.Work\2.Gather_data\NATURE DATA\FAOFBS_wpp2010.xlsx"
 OUT= work.wpp2010_znreq
  dbms = xlsx REPLACE;
   sheet = wpp2010_znreq;
run;










OPTIONS nofmterr;

ods listing close;*to suppress the output printing;
/*options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;*/

options notes source source2 MLOGIC MPRINT MRECALL SYMBOLGEN errors=0; *suppresses LOG WINDOW printing;


***Model PFS_WR**************************************************************************;

proc sort data = work.Final_PFS_WR; by itemcode1; run;

proc univariate data = work.Final_PFS_WR;
class country_code name_of_former_variable_1;
 var mgZn_100kcal;
  output out = work.out1zn_PFS_WR(keep = itemcode1 country_code name_of_former_variable_1 P_2_5 P_50 P_97_5) pctlpre=P_ pctlpts= 0 to 100 by 2.5;
   by itemcode1;
run;

data work.out1_PFS_WR_zn(drop = P_2_5 P_50 P_97_5);
 set work.out1zn_PFS_WR; 
  P_2_5_zn  = P_2_5;
  P_50_zn   = P_50; 
  P_97_5_zn = P_97_5;
run;


proc univariate data = work.Final_PFS_WR;
class country_code name_of_former_variable_1;
 var mgPhyt_100kcal;
  output out = work.out1phy_PFS_WR(keep = itemcode1 country_code name_of_former_variable_1 P_2_5 P_50 P_97_5) pctlpre=P_ pctlpts= 0 to 100 by 2.5;
   by itemcode1;
run;

data work.out1_PFS_WR_phy(drop = P_2_5 P_50 P_97_5);
 set work.out1phy_PFS_WR; 
  P_2_5_phy   = P_2_5;
  P_50_phy    = P_50; 
  P_97_5_phy  = P_97_5;
run;


proc sort data = work.out1_PFS_WR_phy; by country_code name_of_former_variable_1 itemcode1 ;run;
proc sort data = work.out1_PFS_WR_zn ; by country_code name_of_former_variable_1 itemcode1 ;run;

data work.Out1_Final_PFS_WR;
merge work.out1_PFS_WR_zn(in=a) work.out1_PFS_WR_phy (in=b)  ;
  by   country_code name_of_former_variable_1 itemcode1;
run;


data work.Variable(drop = iteration -- mgPhyt_100kcal);
 set work.Final_PFS_WR;
  if iteration = 1;
run;


proc sort data = work.Variable;         by country_code name_of_former_variable_1 itemcode1 ;run;
proc sort data = work.Out1_Final_PFS_WR; by country_code name_of_former_variable_1 itemcode1 ;run;

data work.Out1_Final_PFS_WR;
merge work.Variable(in=a) work.Out1_Final_PFS_WR (in=b)  ;
  by   country_code name_of_former_variable_1 itemcode1;
run;




data work.Out1_Final_PFS_WR;
set work.Out1_Final_PFS_WR;

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


data work.faofbs_krw_WR;
 set work.Out1_Final_PFS_WR;
 
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


data work.faofbs_krw_WR (rename =(name_of_former_variable_1 = year));
 set work.faofbs_krw_WR ;
run; 

proc sort data=work.faofbs_krw_WR;
by continent_code continent region_code region country_code country iso_code
   itemcode1 itemcodeB itemcode;
run; 

proc summary data=work.faofbs_krw_WR;
 var  kcal_cap_d kcalrczn kcalrcphyt extract1 extract2 extr_zn extr_phyt ferment1 ferm_phyt

krw_mgZn_cd_25    krw_mgZn_cap_d    krw_mgZn_cd_97_5   krw_mgZn_cd_org
krw_mgPhyt_cd_25  krw_mgPhyt_cap_d  krw_mgPhyt_cd_97_5 krw_mgPhyt_cd_org 

P_2_5_zn  P_50_zn P_97_5_zn
P_2_5_phy P_50_phy P_97_5_phy;

  by continent_code continent region_code region country_code country iso_code itemcode1 itemcodeB itemcode;

where year IN ('y2003', 'y2004', 'y2005', 'y2006', 'y2007');

output out=work.faofbs_krw2005a_WR mean = ;

run;

***Now transpose data to get foods as variables and kcal_cap_d, mgZn_cap_d and mgphyt_cap_d on separate lines, sorted by 
country;



proc sort data=work.faofbs_krw2005a_WR;
by continent_code continent region_code region country_code country iso_code  ;
run;

*Transpose data ;
proc transpose data=work.faofbs_krw2005a_WR out = work.faofbs_krw2005_WR;
by continent_code continent region_code region country_code country iso_code ;
id itemcode1;
var krw_mgZn_cd_25    krw_mgZn_cap_d    krw_mgZn_cd_97_5   krw_mgZn_cd_org
    krw_mgPhyt_cd_25  krw_mgPhyt_cap_d  krw_mgPhyt_cd_97_5 krw_mgPhyt_cd_org kcal_cap_d;
run;
 
*Create calculated total of kcal_cap_d, mgZn_cap_d and mgphyt_cap_d based on individual food commodities;
data work.faofbs_krw2005_WR;
set work.faofbs_krw2005_WR;
calc_tot = SUM (OF _25112 -- _25202);
run;

proc sort data=work.faofbs_krw2005_WR;
by continent_code continent region_code region country_code country iso_code  ;
run;

proc transpose data=work.faofbs_krw2005_WR out = work.faofbs_krwyear_WR;
by continent_code continent region_code region country_code country iso_code ;
id _name_;
var calc_tot;
run;


*****Merging pop_total data (from work.wpp2010_b) with faofbs_year dataset;
*Because right now FAOFBS_krwyear Kcal, Zn and Phytate are calculated on a per capita basis and we want to know how much
zinc is in the national food supply according to these calculations;


proc sort data=work.faofbs_krwyear_WR;
by country_code ;
run;

proc sort data=work.wpp2010_b ;
by country_code ;
run;

data work.faofbs_krwyear_WR (rename = (_name_ = nut_cap_d));
merge work.faofbs_krwyear_WR work.wpp2010_b (keep = country_code pop_tot1000 );
by country_code ;
run;
*Now we need to merge the population numbers with the faofbs_krwyear dataset from above;

proc sort data=work.faofbs_krwyear_WR;
by continent_code continent region_code region country_code country iso_code ;
run;

proc sort data=work.wpp2010_znreq;
by continent_code continent region_code region country_code country iso_code ;
run;

data work.faofbs_krwyear_WR;
merge work.faofbs_krwyear_WR work.wpp2010_znreq (keep=continent_code continent region_code region country_code country iso_code 
PrZn_mean);
by continent_code continent region_code region country_code country iso_code ;
run;




*Calculation phytate:Zn ratios here now;
data work.faofbs_krwyear_WR;
set work.faofbs_krwyear_WR;

krw_P25_Zn   = (krw_mgPhyt_cd_25/660)/(krw_mgZn_cd_25 /65.4);
krw_P_Zn     = (krw_mgphyt_cap_d/660)/(krw_mgZn_cap_d/65.4);
krw_P97_5_Zn = (krw_mgPhyt_cd_97_5/660)/(krw_mgZn_cd_97_5  /65.4);

krw_P_Zn_org  = (krw_mgphyt_cd_org/660)/(krw_mgZn_cd_org/65.4);

run;

*** Application of Miller equation;

*Have kcal,zinc, and phytate values as a single observation;

/*Miller version 2010 (Hambidge et al. AJCN 2010;91 (suppl):1478s-83S*/

data work.faofbs_krwyear_WR;
set work.faofbs_krwyear_WR;
      amax2010 = .091;
      kr2010 = .033;
      kp2010 = .68;
run;

data work.faofbs_krwyear_WR;
set work.faofbs_krwyear_WR;

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



data work.faofbs_krwdef_izincg_WR;
set work.faofbs_krwyear_WR;

      krw_przn_earpctmiller_25 = 100*krw_absznmiller_25/(przn_mean);
      krw_przn_earpctmiller = 100*krw_absznmiller/(przn_mean);
      krw_przn_earpctmiller_97_5 = 100*krw_absznmiller_97_5/(przn_mean);
	  krw_przn_earpctmiller_org = 100*krw_absznmiller_org/(przn_mean);

      krw_przn_pctdefmiller_25 = 100*probnorm((przn_mean-krw_absznmiller_25)/(.25*krw_absznmiller_25));
      krw_przn_pctdefmiller = 100*probnorm((przn_mean-krw_absznmiller)/(.25*krw_absznmiller));
      krw_przn_pctdefmiller_97_5 = 100*probnorm((przn_mean-krw_absznmiller_97_5)/(.25*krw_absznmiller_97_5));
      krw_przn_pctdefmiller_org = 100*probnorm((przn_mean-krw_absznmiller_org)/(.25*krw_absznmiller_org));

run;




data work.faofbs_krwdef_PFS_WR;
set  work.faofbs_krwdef_izincg_WR;
run; 

proc datasets lib = work;   delete faofbs_krwdef_izincg_WR; run; 


***Model PFS_NR***********************************************************************************************;

proc sort data = work.Final_PFS_nr; by itemcode1; run;

proc univariate data = work.Final_PFS_nr;
class country_code name_of_former_variable_1;
 var mgZn_100kcal;
  output out = work.out1zn_PFS_nr(keep = itemcode1 country_code name_of_former_variable_1 P_2_5 P_50 P_97_5) pctlpre=P_ pctlpts= 0 to 100 by 2.5;
   by itemcode1;
run;

data work.out1_PFS_nr_zn(drop = P_2_5 P_50 P_97_5);
 set work.out1zn_PFS_nr; 
  P_2_5_zn  = P_2_5;
  P_50_zn   = P_50; 
  P_97_5_zn = P_97_5;
run;


proc univariate data = work.Final_PFS_nr;
class country_code name_of_former_variable_1;
 var mgPhyt_100kcal;
  output out = work.out1phy_PFS_nr(keep = itemcode1 country_code name_of_former_variable_1 P_2_5 P_50 P_97_5) pctlpre=P_ pctlpts= 0 to 100 by 2.5;
   by itemcode1;
run;

data work.out1_PFS_nr_phy(drop = P_2_5 P_50 P_97_5);
 set work.out1phy_PFS_nr; 
  P_2_5_phy   = P_2_5;
  P_50_phy    = P_50; 
  P_97_5_phy  = P_97_5;
run;


proc sort data = work.out1_PFS_nr_phy; by country_code name_of_former_variable_1 itemcode1 ;run;
proc sort data = work.out1_PFS_nr_zn ; by country_code name_of_former_variable_1 itemcode1 ;run;

data work.Out1_Final_PFS_nr;
merge work.out1_PFS_nr_zn(in=a) work.out1_PFS_nr_phy (in=b)  ;
  by   country_code name_of_former_variable_1 itemcode1;
run;


data work.Variable(drop = iteration -- mgPhyt_100kcal);
 set work.Final_PFS_nr;
  if iteration = 1;
run;


proc sort data = work.Variable;         by country_code name_of_former_variable_1 itemcode1 ;run;
proc sort data = work.Out1_Final_PFS_nr; by country_code name_of_former_variable_1 itemcode1 ;run;

data work.Out1_Final_PFS_nr;
merge work.Variable(in=a) work.Out1_Final_PFS_nr (in=b)  ;
  by   country_code name_of_former_variable_1 itemcode1;
run;




data work.Out1_Final_PFS_nr;
set work.Out1_Final_PFS_nr;

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


data work.faofbs_krw_nr;
 set work.Out1_Final_PFS_nr;
 
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


data work.faofbs_krw_nr (rename =(name_of_former_variable_1 = year));
 set work.faofbs_krw_nr ;
run; 

proc sort data=work.faofbs_krw_nr;
by continent_code continent region_code region country_code country iso_code
   itemcode1 itemcodeB itemcode;
run; 

proc summary data=work.faofbs_krw_nr;
 var  kcal_cap_d kcalrczn kcalrcphyt extract1 extract2 extr_zn extr_phyt ferment1 ferm_phyt

krw_mgZn_cd_25    krw_mgZn_cap_d    krw_mgZn_cd_97_5   krw_mgZn_cd_org
krw_mgPhyt_cd_25  krw_mgPhyt_cap_d  krw_mgPhyt_cd_97_5 krw_mgPhyt_cd_org 

P_2_5_zn  P_50_zn P_97_5_zn
P_2_5_phy P_50_phy P_97_5_phy;

  by continent_code continent region_code region country_code country iso_code itemcode1 itemcodeB itemcode;

where year IN ('y2003', 'y2004', 'y2005', 'y2006', 'y2007');

output out=work.faofbs_krw2005a_nr mean = ;

run;

***Now transpose data to get foods as variables and kcal_cap_d, mgZn_cap_d and mgphyt_cap_d on separate lines, sorted by 
country;



proc sort data=work.faofbs_krw2005a_nr;
by continent_code continent region_code region country_code country iso_code  ;
run;

*Transpose data ;
proc transpose data=work.faofbs_krw2005a_nr out = work.faofbs_krw2005_nr;
by continent_code continent region_code region country_code country iso_code ;
id itemcode1;
var krw_mgZn_cd_25    krw_mgZn_cap_d    krw_mgZn_cd_97_5   krw_mgZn_cd_org
    krw_mgPhyt_cd_25  krw_mgPhyt_cap_d  krw_mgPhyt_cd_97_5 krw_mgPhyt_cd_org kcal_cap_d;
run;
 
*Create calculated total of kcal_cap_d, mgZn_cap_d and mgphyt_cap_d based on individual food commodities;
data work.faofbs_krw2005_nr;
set work.faofbs_krw2005_nr;
calc_tot = SUM (OF _25112 -- _25202);
run;

proc sort data=work.faofbs_krw2005_nr;
by continent_code continent region_code region country_code country iso_code  ;
run;

proc transpose data=work.faofbs_krw2005_nr out = work.faofbs_krwyear_nr;
by continent_code continent region_code region country_code country iso_code ;
id _name_;
var calc_tot;
run;


*****Merging pop_total data (from work.wpp2010_b) with faofbs_year dataset;
*Because right now FAOFBS_krwyear Kcal, Zn and Phytate are calculated on a per capita basis and we want to know how much
zinc is in the national food supply according to these calculations;


proc sort data=work.faofbs_krwyear_nr;
by country_code ;
run;

proc sort data=work.wpp2010_b ;
by country_code ;
run;

data work.faofbs_krwyear_nr (rename = (_name_ = nut_cap_d));
merge work.faofbs_krwyear_nr work.wpp2010_b (keep = country_code pop_tot1000 );
by country_code ;
run;
*Now we need to merge the population numbers with the faofbs_krwyear dataset from above;

proc sort data=work.faofbs_krwyear_nr;
by continent_code continent region_code region country_code country iso_code ;
run;

proc sort data=work.wpp2010_znreq;
by continent_code continent region_code region country_code country iso_code ;
run;

data work.faofbs_krwyear_nr;
merge work.faofbs_krwyear_nr work.wpp2010_znreq (keep=continent_code continent region_code region country_code country iso_code 
PrZn_mean);
by continent_code continent region_code region country_code country iso_code ;
run;




*Calculation phytate:Zn ratios here now;
data work.faofbs_krwyear_nr;
set work.faofbs_krwyear_nr;

krw_P25_Zn   = (krw_mgPhyt_cd_25/660)/(krw_mgZn_cd_25 /65.4);
krw_P_Zn     = (krw_mgphyt_cap_d/660)/(krw_mgZn_cap_d/65.4);
krw_P97_5_Zn = (krw_mgPhyt_cd_97_5/660)/(krw_mgZn_cd_97_5  /65.4);

krw_P_Zn_org  = (krw_mgphyt_cd_org/660)/(krw_mgZn_cd_org/65.4);

run;

*** Application of Miller equation;

*Have kcal,zinc, and phytate values as a single observation;

/*Miller version 2010 (Hambidge et al. AJCN 2010;91 (suppl):1478s-83S*/

data work.faofbs_krwyear_nr;
set work.faofbs_krwyear_nr;
      amax2010 = .091;
      kr2010 = .033;
      kp2010 = .68;
run;

data work.faofbs_krwyear_nr;
set work.faofbs_krwyear_nr;

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

data work.faofbs_krwdef_izincg_NR;
set work.faofbs_krwyear_NR;

      krw_przn_earpctmiller_25 = 100*krw_absznmiller_25/(przn_mean);
      krw_przn_earpctmiller = 100*krw_absznmiller/(przn_mean);
      krw_przn_earpctmiller_97_5 = 100*krw_absznmiller_97_5/(przn_mean);
	  krw_przn_earpctmiller_org = 100*krw_absznmiller_org/(przn_mean);

      krw_przn_pctdefmiller_25 = 100*probnorm((przn_mean-krw_absznmiller_25)/(.25*krw_absznmiller_25));
      krw_przn_pctdefmiller = 100*probnorm((przn_mean-krw_absznmiller)/(.25*krw_absznmiller));
      krw_przn_pctdefmiller_97_5 = 100*probnorm((przn_mean-krw_absznmiller_97_5)/(.25*krw_absznmiller_97_5));
      krw_przn_pctdefmiller_org = 100*probnorm((przn_mean-krw_absznmiller_org)/(.25*krw_absznmiller_org));

	  krw_iomprzn_earpctmiller_25 = 100*krw_absznmiller_25/(iomprzn_mean);
      krw_iomprzn_earpctmiller = 100*krw_absznmiller/(iomprzn_mean);
      krw_iomprzn_earpctmiller_97_5 = 100*krw_absznmiller_97_5/(iomprzn_mean);
	  krw_iomprzn_earpctmiller_org = 100*krw_absznmiller_org/(iomprzn_mean);

      krw_iomprzn_pctdefmiller_25 = 100*probnorm((iomprzn_mean-krw_absznmiller_25)/(.25*krw_absznmiller_25));
      krw_iomprzn_pctdefmiller = 100*probnorm((iomprzn_mean-krw_absznmiller)/(.25*krw_absznmiller));
      krw_iomprzn_pctdefmiller_97_5 = 100*probnorm((iomprzn_mean-krw_absznmiller_97_5)/(.25*krw_absznmiller_97_5));
      krw_iomprzn_pctdefmiller_org = 100*probnorm((iomprzn_mean-krw_absznmiller_org)/(.25*krw_absznmiller_org));

run;

data work.faofbs_krwdef_PFS_NR;
set  work.faofbs_krwdef_izincg_NR;
run;

proc datasets lib = work;   delete faofbs_krwdef_izincg_R; run; 
quit;
