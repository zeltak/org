/*IMPORT AND FIX 2003*/

PROC IMPORT OUT= WORK.bw
            DATAFILE= "z:\Uni\Projects\P011.BirthW_NE\3.1.11.4.Work\2.Gather_data\FN001_BW_meta_Final\births_guid_meta0008_lu.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;

data  WORK.bw;
set  WORK.bw;
if byob=2003 then delete;
run; 

data  WORK.bw;
set  WORK.bw;
date =  bdob;
format date ddmmyy10.;
run;

PROC IMPORT OUT= WORK.bw03
            DATAFILE= "z:\Uni\Projects\P011.BirthW_NE\3.1.11.4.Work\2.Gather_data\FN001_BW_meta_Final\births_guid_2003_lu.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;

data bwall;
set bw bw03;
drop guid;
run; 


/*ADD EXPOSURE ID'S*/


PROC IMPORT OUT= bwid_guid
            DATAFILE= "z:\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\2.Gather_data\FN007_keytables\births_guid_stn.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


proc sort data = bwid_guid; by uniqueid_y   ;run;
proc sort data = bwall ; by uniqueid_y ;run;

data DATA3;
merge bwall(in=a) bwid_guid (in=b keep=uniqueid_y xx stn  yy)  ;
  by uniqueid_y;
    if a;
	run; 



data data34 (drop = xnym ynym xnymx ynymx);;;
set data3;
xnym=put(xx,Best12.);
   ynym=put(yy, Best12.);
   xnymx = xnym*10000;
   ynymx = ynym*10000;
   guid = compress(xnymx||ynymx);
run; 


/*sorting the 2 files*/

data data34;
set data34;
date=  bdob;
format date ddmmyy10.;
run; 

proc sort data=data34;
by guid date;
run;

libname exp 'z:\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\2.Gather_data\FN002_BW_exposure\' ;



proc sort data=exp.templag;
by guid date;
run;


data bw2;
merge data34 (in=a) exp.templag(in=b)  ;
  by guid date;
    if a;
	run; 




data bw4;
set bw2;
if fintemp=. then delete;
run;





/*rename variables*/



data bw5(drop= OBJECTID Join_Count TARGET_FID );
set bw4;
gender1=sex;
mother_race=mrace 	;
father_race=frace;
parity1=parit;
ges_calc1=gacalc;
ges_clinic1=clinega;
pre_vists1=npncv;
cig_pre1=cigdpp;
cig_preg1 =cigddp;
csect1=modpcs;
birthw=bwg;
utbleed1=rfutbld	;
renal1=rfrenal		;
lungd1  =rflungd 	;
hyper_other1=rfhypc 	;
hyper1=rfhyppr;
diab1 =rfdiabg	;
diab_other1=rfdiabo	;
prev_4001=rfpi4k	;
prevpre=rfpisga;
age=byob-myob;
med_income=medhhinctr;
p_ospace=pctreccono; 
fage1=      fage ;
frace1= frace ;
fethn1= fethn ;
fedu1= fedu ;
flangp1= flangp;
mlangp1= mlangp;
methn1=methn;
marstat1=marstat;
mbpstc1=mbpstc;
gravid1=gravid*1;
mpncp1=mpncp;
pncgov1=pncgov;
pncgov1=pncgov1*1;
methnic=methn*1;
fethnic=fethn1*1;
 run; 


 
  

/*Recode variables*/

data bw6 (drop= utbleed1 lungd1  renal1  hyper1 hyper_other1 diab1 cig_pre1 cig_preg1 diab_other1 prev_4001 prev_sga1 parity1 ges_calc1 ges_clinic1 pre_vists1 );
set bw5;

gender=gender1*1;

pre_vists=pre_vists1*1;

parity=parity1*1;
if parity=99 then prevpret=.;
if gravid1=99 then gravid1=.;



cig_pre=cig_pre1*1;
cig_preg=cig_preg1*1;
if cig_pre > 50 then cig_pre=.;
if cig_preg > 50 then cig_preg=.;

cig_preq = cig_pre*cig_pre;
cig_pregq = cig_preg*cig_preg;

if methnic=99 then methnic=.;
if fethnic=99 then fethnic=.;


ges_calc=ges_calc1*1;
ges_clinic=ges_clinic1*1;

 if ges_calc < 37 then lges=1;
 if ges_calc >= 37 then lges=0;
 if ges_clinic < 37 then lgescl=1;
  if ges_clinic >= 37 then lgescl=0;

prevpret=prevpre*1;
if prevpret=9 then prevpret=.;

if prevpret=2 then prevpret=0;
if prevpret=1 then prevpret=1;



if csect1=2 then csect=0;
if csect1=9 then csect=.;
if csect1=1 then csect=1;

if utbleed1=2 then utbleed=0;
if utbleed1=9 then utbleed=.;
if utbleed1=1 then utbleed=1;

if lungd1=2 then lungd=0;
if lungd1=9 then lungd=.;
if lungd1=1 then lungd=1;

if renal1=2 then renal=0;
if renal1=9 then renal=.;
if renal1=1 then renal=1;

if hyper_other1=2 then hyper_other=0;
if hyper_other1=9 then hyper_other=.;
if hyper_other1=1 then hyper_other=1;

if hyper1=2 then hyper=0;
if hyper1=9 then hyper=.;
if hyper1=1 then hyper=1;

if diab1=2 then diab=0;
if diab1=9 then diab=.;
if diab1=1 then diab=1;

if diab_other1=2 then diab_other=0;
if diab_other1=9 then diab_other=.;
if diab_other1=1 then diab_other=1;

if prev_4001=2 then prev_400=0;
if prev_4001=9 then prev_400=.;
if prev_4001=1 then prev_400=1;

if prev_sga1=2 then prev_sga=0;
if prev_sga1=9 then prev_sga=.;
if prev_sga1=1 then prev_sga=1;

 if mother_race="1" then MRN=0;
if mother_race="2" then MRN=1;
if mother_race="-" then MRN=2;
if mother_race="3" then MRN=2;
if mother_race="4" then MRN=2;
if mother_race="5" then MRN=2;
if mother_race="8" then MRN=2;
if mother_race="9" then MRN=2;


 if BIRTHW  <2500 then lbw=0;
 if BIRTHW  >= 2500 then lbw=1;

m = month(date); 
if (m=1 or m=2 or m=3 or m=10 or m=11 or m=12) then season=0; 
if (m=4 or m=5 or m=6 or m=7 or m=8 or m=9) then season=1; 

if BIRTHW  <2500 then lbw=1;
 if BIRTHW  >= 2500 then lbw=0;

 m = month(date); 
if (m=1 or m=2 or m=3 or m=10 or m=11 or m=12) then season=0; 
if (m=4 or m=5 or m=6 or m=7 or m=8 or m=9) then season=1; 


if parity=1 then npar=1;
 if parity=2 then npar=2;
 if parity=3 then npar=3;
 if parity=4 then npar=4;
 if parity=5 then npar=5;
 if parity=6 then npar=6;
 if parity=7 then npar=7;
 if parity=8 then npar=8;
 if parity > 8 then npar=9;


 year=year(date);

 if age > 80 then age=.;
 age_centered = age-29.85;
 age_centered_sq=age_centered*age_centered;

 if 0 < myredu <= 8 then edu_group=1;
 if 8 < myredu <= 12 then edu_group=2;
 if 12 < myredu <= 15 then edu_group=3;
 if myredu >15  then edu_group=4;
 
 adtmean=gridadt/1318000;

 med_incomeq=med_income*med_income;

 f_age=fage1*1;
if f_age > 80 then f_age=.;
f_age_centered = f_age-35.05;
 f_age_centered_sq=age_centered*age_centered;




f_race=frace1;

if f_race="1" then FRN=0;
if f_race="2" then FRN=1;
if f_race="-" then FRN=2;
if f_race="3" then FRN=2;
if f_race="4" then FRN=2;
if f_race="5" then FRN=2;
if f_race="8" then FRN=2;
if f_race="9" then FRN=2;






if  flangp1 = 1 then f_lang=0;
if  flangp1 ne 1 then f_lang=1;


if  mbpstc1 = "57" or mbpstc1 = "59"  then p_birth=0;
else  p_birth=1;


if  mlangp1 = 1 then m_lang=0;
if  mlangp1 ne 1 then m_lang=1;

if marstat1="1" then mstat=1;
if marstat1="2" then mstat=0;
if marstat1="3" then mstat=1;
if marstat1="9" then mstat=.;

m_care=mpncp1*1;
if m_care=99 then m_care=.;

bw=BIRTHW;

plural=plur*1;

if pncgov1=2 then pcare=0;
if pncgov1=9 then pcare=.;
if pncgov1=1 then pcare=1;

if age <= 20 then aged=1;
if  20 < age <=29  then aged=2;
if  29 < age <=34  then aged=3;
if  34 < age <=39  then aged=4;
if age > 39 then aged=5;
run; 




/*continue with dataset preperation*/

libname ses 'z:\Uni\Projects\P011.BirthW_NE\3.1.11.1.Raw_data\tract level SES\' ;

data tses;
set ses.tr00_clarcsumv3;
run; 

proc sort data = tses; by fips   ;run;
proc sort data = bw6 ; by fips ;run;

data bw7;
merge bw6(in=a) tses (in=b)  ;
  by fips;
    if a;
	run; 

data bw8;
set bw7;
if long =0 then delete;
run; 




data cwtc;
set exp.cwtc;
run;


proc sort data = cwtc; by date  ;run;
proc sort data =  bw8 ; by date ;run;

data  bw9;
merge  bw8(in=a) cwtc (in=b)  ;
  by date;
    if a;
	run; 


data tncdc;
set exp.tncdc;
run;


proc sort data = tncdc; by date stn  ;run;
proc sort data =  bw9 ; by date stn ;run;

data  bw10;
merge  bw9(in=a) tncdc (in=b)  ;
  by date stn;
    if a;
	run; 

data  bw10;
set  bw10;
dow=weekday(date);
   doy=put (date,julian5.);
   doy2=substr(doy,3,3);
   sinetime=sin(2*constant('pi')*doy2/365.25);
   costime=cos(2*constant('pi')*doy2/365.25);
   newvar=put(date, date9.);
udate = substr(newvar,1,5);
run; 



proc sort data = exp.devfin2; by udate guid  ;run;
proc sort data =  bw10 ; by udate guid ;run;

data  bw11;
merge  bw10(in=a) exp.devfin2 (in=b)  ;
  by udate guid;
    if a;
	run; 

data bw11;
set bw11;
findevtemp=fintemp-devtemp;
run; 


data bw12;
IQRfintemp         =fintemp          /  8.8846125  ;
IQRfintemp_l0      =fintemp_l0       /  8.8846125  ;
IQRfintemp_l1      =fintemp_l1       /  8.8922754  ;
IQRfintemp_l2      =fintemp_l2       /  8.9262760  ;
IQRfintemp_l3      =fintemp_l3       /  8.9559770  ;
IQRfintempma1      =fintempma1       /  8.8690465  ;
IQRfintempma3      =fintempma3       /  8.9217545  ;
IQRfintempmaweek   =fintempmaweek    /  9.0582818  ;
IQRfintempma2week  =fintempma2week   /  9.0600401  ;
IQRfintempma3week  =fintempma3week   /  9.0510531  ;
IQRfintempmamonth  =fintempmamonth   /  9.1899525  ;
IQRfintempma3month =fintempma3month  /  8.4217488  ;
IQRfintempmabirth  =fintempmabirth   /  2.6451869  ;
IQRtncdc         =tncdc              /      15.8888889;
IQRtncdc_l0      =tncdc_l0           /      15.8888889;
IQRtncdc_l1      =tncdc_l1           /      16.6111111;
IQRtncdc_l2      =tncdc_l2           /      16.2777778;
IQRtncdc_l3      =tncdc_l3           /      16.2222222;
IQRtncdcma1      =tncdcma1           /      16.2222222;
IQRtncdcma3      =tncdcma3           /      16.2222222;
IQRtncdcmaweek   =tncdcmaweek        /      16.1805556;
IQRtncdcma2week  =tncdcma2week       /      16.3629630;
IQRtncdcmamonth  =tncdcmamonth       /      16.2956989;
IQRtncdcma3month =tncdcma3month      /      16.6416361;
IQRtncdcmabirth  =tncdcmabirth       /      15.8567091;
set bw11;
run; 





libname bww 'z:\Uni\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\2.Gather_data\FN010_bwdatasets\' ;


data for preterm analysis
data bww.bw_noces;
set bw12;
if csect= 1 then delete;
if birthw < 700 then delete;
if plural =1;
where ges_calc >28;
if yrod ne 0 then delete;
if ges_calc >=37 then bwcat=1;
else if ges_calc = 36 then  bwcat=2;
else if ges_calc =35 then bwcat=2;
else if ges_calc < 35 then bwcat=3;
if pctwhttr00=. then delete;
run; 

data bww.bw_all;
set bw12;
/*if csect= 1 then delete;*/
if birthw < 700 then delete;
if plural =1;
if yrod ne 0 then delete;
run; 




PROC EXPORT DATA= bww.bw_all
            OUTFILE= "z:\Uni\Projects\P043_BirthW_Temp_MA\3.1.11.4.Work\3.Analysis\2.R_analysis\bw_all.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  
