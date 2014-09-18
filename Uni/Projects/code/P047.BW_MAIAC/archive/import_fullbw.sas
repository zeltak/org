PROC IMPORT OUT= WORK.bw
            DATAFILE= "z:\Projects\P011.BirthW_NE\3.1.11.4.Work\2.Gather_data\FN001_BW_meta_Final\births_guid_meta0008_lu.dbf" 
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
            DATAFILE= "z:\Projects\P011.BirthW_NE\3.1.11.4.Work\2.Gather_data\FN001_BW_meta_Final\births_guid_2003_lu.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;

data  WORK.bw03;
set  WORK.bw03;
run; 

data  WORK.bw03;
set  WORK.bw03;
date=  bdob;
format date ddmmyy10.;
run;


data bwall;
set bw bw03;
run; 




/*rename variables*/



data bw5(drop= OBJECTID Join_Count TARGET_FID );
set bwall;
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

libname ses 'z:\Projects\P011.BirthW_NE\3.1.11.1.Raw_data\tract level SES\' ;

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


 

/*data for preterm analysis*/
data bwall;
set bw8;
/*where ges_calc >37;*/
if yrod ne 0 then delete;
/*if pmnewma3month < 0 then delete;*/
/*if pmnewmamonth < 0 then delete;*/
if ges_calc >=37 then bwcat=1;
else if ges_calc = 36 then  bwcat=2;
else if ges_calc =35 then bwcat=2;
else if ges_calc < 35 then bwcat=3;
if pctwhttr00=. then delete;
/*if diab_other =1 then delete;*/
dow=weekday(date);
   doy=put (date,julian5.);
   doy2=substr(doy,3,3);
   sinetime=sin(2*constant('pi')*doy2/365.25);
   costime=cos(2*constant('pi')*doy2/365.25);
   newvar=put(date, date9.);
udate = substr(newvar,1,5);
run; 


  LIBNAME wor 'c:\work';
  LIBNAME sasxpt XPORT 'c:\outdata\portable.xpt';
  PROC COPY IN=misc OUT=sasxpt;
    SELECT job1;
RUN;

/**/
/*In the example above:*/
/**/
/*    The first LIBNAME statement aliases the library reference (libref) misc to the ~/work directory.*/
/**/
/*    The second LIBNAME statement aliases the libref sasxpt with the physical name of the SAS transport format file (~/outdata/portable.xpt).*/
/**/
/*    The COPY procedure copies one or more SAS data sets in the IN= libref (in this case, misc) to the OUT= libref (in this case, sasxpt).*/
/**/
/*    The SELECT statement specifies that only the file job1.sas7bdat should be included in the transport file ~/outdata/portable.xpt. */
/**/
/*Note: The filenames and pathnames in the above example follow Unix conventions. If you use SAS for Windows, use the appropriate Windows filename and pathname conventions. For example, in SAS for Windows, the two LIBNAME statements in the above example would instead be:*/





libname db "z:\Projects\P011.BirthW_NE\3.1.11.4.Work\3.Analysis\4.sas analysis\" ;

data bwallv2;
set bwall;
keep bdob byob birthw ges_calc uniqueid_y clinega ;
run;

data db.bwallv2_short;
set bwallv2;
run;

PROC EXPORT DATA= bwallv2
            OUTFILE= "z:\Projects\P011.BirthW_NE\3.1.11.4.Work\3.Analysis\2.R_analysis\bwallv2.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;

 


