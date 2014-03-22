
PROC IMPORT OUT= WORK.nasguid
            DATAFILE= "f:\Uni\Projects\3.1.6.NAS\3.1.6.4.Work\2.Gather_data\FN030_tempe_address_guid\nas_temp_guid.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;




/*import nas addresses*/

data add;
set nasguid;
AddStartDate=AddStartDa;
SeasonStartMos=SeasonStar;
SeasonEndMos=SeasonEndM;
keep nasid addid AddStartDate AddEndDate SeasonStartMos SeasonEndMos;
run;  

proc sort data = add; by addid   ;run; 

/*populate all available dates*/

 data add_days;
  set add;
  by addid;
  retain date;
  if first.addid then do;
    date=mdy(12,31,1999); 
	  do while (date <= mdy(12,31,2011));
	    date=date+1;
	    output;
	  end;
	end;
  format date mmddyy10.;
run;


/*for each subject id create the dates the subject was at that address*/

data add_days2;
set add_days;
where AddStartDate <= date and AddEndDate > date;
run; 


/*address seasonality*/

data add_days2x;
set add_days2;
m=month(date);
d=day(date);
ms=substr(SeasonStartMos,1,2);
ms2=ms*1;
ds=substr(SeasonStartMos,3,2);
ds2=ds*1;
me=substr(SeasonEndMos,1,2);
me2=me*1;
de=substr(SeasonEndMos,3,2);
de2=de*1;
drop ms ds me de;
run; 


data add_days2y;
set add_days2x;
if ms2 ne . and m = ms2 and d <= ds2 then delete;
if me2 ne . and m  = me2 and d >= de2 then delete;
if ms2 ne . and ms2< me2 and m < ms2 then delete;
if me2 ne . and ms2< me2 and m > me2 then delete;
if ms2 ne . and ms2 >me2 and m < ms2 and m > me2 then delete ;
run; 


proc sort data = add_days2y nodupkey out=add_days2z;
by nasid date; 
run;


/*load exposure dataset*/


libname exp 'f:\Uni\Projects\3.1.6.NAS\3.1.6.4.Work\2.Gather_data\FN031_exposure\' ;

 
 
data expo;
set exp.tempNAS;
drop OBJECTID--TARGET_FID Join_Cou_1--TARGET_F_1;
run; 



proc sort data = expo; by addid date   ;run;
proc sort data = add_days2z ; by addid date ;run;

data add_days3;
merge add_days2z(in=a) expo (in=b keep=addid date pmnew  )  ;
  by addid date;
    if a;
	run; 


%macro makelags(fname,pol);

proc sort data = &fname; by nasid date;run; 
 data &pol; set  &fname;by nasid;
  &pol._l0=&pol;
%local i;
 %do i=0 %to 365;
  &pol._l%eval(&i+1)=lag1(&pol._l&i);
   if first.nasid then &pol._l%eval(&i+1)=.;
 %end;
 &pol.ma1 = mean(&pol._l0,&pol._l1);
 &pol.ma3 = mean(of &pol._l0 - &pol._l2);
 &pol.ma4 = mean(of &pol._l0 - &pol._l3);
 &pol.ma5 = mean(of &pol._l0 - &pol._l4);
 &pol.ma6 = mean(of &pol._l0 - &pol._l5);
 &pol.ma7 = mean(of &pol._l0 - &pol._l6);
 &pol.maweek = mean(of &pol._l0 - &pol._l7);
 &pol.ma10 = mean(of &pol._l0 - &pol._l9);
 &pol.ma2week = mean(of &pol._l0 - &pol._l14);
 &pol.ma3week = mean(of &pol._l0 - &pol._l21);
 &pol.mamonth = mean(of &pol._l0 - &pol._l30);
 &pol.ma2month = mean(of &pol._l0 - &pol._l60);
 &pol.ma3month = mean(of &pol._l0 - &pol._l90);
 &pol.ma6month = mean(of &pol._l0 - &pol._l182);
 &pol.mayear = mean(of &pol._l0 - &pol._l365);
 run;
%mend;

%makelags(add_days3,pmnew);
%makelags(add_days3,temp_f);
%makelags(add_days3,ah_gm3_F);


/*save complete file*/

libname exx 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.6.NAS\3.1.6.4.Work\3.Analysis\AN003_FULL_NAS_EXPOSURE\' ;


data exx.nasall_pm;
set pmnew;
run; 





/*shrink dataset*/


data pmnew_short;
set pmnew;
keep nasid addid date guid 
pmnew
pmnewma1 
pmnewma3 
pmnewma4 
pmnewma5 
pmnewma6 
pmnewma7 
pmnewmaweek 
pmnewma10 
pmnewma2week 
pmnewma3week 
pmnewmamonth 
pmnewma2month
pmnewma3month
pmnewma6month
pmnewmayear 
pmnew;
run; 

data ah_gm3_F_short;
set ah_gm3_F;
keep nasid addid date guid 
ah_gm3_F
ah_gm3_Fma1 
ah_gm3_Fma3 
ah_gm3_Fma4 
ah_gm3_Fma5 
ah_gm3_Fma6 
ah_gm3_Fma7 
ah_gm3_Fmaweek 
ah_gm3_Fma10 
ah_gm3_Fma2week 
ah_gm3_Fma3week 
ah_gm3_Fmamonth 
ah_gm3_Fma2month
ah_gm3_Fma3month
ah_gm3_Fma6month
ah_gm3_Fmayear
run; 


data temp_f_short;
set temp_f;
keep nasid addid date guid 
temp_f
temp_fma1 
temp_fma3 
temp_fma4 
temp_fma5 
temp_fma6 
temp_fma7 
temp_fmaweek 
temp_fma10 
temp_fma2week 
temp_fma3week 
temp_fmamonth 
temp_fma2month
temp_fma3month
temp_fma6month
temp_fmayear;
run; 




proc sort data = pmnew_short; by addid date   ;run;
proc sort data = ah_gm3_F_short ; by addid date ;run;
proc sort data = temp_f_short ; by addid date ;run;

data DATA3;
merge pmnew_short(in=a) ah_gm3_F_short (in=b) temp_f_short (in=c) ;
  by addid date;
    if a;
	run; 




/*convert nasid to numeric*/

data pmnew_short2;
set DATA3;
nasid2=nasid*1;
drop nasid;
run; 

data pmnew_short3;
set pmnew_short2;
nasid=nasid2;
drop nasid2;
run; 



/*FLAG NOT 100 COMPLETE CASES*/

data pmnew_shortZ;
  set pmnew_short3;
 count=1;
 run;

 proc summary  data=pmnew_shortz;
 class nasid ;
 var count;
 output out=pmnew_shortZ2 sum=countsum;
 run; 


data pmnew_shortZ3;
set pmnew_shortZ2;
if countsum <1000 then flag1=1;
run; 


data DATA3 (drop=nasid);
set DATA3;
nasid1=nasid*1;
run; 

data DATA3(drop=nasid1);
set DATA3;
nasid=nasid1;
run; 

proc sort data = pmnew_shortZ3; by nasid   ;run;
proc sort data = DATA3 ; by  nasid ;run;

data  pmnew_short_fin;
merge Data3(in=a) pmnew_shortZ3 (in=b keep= nasid flag1)  ;
  by  nasid;
    if a;
	run; 


/*	load full nas dataset*/

	libname nas 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.6.NAS\3.1.6.1.Raw_data\ORIG_NAS_2012\' ;


/*subset only for >2000*/

data nas (where=(date>='01MAR2000'D and date<='31DEC2011'D)) ;
set nas.nas;
addid=addid1;
nasid=id;
datevisit=date;
format datevisit  mmddyy8.;
addchange=addenddate1;
format addchange  mmddyy8.;
if liveOutNE=1 then delete;
run; 


data nas_short;
set nas;
keep id date addid1 addStartDate1 addEndDate1 mainAddr1 liveOutNE addLat1 addLong1 addID2 addStartDate2 addEndDate2 addid nasid datevisit addchange;
if liveOutNE=1 then delete;
run; 


proc sort data = pmnew_short_fin; by nasid date   ;run;
proc sort data = nas_short ; by nasid date ;run;

data nas_short_pm;
merge nas_short(in=a) Pmnew_short_fin(in=b)  ;
  by nasid date;
    if a;
	run; 


data nas_short_pm_clean;
set nas_short_pm;
if pmnewmayear = . then delete;
run; 

/*export final 10x10 area pm for all nas*/



libname fin 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.6.NAS\3.1.6.4.Work\2.Gather_data\FN020_Final_NAS_POLL\' ;


data fin.Final_NAS_POLL;
set nas_short_pm_clean;
run; 




/*add local PM*/




PROC IMPORT OUT= naslpm
  DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.6.NAS\3.1.6.4.Work\2.Gather_data\FN010_localPM_predictions\nas_lpm.csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;




proc sort data = naslpm; by AddID   ;run;
proc sort data = nas_short_pm_clean ; by AddID ;run;

data  nas_short_pm_clean_lpm;
merge nas_short_pm_clean(in=a) naslpm (in=b)  ;
  by AddID;
    if a;
	run; 




data fin.Final_NAS_POLL_lpm;
set nas_short_pm_clean_lpm;
run; 


proc sort data = nas_short_pm_clean_lpm; by date   ;run; 
