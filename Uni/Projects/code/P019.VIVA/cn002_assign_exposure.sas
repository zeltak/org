libname add 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.6.NAS\3.1.6.1.Raw_data\ORIG_NAS_2012\' ;

/*import viva addresses*/

data add;
set add.adr_nas;
keep nasid addid AddStartDate AddEndDate SeasonStartMos SeasonEndMos LiveOutNE;
if LiveOutNE=1 then delete;
run;  

proc sort data = add; by addid   ;run; 



/*populate all available dates*/

 data add_days;
  set add;
  by addid;
  retain date;
  if first.addid then do;
    date=mdy(12,31,1999); 
	  do while (date <= mdy(12,31,2008));
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



libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.6.NAS\3.1.6.4.Work\3.Analysis\AN003_FULL_NAS_EXPOSURE\' ;

data expo;
set aod.nas_expo;
drop  addlat addlong  lat_aod_x long_aod_x;
run; 



proc sort data = expo; by addid date   ;run;
proc sort data = add_days2z ; by addid date ;run;

data add_days3;
merge add_days2z(in=a) expo (in=b keep=addid date pmnew )  ;
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
 &pol.maweek = mean(of &pol._l0 - &pol._l7);
 &pol.ma2week = mean(of &pol._l0 - &pol._l14);
 &pol.mamonth = mean(of &pol._l0 - &pol._l30);
 &pol.ma3month = mean(of &pol._l0 - &pol._l90);
 &pol.mabirth = mean(of &pol._l0 - &pol._l280);
 &pol.mayear = mean(of &pol._l0 - &pol._l365);
 run;
%mend;

%makelags(add_days3,pmnew);
/*%makelags(add_days3,temp_f);*/


/*save complete file*/

libname exx 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.6.NAS\3.1.6.4.Work\3.Analysis\AN003_FULL_NAS_EXPOSURE\' ;


data exx.nasall_pm;
set pmnew;
run; 





/*shrink dataset*/


data pmnew_short;
set pmnew;
keep nasid addid date guid long_AOD_X lat_AOD_x pmnew pmnew_l0 pmnew_l1 pmnew_l2 pmnew_l3 pmnewma1 pmnewma3 pmnewmaweek pmnewma2week pmnewmamonth pmnewma3month  pmnewmabirth pmnewmayear;
run; 


/*convert nasid to numeric*/

data pmnew_short2;
set pmnew_short;
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


data pmnew_short (drop=nasid);
set pmnew_short;
nasid1=nasid*1;
run; 

data pmnew_short(drop=nasid1);
set pmnew_short;
nasid=nasid1;
run; 

proc sort data = pmnew_shortZ3; by nasid   ;run;
proc sort data = pmnew_short ; by  nasid ;run;

data  pmnew_short_fin;
merge pmnew_short(in=a) pmnew_shortZ3 (in=b keep= nasid flag1)  ;
  by  nasid;
    if a;
	run; 


/*	load full nas dataset*/

	libname nas 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.6.NAS\3.1.6.1.Raw_data\ORIG_NAS_2012\' ;


/*subset only for >2000*/

data nas (where=(date>='01MAR2000'D and date<='31DEC2008'D)) ;
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
    DBMS=CSV REPLAC;
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

