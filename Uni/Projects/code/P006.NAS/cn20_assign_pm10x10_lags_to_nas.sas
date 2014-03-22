
PROC IMPORT OUT= WORK.nasguid
              DATAFILE= "f:\Uni\Projects\3.1.6.NAS\3.1.6.4.Work\2.Gather_data\FN007_key\nas_PM10x10guid.dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;



/*import nas addresses*/

data add;
set nasguid;
AddStartDate=AddStartDa;
SeasonStartMos=SeasonStar;
SeasonEndMos=SeasonEndM;
keep guid nasid addid AddStartDate AddEndDate SeasonStartMos SeasonEndMos;
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


libname exp 'f:\Uni\Projects\3.1.6.NAS\3.1.6.4.Work\2.Gather_data\FN029_pm10_expo\' ;

 
  proc sort data = exp.tall nodupkey out=exp.tall;
 by date guid;
 run;
 


data add_days2z  (where=(date>="01MAR2000"D and date<="31DEC2011"D )) ;
set add_days2z;
run; 


proc sort data = exp.tall; by guid date   ;run;
proc sort data = add_days2z ; by guid date ;run;

data add_days3;
merge add_days2z(in=a) exp.tall (in=b  )  ;
  by guid date;
    if a;
	run; 


proc means data=add_days3 n min max mean std nmiss;
var pmnew; 
run; 





data add_days3x;
set add_days3;
if pmnew=. then delete;
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
 &pol.ma48h = mean(&pol._l1,&pol._l2);
 &pol.ma3 = mean(of &pol._l0 - &pol._l2);
 &pol.ma4 = mean(of &pol._l0 - &pol._l3);
 &pol.ma5 = mean(of &pol._l0 - &pol._l4);
 &pol.ma6 = mean(of &pol._l0 - &pol._l5);
 &pol.ma7 = mean(of &pol._l0 - &pol._l6);
 &pol.maweek = mean(of &pol._l0 - &pol._l6);
 &pol.ma10 = mean(of &pol._l0 - &pol._l9);
 &pol.ma2week = mean(of &pol._l0 - &pol._l13);
 &pol.ma3week = mean(of &pol._l0 - &pol._l20);
 &pol.mamonth = mean(of &pol._l0 - &pol._l30);
 &pol.ma2month = mean(of &pol._l0 - &pol._l60);
 &pol.ma3month = mean(of &pol._l0 - &pol._l90);
 &pol.ma6month = mean(of &pol._l0 - &pol._l182);
 &pol.mayear = mean(of &pol._l0 - &pol._l365);
 run;
%mend;

%makelags(add_days3x,pmnew);



data pmnew_s;
set pmnew;
drop pmnew_l21--pmnew_l366;
run; 


DATA fin.pmnew_s;
SET pmnew_s;
RUN; 





/*convert nasid to numeric*/

data pmnew_short2;
set  pmnew_s;
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


data pmnew_s (drop=nasid);
set pmnew_s;
nasid1=nasid*1;
run; 

data pmnew_s(drop=nasid1);
set pmnew_s;
nasid=nasid1;
run; 

proc sort data = pmnew_shortZ3; by nasid   ;run;
proc sort data = pmnew_s ; by  nasid ;run;

data  pmnew_short_fin;
merge pmnew_s(in=a) pmnew_shortZ3 (in=b keep= nasid flag1)  ;
  by  nasid;
    if a;
	run; 


/*	load full nas dataset*/

	libname nas 'f:\Uni\Projects\3.1.6.NAS\3.1.6.1.Raw_data\ORIG_NAS_2013\' ;


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

data nas_short_temp;
merge nas_short(in=a) pmnew_short_fin(in=b)  ;
  by nasid date;
    if a;
	run; 

proc means data= nas_short_temp n  nmiss;
var; 
run; 


data nas_short_temp_clean;
set nas_short_temp;
if pmnew = . then delete;
run; 

/*export final 10x10 area pm for all nas*/



libname fin 'f:\Uni\Projects\3.1.6.NAS\3.1.6.4.Work\2.Gather_data\FN040_pm10x10_final\' ;


data fin.Final_NAS_pm10x10;
set nas_short_temp_clean;
run; 


