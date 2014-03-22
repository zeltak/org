PROC IMPORT OUT= WORK.nas_resid_poll 
            DATAFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Proj
etcs\3.1.6.NAS\3.1.6.4.Work\3.Analysis\MB_analysis\mb_expo.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;


data nas_base;
set nas_resid_poll ;
drop PODEN10--smk2;
run; 



libname fin 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.6.NAS\3.1.6.4.Work\2.Gather_data\FN020_Final_NAS_POLL\' ;


data poll;
set fin.final_nas_poll_lpm_bc;
drop addid2 addid1 addid flag1 var1;
run; 



proc sort data = poll; by id date   ;run;
proc sort data = nas_base ; by id date ;run;

data DATA3;
merge nas_base(in=a) poll (in=b)  ;
  by id date;
    if a;
	run;

data DATA4;
set  DATA3;
     if smk=1 then smk2=0;
else if smk=3 then smk2=2;
else if smk=4 then smk2=1;
sin=sin(2*3.14159265*date/365.24);
cos=cos(2*3.14159265*date/365.24);
	 run;


data DATA5;
set DATA4;
if pmnew=. then delete;
logicam=log(icam);
logvcam=log(vcam);
logfib=log(fib);
logcrp=log(crp);
run; 


data  DATA6;
set  DATA5;
if logicam= . then delete;
if logvcam= . then delete;
if logfib= . then delete;
if logcrp= . then delete;
run;


data data7;
set data6;
resid1d=pmnewma1-BCpred_1Day_lag1;
resid3d=pmnewma3-BCpred_1Day_lag3;
resid1w=pmnewmaweek-((BCpred_1Day+BCpred_1Day_lag1+BCpred_1Day_lag2+BCpred_1Day_lag3+BCpred_1Day_lag4+BCpred_1Day_lag5+BCpred_1Day_lag6)/7);
resid2w= pmnewma2week-BCpred_2Wk;
resid3w= pmnewma3week-BCpred_3Wk;
resid4w=pmnewmamonth-BCpred_4Wk;
resid8w=pmnewma2month-BCpred_8Wk;
resid12w=pmnewma3month-BCpred_12Wk;
resdi1yr=pmnewmayear-BCpred_1Yr;
if resdi1yr =. then delete;
run; 

PROC EXPORT DATA= WORK.data6 
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.6.NAS\3.1.6.4.Work\3.Analysis\MB_analysis\mb_expo.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  
PROC EXPORT DATA= WORK.data7 
            OUTFILE= "C:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.6.NAS\3.1.6.4.Work\3.Analysis\MB_analysis\mb_expo_bc.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  
