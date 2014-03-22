
		 
PROC IMPORT OUT= WORK.cases
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.19.VIVA\3.1.6.4.Work\2.Gather_data\FN009_localPM\nviva_guid_lpm.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


proc sort data = cases; by aid_ct  ;run; 

data casesd;
  set cases;
  by aid_ct;
  retain date;
  if first.aid_ct then do;
    date=mdy(03,1,2000);

          do while (date <= mdy(12,31,2008));
        /*do while (date <= mdy(3,31,1999));*/

                /*hrint = 0;
                        do while  (hrint < 24);
                                output;
                                hrint = hrint+1;
                        end;*/
                        output;
                        date=date+1;
          end;
        end;
format date mmddyy10.;
run;



libname poll 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\3.Analysis\AN040_Lags\' ;



 data poll;
set poll.poll_lag_v5  ;
 run; 


 proc sort data = casesd; by guid date  ;run;
 proc sort data = poll ; by guid date ;run;

 data DATA3;
 merge casesd(in=a) poll (in=b)  ;
   by guid date;
   if a;
	 run; 



data DATA31;
set DATA3;
if aid_ct="" then delete;
pm25predstart_date=bcpredstar;
pm25predstop=bcpredstop;
run; 





proc sort data = DATA31; by aid_ct date   ;run; 

data data4;
set Data31;
where date>=pm25predstart_date and date <=pm25predstop;
aid=SUBSTR(aid_ct,1,6);
run; 



proc means data=data4 n min max mean std nmiss sum;
var aid; 
run; 

libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.19.VIVA\3.1.6.4.Work\2.Gather_data\FN020_Final_NAS_POLL\' ;



 
data aod.data4;
set data4;
run; 



%macro makelags(dir,fname,pol);
 data work.&pol;	set work.&fname;
 	&pol.l0=&pol;
/*Check if first observation for subject*/
prevaid = lag1(aid);
if aid NE prevaid then firstdate=datepred;
%local i;
	%do i=0 %to 365;
		&pol.l%eval(&i+1)=lag1(&pol.l&i);
		/*for first observation for a subject set &pol.1%eval(&i+1) to . */
		if date = firstdate then &pol.l%eval(&i+1) = .;
	%end;
	&pol.ma2 = mean(&pol.l0,&pol.l1);	
	*&pol._n1 = n(of &pol.l0 - &pol.l3);
	*if &pol._n1 lt 1 then &pol.ma1 = .; 

	&pol.ma3 = mean(of &pol.l0 - &pol.l2);
	&pol._n2 = n(of &pol.l0 - &pol.l2);
	if &pol._n2 lt 2 then &pol.ma3 = .; 

	&pol.ma4 = mean(of &pol.l0 - &pol.l3);
	&pol._n3 = n(of &pol.l0 - &pol.l3);
	if &pol._n3 lt 3 then &pol.ma4 = .; 

	&pol.ma5 = mean(of &pol.l0 - &pol.l4);
	&pol._n4 = n(of &pol.l0 - &pol.l4);
	if &pol._n4 lt 3 then &pol.ma5 = .; 

	&pol.ma6 = mean(of &pol.l0 - &pol.l5);
	&pol._n5 = n(of &pol.l0 - &pol.l5);
	if &pol._n5 lt 4 then &pol.ma6 = .; 

	&pol.ma7 = mean(of &pol.l0 - &pol.l6);
	&pol._n6 = n(of &pol.l0 - &pol.l6);
	if &pol._n6 lt 5 then &pol.ma7 = .; 

	&pol.ma8 = mean(of &pol.l0 - &pol.l7);
	&pol._n7 = n(of &pol.l0 - &pol.l7);
	if &pol._n7 lt 6 then &pol.ma8 = .; 

	&pol.ma9 = mean(of &pol.l0 - &pol.l8);
	&pol._n8 = n(of &pol.l0 - &pol.l8);
	if &pol._n8 lt 6 then &pol.ma9 = .; 

	&pol.ma10 = mean(of &pol.l0 - &pol.l9);
	&pol._n9 = n(of &pol.l0 - &pol.l9); 
	if &pol._n9 lt 7 then &pol.ma10 = .; 

	&pol.ma11 = mean(of &pol.l0 - &pol.l10);
	&pol._n10 = n(of &pol.l0 - &pol.l10);
	if &pol._n10 lt 8 then &pol.ma11 = .; 

	&pol.ma12 = mean(of &pol.l0 - &pol.l11);
	&pol._n11 = n(of &pol.l0 - &pol.l11);
	if &pol._n11 lt 9 then &pol.ma12 = .; 

	&pol.ma13 = mean(of &pol.l0 - &pol.l12);
	&pol._n12 = n(of &pol.l0 - &pol.l12);
	if &pol._n12 lt 9 then &pol.ma13 = .; 

	&pol.ma14 = mean(of &pol.l0 - &pol.l13);
	&pol._n13 = n(of &pol.l0 - &pol.l13);
	if &pol._n13 lt 10 then &pol.ma14 = .; 

	&pol.ma15 = mean(of &pol.l0 - &pol.l14);
	&pol._n14 = n(of &pol.l0 - &pol.l14);
	if &pol._n14 lt 11 then &pol.ma15 = .; 

	&pol.ma16 = mean(of &pol.l0 - &pol.l15);
	&pol._n15 = n(of &pol.l0 - &pol.l15);
	if &pol._n15 lt 12 then &pol.ma16 = .; 

	&pol.ma17 = mean(of &pol.l0 - &pol.l16);
	&pol._n16 = n(of &pol.l0 - &pol.l16);
	if &pol._n16 lt 12 then &pol.ma17 = .; 

	&pol.ma18 = mean(of &pol.l0 - &pol.l17);
	&pol._n17 = n(of &pol.l0 - &pol.l17);
	if &pol._n17 lt 13 then &pol.ma18 = .; 

	&pol.ma19 = mean(of &pol.l0 - &pol.l18);
	&pol._n18 = n(of &pol.l0 - &pol.l18);
	if &pol._n18 lt 14 then &pol.ma19 = .; 

	&pol.ma20 = mean(of &pol.l0 - &pol.l19);
	&pol._n19 = n(of &pol.l0 - &pol.l19);
	if &pol._n19 lt 15 then &pol.ma20 = .; 

	&pol.ma21 = mean(of &pol.l0 - &pol.l20);
	&pol._n20 = n(of &pol.l0 - &pol.l20);
	if &pol._n20 lt 16 then &pol.ma21 = .; 

	&pol.ma22 = mean(of &pol.l0 - &pol.l21);
	&pol._n21 = n(of &pol.l0 - &pol.l21);
	if &pol._n21 lt 16 then &pol.ma22 = .; 

	&pol.ma23 = mean(of &pol.l0 - &pol.l22);
	&pol._n22 = n(of &pol.l0 - &pol.l22);
	if &pol._n22 lt 17 then &pol.ma23 = .; 

	&pol.ma24 = mean(of &pol.l0 - &pol.l23);
	&pol._n23 = n(of &pol.l0 - &pol.l23);
	if &pol._n23 lt 18 then &pol.ma24 = .; 

	&pol.ma25 = mean(of &pol.l0 - &pol.l24);
	&pol._n24 = n(of &pol.l0 - &pol.l24);
	if &pol._n24 lt 19 then &pol.ma25 = .; 

	&pol.ma26 = mean(of &pol.l0 - &pol.l25);
	&pol._n25 = n(of &pol.l0 - &pol.l25);
	if &pol._n25 lt 19 then &pol.ma26 = .; 

	&pol.ma27 = mean(of &pol.l0 - &pol.l26);
	&pol._n26 = n(of &pol.l0 - &pol.l26);
	if &pol._n26 lt 20 then &pol.ma27 = .; 

	&pol.ma28 = mean(of &pol.l0 - &pol.l27);
	&pol._n27 = n(of &pol.l0 - &pol.l27);
	if &pol._n27 lt 21 then &pol.ma28 = .; 

	&pol.ma29 = mean(of &pol.l0 - &pol.l28);
	&pol._n28 = n(of &pol.l0 - &pol.l28);
	if &pol._n28 lt 22 then &pol.ma29 = .; 

	&pol.ma30 = mean(of &pol.l0 - &pol.l29);
	&pol._n29 = n(of &pol.l0 - &pol.l29);
	if &pol._n29 lt 22 then &pol.ma30 = .; 

	&pol.ma365 = mean(of &pol.l0 - &pol.l364);
	&pol._n364 = n(of &pol.l0 - &pol.l364);
	if &pol._n364 lt 274 then &pol.ma365 = .; 

	keep aid date &pol.l1-&pol.l30 &pol.ma2-&pol.ma30
	&pol.ma10 &pol.ma11 &pol.ma12 &pol.ma13 &pol.ma14 
	&pol.ma15 &pol.ma16 &pol.ma17 &pol.ma18 &pol.ma19
	&pol.ma20 &pol.ma21 &pol.ma22 &pol.ma23 &pol.ma24 
	&pol.ma25 &pol.ma26 &pol.ma27 &pol.ma28 &pol.ma29
	&pol.ma30 &pol.ma365 &pol ;
	run;
%mend;


/*HERE'S WHERE MAKELAGS MACRO IS CALLED*/



%makelags(work,DATA4,pmnew);

data pm25movingavg;
set pmnew;
rename pmnewl1=pm25d1;
rename pmnewl2=pm25d2;
rename pmnewl3=pm25d3;
rename pmnewl4=pm25d4;
rename pmnewl5=pm25d5;
rename pmnewl6=pm25d6; 
rename pmnewl7=pm25d7; 
rename pmnewl8=pm25d8; 
rename pmnewl9=pm25d9; 
rename pmnewl10=pm25d10; 
rename pmnewl11=pm25d11; 
rename pmnewl12=pm25d12;
rename pmnewl13=pm25d13; 
rename pmnewl14=pm25d14;
rename pmnewl15=pm25d15;
rename pmnewl16=pm25d16; 
rename pmnewl17=pm25d17; 
rename pmnewl18=pm25d18; 
rename pmnewl19=pm25d19; 
rename pmnewl20=pm25d20; 
rename pmnewl21=pm25d21; 
rename pmnewl22=pm25d22;
rename pmnewl23=pm25d23; 
rename pmnewl24=pm25d24;
rename pmnewl25=pm25d25; 
rename pmnewl26=pm25d26;
rename pmnewl27=pm25d27; 
rename pmnewl28=pm25d28;
rename pmnewl29=pm25d29; 
rename pmnewl30=pm25d30;
rename pmnewma2=pm25ma2;
rename pmnewma3=pm25ma3;
rename pmnewma4=pm25ma4;
rename pmnewma5=pm25ma5;
rename pmnewma6=pm25ma6; 
rename pmnewma7=pm25ma7; 
rename pmnewma8=pm25ma8; 
rename pmnewma9=pm25ma9; 
rename pmnewma10=pm25ma10; 
rename pmnewma11=pm25ma11; 
rename pmnewma12=pm25ma12;
rename pmnewma13=pm25ma13; 
rename pmnewma14=pm25ma14;
rename pmnewma15=pm25ma15;
rename pmnewma16=pm25ma16; 
rename pmnewma17=pm25ma17; 
rename pmnewma18=pm25ma18; 
rename pmnewma19=pm25ma19; 
rename pmnewma20=pm25ma20; 
rename pmnewma21=pm25ma21; 
rename pmnewma22=pm25ma22;
rename pmnewma23=pm25ma23; 
rename pmnewma24=pm25ma24;
rename pmnewma25=pm25ma25; 
rename pmnewma26=pm25ma26;
rename pmnewma27=pm25ma27; 
rename pmnewma28=pm25ma28;
rename pmnewma29=pm25ma29; 
rename pmnewma30=pm25ma30;
rename pmnewma365=pm25ma365;
run;

data pm25movingavg (keep =aid pm25_24hr date pm25d1 pm25d2 pm25d3 pm25d4 pm25d5 pm25d6 pm25ma2 pm25ma3 
pm25ma4 pm25ma5 pm25ma6 pm25ma7 pm25ma14
 pm25ma30 pm25ma365);
set pm25movingavg;
label
pm25d1="pm25,1 day lag" 
pm25d2="pm25,2 day lag" 
pm25d3="pm25,3 day lag" 
pm25d4="pm25,4 day lag" 
pm25d5="pm25,5 day lag" 
pm25d6="pm25,6 day lag" 
pm25d7="pm25,7 day lag" 
pm25d8="pm25,8 day lag" 
pm25d9="pm25,9 day lag" 
pm25d10="pm25,10 day lag" 
pm25d11="pm25,11 day lag" 
pm25d12="pm25,12 day lag" 
pm25d13="pm25,13 day lag" 
pm25d14="pm25,14 day lag" 
pm25d15="pm25,15 day lag" 
pm25d16="pm25,16 day lag" 
pm25d17="pm25,17 day lag" 
pm25d18="pm25,18 day lag" 
pm25d19="pm25,19 day lag" 
pm25d20="pm25,20 day lag" 
pm25d21="pm25,21 day lag" 
pm25d22="pm25,22 day lag" 
pm25d23="pm25,23 day lag" 
pm25d24="pm25,24 day lag" 
pm25d25="pm25,25 day lag" 
pm25d26="pm25,26 day lag" 
pm25d27="pm25,27 day lag" 
pm25d28="pm25,28 day lag" 
pm25d29="pm25,29 day lag" 
pm25d30="pm25,30 day lag" 
pm25ma2="pm25,2 day average" 
pm25ma3="pm25,3 day average" 
pm25ma4="pm25,4 day average" 
pm25ma5="pm25,5 day average" 
pm25ma6="pm25,6 day average" 
pm25ma7="pm25,7 day average" 
pm25ma8="pm25,8 day average" 
pm25ma9="pm25,9 day average" 
pm25ma10="pm25,10 day average" 
pm25ma11="pm25,11 day average" 
pm25ma12="pm25,12 day average" 
pm25ma13="pm25,13 day average" 
pm25ma14="pm25,14 day average" 
pm25ma15="pm25,15 day average" 
pm25ma16="pm25,16 day average" 
pm25ma17="pm25,17 day average" 
pm25ma18="pm25,18 day average" 
pm25ma19="pm25,19 day average" 
pm25ma20="pm25,20 day average" 
pm25ma21="pm25,21 day average" 
pm25ma22="pm25,22 day average" 
pm25ma23="pm25,23 day average" 
pm25ma24="pm25,24 day average" 
pm25ma25="pm25,25 day average" 
pm25ma26="pm25,26 day average" 
pm25ma27="pm25,27 day average" 
pm25ma28="pm25,28 day average" 
pm25ma29="pm25,29 day average" 
pm25ma30="pm25,30 day average" 
pm25ma365="pm25,365 day average";
run;

%makelags(work,DATA4,lpm);

data lpmmovingavg;
set lpm;
rename lpml1=lpmd1;
rename lpml2=lpmd2;
rename lpml3=lpmd3;
rename lpml4=lpmd4;
rename lpml5=lpmd5;
rename lpml6=lpmd6; 
rename lpml7=lpmd7; 
rename lpml8=lpmd8; 
rename lpml9=lpmd9; 
rename lpml10=lpmd10; 
rename lpml11=lpmd11; 
rename lpml12=lpmd12;
rename lpml13=lpmd13; 
rename lpml14=lpmd14;
rename lpml15=lpmd15;
rename lpml16=lpmd16; 
rename lpml17=lpmd17; 
rename lpml18=lpmd18; 
rename lpml19=lpmd19; 
rename lpml20=lpmd20; 
rename lpml21=lpmd21; 
rename lpml22=lpmd22;
rename lpml23=lpmd23; 
rename lpml24=lpmd24;
rename lpml25=lpmd25; 
rename lpml26=lpmd26;
rename lpml27=lpmd27; 
rename lpml28=lpmd28;
rename lpml29=lpmd29; 
rename lpml30=lpmd30;
run;

data lpmmovingavg (keep =aid lpm date lpmd1 lpmd2 lpmd3 lpmd4 lpmd5 lpmd6 lpmma2 lpmma3 lpmma4 lpmma5 lpmma6 
lpmma7 lpmma14
 lpmma30 lpmma365);
set lpmmovingavg;
label
lpmd1="lpm,1 day lag" 
lpmd2="lpm,2 day lag" 
lpmd3="lpm,3 day lag" 
lpmd4="lpm,4 day lag" 
lpmd5="lpm,5 day lag" 
lpmd6="lpm,6 day lag" 
lpmd7="lpm,7 day lag" 
lpmd8="lpm,8 day lag" 
lpmd9="lpm,9 day lag" 
lpmd10="lpm,10 day lag" 
lpmd11="lpm,11 day lag" 
lpmd12="lpm,12 day lag" 
lpmd13="lpm,13 day lag" 
lpmd14="lpm,14 day lag" 
lpmd15="lpm,15 day lag" 
lpmd16="lpm,16 day lag" 
lpmd17="lpm,17 day lag" 
lpmd18="lpm,18 day lag" 
lpmd19="lpm,19 day lag" 
lpmd20="lpm,20 day lag" 
lpmd21="lpm,21 day lag" 
lpmd22="lpm,22 day lag" 
lpmd23="lpm,23 day lag" 
lpmd24="lpm,24 day lag" 
lpmd25="lpm,25 day lag" 
lpmd26="lpm,26 day lag" 
lpmd27="lpm,27 day lag" 
lpmd28="lpm,28 day lag" 
lpmd29="lpm,29 day lag" 
lpmd30="lpm,30 day lag" 
lpmma2="lpm,2 day average" 
lpmma3="lpm,3 day average" 
lpmma4="lpm,4 day average" 
lpmma5="lpm,5 day average" 
lpmma6="lpm,6 day average" 
lpmma7="lpm,7 day average" 
lpmma8="lpm,8 day average" 
lpmma9="lpm,9 day average" 
lpmma10="lpm,10 day average" 
lpmma11="lpm,11 day average" 
lpmma12="lpm,12 day average" 
lpmma13="lpm,13 day average" 
lpmma14="lpm,14 day average" 
lpmma15="lpm,15 day average" 
lpmma16="lpm,16 day average" 
lpmma17="lpm,17 day average" 
lpmma18="lpm,18 day average" 
lpmma19="lpm,19 day average" 
lpmma20="lpm,20 day average" 
lpmma21="lpm,21 day average" 
lpmma22="lpm,22 day average" 
lpmma23="lpm,23 day average" 
lpmma24="lpm,24 day average" 
lpmma25="lpm,25 day average" 
lpmma26="lpm,26 day average" 
lpmma27="lpm,27 day average" 
lpmma28="lpm,28 day average" 
lpmma29="lpm,29 day average" 
lpmma30="lpm,30 day average" 
lpmma365="lpm,365 day average";
run;


































proc sort data = DATA4; by aid_ct   ;run; 

libname viv 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.19.VIVA\3.1.6.1.Raw_data\VIVA data\' ;

data vivdate;
set viv.viva_aid_trdates;
rename datepred=date; 
run; 





proc sort data = vivdate; by aid date   ;run;
proc sort data = DATA4 ; by aid date ;run;

data vivdate_v2;
merge vivdate(in=a) DATA4 (in=b)  ;
  by aid date;
    if a;
	run; 

proc sort data = vivdate_v2; by aid_ct   ;run; 


 data vivdate_v3;
 set vivdate_v2;
 if lpm=. then delete;
 run; 




libname viva 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.19.VIVA\3.1.6.4.Work\2.Gather_data\FN020_Final_NAS_POLL\' ;


  data viva.viva_pm25;
 set vivdate_v3;
 run; 


 data viva.lpm_keytable;
 set cases;
 keep aid_ct lpm;
 run; 
