libname viva "/usr1/home/sjmelly/bcmodel/VIVA/";


proc sort data = viva.DATA4;
by aid datepred;
run;



%macro makelags(dir,fname,pol);
 data viva.&pol;	set viva.&fname;
 	&pol.l0=&pol;
/*Check if first observation for subject*/
prevaid = lag1(aid);
if aid NE prevaid then firstdate=datepred;
%local i;
	%do i=0 %to 365;
		&pol.l%eval(&i+1)=lag1(&pol.l&i);
		/*for first observation for a subject set &pol.1%eval(&i+1) to . */
		if datepred = firstdate then &pol.l%eval(&i+1) = .;
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

	keep aid datepred &pol.l1-&pol.l30 &pol.ma2-&pol.ma30
	&pol.ma10 &pol.ma11 &pol.ma12 &pol.ma13 &pol.ma14 
	&pol.ma15 &pol.ma16 &pol.ma17 &pol.ma18 &pol.ma19
	&pol.ma20 &pol.ma21 &pol.ma22 &pol.ma23 &pol.ma24 
	&pol.ma25 &pol.ma26 &pol.ma27 &pol.ma28 &pol.ma29
	&pol.ma30 &pol.ma365 &pol ;
	run;
%mend;


/*HERE'S WHERE MAKELAGS MACRO IS CALLED*/



%makelags(viva,DATA4,pm25new);

data viva.pm25movingavg;
set viva.pm25new;
rename pm25newl1=pm25d1;
rename pm25newl2=pm25d2;
rename pm25newl3=pm25d3;
rename pm25newl4=pm25d4;
rename pm25newl5=pm25d5;
rename pm25newl6=pm25d6; 
rename pm25newl7=pm25d7; 
rename pm25newl8=pm25d8; 
rename pm25newl9=pm25d9; 
rename pm25newl10=pm25d10; 
rename pm25newl11=pm25d11; 
rename pm25newl12=pm25d12;
rename pm25newl13=pm25d13; 
rename pm25newl14=pm25d14;
rename pm25newl15=pm25d15;
rename pm25newl16=pm25d16; 
rename pm25newl17=pm25d17; 
rename pm25newl18=pm25d18; 
rename pm25newl19=pm25d19; 
rename pm25newl20=pm25d20; 
rename pm25newl21=pm25d21; 
rename pm25newl22=pm25d22;
rename pm25newl23=pm25d23; 
rename pm25newl24=pm25d24;
rename pm25newl25=pm25d25; 
rename pm25newl26=pm25d26;
rename pm25newl27=pm25d27; 
rename pm25newl28=pm25d28;
rename pm25newl29=pm25d29; 
rename pm25newl30=pm25d30;
rename pm25newma2=pm25ma2;
rename pm25newma3=pm25ma3;
rename pm25newma4=pm25ma4;
rename pm25newma5=pm25ma5;
rename pm25newma6=pm25ma6; 
rename pm25newma7=pm25ma7; 
rename pm25newma8=pm25ma8; 
rename pm25newma9=pm25ma9; 
rename pm25newma10=pm25ma10; 
rename pm25newma11=pm25ma11; 
rename pm25newma12=pm25ma12;
rename pm25newma13=pm25ma13; 
rename pm25newma14=pm25ma14;
rename pm25newma15=pm25ma15;
rename pm25newma16=pm25ma16; 
rename pm25newma17=pm25ma17; 
rename pm25newma18=pm25ma18; 
rename pm25newma19=pm25ma19; 
rename pm25newma20=pm25ma20; 
rename pm25newma21=pm25ma21; 
rename pm25newma22=pm25ma22;
rename pm25newma23=pm25ma23; 
rename pm25newma24=pm25ma24;
rename pm25newma25=pm25ma25; 
rename pm25newma26=pm25ma26;
rename pm25newma27=pm25ma27; 
rename pm25newma28=pm25ma28;
rename pm25newma29=pm25ma29; 
rename pm25newma30=pm25ma30;
rename pm25newma365=pm25ma365;
run;

data viva.pm25movingavg (keep =aid pm25_24hr datepred pm25d1 pm25d2 pm25d3 pm25d4 pm25d5 pm25d6 pm25ma2 pm25ma3 
pm25ma4 pm25ma5 pm25ma6 pm25ma7 pm25ma14
 pm25ma30 pm25ma365);
set viva.pm25movingavg;
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

%makelags(viva,DATA4,lpm);

data viva.lpmmovingavg;
set viva.lpm;
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

data viva.lpmmovingavg (keep =aid lpm datepred lpmd1 lpmd2 lpmd3 lpmd4 lpmd5 lpmd6 lpmma2 lpmma3 lpmma4 lpmma5 lpmma6 
lpmma7 lpmma14
 lpmma30 lpmma365);
set viva.lpmmovingavg;
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

