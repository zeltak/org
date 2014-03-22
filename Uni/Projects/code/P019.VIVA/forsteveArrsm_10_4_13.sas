/*08/14/2013*/
/*libname viva "C:\Documents and Settings\KOTLOV\My Documents\Studies\Steve";*/
/*libname viva 'C:\gisdata\ProjectData\MADPH_Births\pregnancyexposure_sas\fromtania\pregnancyexposureperiodaverages_melly_8_1_13' ;*/
libname births 'C:\gisdata\ProjectData\MADPH_Births\pregnancyexposure_sas';
options source2 linesize=78 error=max center number pageno=1;
options mlogic mprint symbolgen;
*options nomlogic nomprint nosymbolgen;
/*calculate average exposure for the following periods:
0 to 12 weeks  lag(b2) to lag(e1) 
13 to 24 weeks lag(b2) to lag(e2)
25 weeks to birth lag(b3) to lag(e3)
0 to 20 weeks  lag(b4) to lag(e4)
21 weeks to birth lag(b5) to lag()
*/

proc contents data = births.bw_pm_prelmp;
run;
data births.bw_pm_prelmp2;
	set births.bw_pm_prelmp /*(rename=(bc=bcl0))*/; /*  */
	  ga = gacalc*7;
	  b1=ga-90.;
	  b2=ga-174.;
	  b3=0.;
	  b4=ga-139.;
	  b5=0;
	  e1=ga;
	  e2=ga-91.;
	  e3=ga-175.;
	  e4=ga;
	  e5=ga-140;
run;

data births.bw_pm_prelmp21;
	set births.bw_pm_prelmp2;
	array pol {321}  pmnew_l0-pmnew_l320;
	retain pol;
	array beg (5) b1-b5;
	array end1 (5) e1-e5;
    /*variable names mapx_y = average of estimates from week x to week y
	mapx_yn = count of estimates from week x to week y*/
    array ma (5)   map0_12 map13_24 map25_bth map0_20 map21_bth ;
	array man (5) map0_12n map13_24n map25_bthn map0_20n map21_bthn ;
	do j=1 to 5;
		array cpol [366] ;
		call missing(of cpol[*]);
		do i=1 to 366;
			if i<beg[j]+1 or i >end1[j]+1 then cpol[i]=.;
			else 
			cpol[i]=pol[i];
		end;
		ma[j]=mean(of cpol(*));
		man[j]=n(of cpol(*));
		/* if more than 75% of the days are missing set to 0*/
		if man[j]*4 lt 3*(end1[j]-beg[j]+1) then 
			ma[j]=.;
		if man[j]=0 then man[j]=.;
  end;
  output;
  run;

  data births.bw_pm_ma (keep = uniqueid_y guid pmnewma1 pmnewma3 pmnewma2week pmnewma3month pmnewmabirth pmnewmamonth pmnewmaweek
  map0_12 map13_24 map25_bth map0_20 map21_bth gacalc ga);
  set births.bw_pm_prelmp21;
  run;

  proc reg data = births.bw_pm_ma;
  model map25_bth = pmnewma3month;
  run;
  data work.test;
  set births.bw_pm_prelmp21;
  where uniqueid_y = '10000_03';
  run;
 
