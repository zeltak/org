/*08/14/2013*/
libname viva "C:\Documents and Settings\KOTLOV\My Documents\Studies\Steve";
options source2 linesize=78 error=max center number pageno=1;
options mlogic mprint symbolgen;
*options nomlogic nomprint nosymbolgen;
data bc2;
	set viva.bc2(rename=(bc=bcl0)); /*  */
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
data bc21;
	set bc2;
	array pol {366}  bcl0-bcl365;
	retain pol;
	array beg (5) b1-b5;
	array end1 (5) e1-e5;
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
		if man[j]*4 lt 3*(end1[j]-beg[j]+1) then 
			ma[j]=.;
		if man[j]=0 then man[j]=.;
  end;
  output;
  run;
 
