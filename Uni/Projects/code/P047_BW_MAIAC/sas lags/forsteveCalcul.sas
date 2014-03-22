/* 08/15/2013 */
/* last edit 08/21/2013 */

libname viva "C:\Documents and Settings\KOTLOV\My Documents\Studies\Steve";
options source2 linesize=78 error=max center number pageno=1;
*options mlogic mprint symbolgen;
options nomlogic nomprint nosymbolgen;
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

	array beg (5) b1-b5;
	array end1 (5) e1-e5;
	array ma (5)   map0_12 map13_24 map25_bth map0_20 map21_bth ;
	array man (5) map0_12n map13_24n map25_bthn map0_20n map21_bthn ;
	array calcn (5) cn1-cn5;
		array pol {366}  bcl0-bcl365;
		retain pol;
	do j=1 to 5;
		calcn[j]=end1[j]-beg[j]+1;
	end;	
	do j=1 to 5;
		sum=0; man[j]=0;
		do i=beg[j]+1 to end1[j]+1;
			if pol[i]=. then;
			else do;
				sum=sum+pol[i];
				man[j]=man[j]+1;
			end;
		end;
		if man[j]=0 then man[j]=.;
		ma[j]=sum/man[j];
		calcn[j]=end1[j]-beg[j]+1;
		if man[j]*4 lt 3*(end1[j]-beg[j]+1) then ma[j]=.;
  end;
  		output;
  run;
