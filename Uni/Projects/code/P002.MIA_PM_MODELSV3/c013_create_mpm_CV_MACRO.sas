
libname mpm2 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\2.Gather_data\FN013_mpm_peryear\' ;

options mprint;
%macro import(year=, it=);


data Mean_&year._v4;
set mpm2.mpm&year;
run; 

 

PROC IMPORT OUT= WORK.mod2pred&year._&it
           DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN009_mod2_CV_files\T&year._m2_pred_&it..dbf" 
		               DBMS=DBF REPLACE;
					        GETDELETED=NO;
							RUN; 



proc sort data = Mean_&year._v4; by date reg_id   ;run;
proc sort data = mod2pred&year._&it ; by date reg_id ;run;

data mod2pred&year._V2;
merge mod2pred&year._&it(in=a) Mean_&year._v4 (in=b keep=date reg_id mpm_F)  ;
  by date reg_id;
    if a;
	run; 


	data mod2pred&year._V3 (keep=guid lat_aod long_aod mpm_F bimon pred pred_si date );
	set mod2pred&year._V2;
	m = month(date); 
if (m=1 or m=2) then bimon=1; 
if (m=3 or m=4) then bimon=2;
if (m=5 or m=6) then bimon=3;
if (m=7 or m=8) then bimon=4;
if (m=9 or m=10) then bimon=5;
if (m=11 or m=12) then bimon=6;
if pred=. then delete;
	run; 


PROC EXPORT DATA= WORK.mod2pred&year._V3
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN009_mod2_CV_files_mpm\T&year._m2_pred_mpm_&it..csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  




%MEND ;

%import(year=2000, It = s1);
%import(year=2000, It = s2);
%import(year=2000, It = s3);
%import(year=2000, It = s4);
%import(year=2000, It = s5);
%import(year=2000, It = s6);
%import(year=2000, It = s7);
%import(year=2000, It = s8);
%import(year=2000, It = s9);
%import(year=2000, It = s10);



%import(year=2001, It = s1);
%import(year=2001, It = s2);
%import(year=2001, It = s3);
%import(year=2001, It = s4);
%import(year=2001, It = s5);
%import(year=2001, It = s6);
%import(year=2001, It = s7);
%import(year=2001, It = s8);
%import(year=2001, It = s9);
%import(year=2001, It = s10);





%import(year=2002, It = s1);
%import(year=2002, It = s2);
%import(year=2002, It = s3);
%import(year=2002, It = s4);
%import(year=2002, It = s5);
%import(year=2002, It = s6);
%import(year=2002, It = s7);
%import(year=2002, It = s8);
%import(year=2002, It = s9);
%import(year=2002, It = s10);



%import(year=2003, It = s1);
%import(year=2003, It = s2);
%import(year=2003, It = s3);
%import(year=2003, It = s4);
%import(year=2003, It = s5);
%import(year=2003, It = s6);
%import(year=2003, It = s7);
%import(year=2003, It = s8);
%import(year=2003, It = s9);
%import(year=2003, It = s10);



%import(year=2004, It = s1);
%import(year=2004, It = s2);
%import(year=2004, It = s3);
%import(year=2004, It = s4);
%import(year=2004, It = s5);
%import(year=2004, It = s6);
%import(year=2004, It = s7);
%import(year=2004, It = s8);
%import(year=2004, It = s9);
%import(year=2004, It = s10);



%import(year=2005, It = s1);
%import(year=2005, It = s2);
%import(year=2005, It = s3);
%import(year=2005, It = s4);
%import(year=2005, It = s5);
%import(year=2005, It = s6);
%import(year=2005, It = s7);
%import(year=2005, It = s8);
%import(year=2005, It = s9);
%import(year=2005, It = s10);



%import(year=2006, It = s1);
%import(year=2006, It = s2);
%import(year=2006, It = s3);
%import(year=2006, It = s4);
%import(year=2006, It = s5);
%import(year=2006, It = s6);
%import(year=2006, It = s7);
%import(year=2006, It = s8);
%import(year=2006, It = s9);
%import(year=2006, It = s10);



%import(year=2007, It = s1);
%import(year=2007, It = s2);
%import(year=2007, It = s3);
%import(year=2007, It = s4);
%import(year=2007, It = s5);
%import(year=2007, It = s6);
%import(year=2007, It = s7);
%import(year=2007, It = s8);
%import(year=2007, It = s9);
%import(year=2007, It = s10);




%import(year=2008, It = s1);
%import(year=2008, It = s2);
%import(year=2008, It = s3);
%import(year=2008, It = s4);
%import(year=2008, It = s5);
%import(year=2008, It = s6);
%import(year=2008, It = s7);
%import(year=2008, It = s8);
%import(year=2008, It = s9);
%import(year=2008, It = s10);








