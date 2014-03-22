


libname fin 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.6.NAS\3.1.6.4.Work\2.Gather_data\FN020_Final_NAS_POLL\' ;

libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.6.NAS\3.1.6.1.Raw_data\' ;



 
data bc;
set aod.bcprednas;
run; 




data nas;
set fin.Final_NAS_POLL_lpm;
run; 

proc sort data = bc; by id date   ;run;
proc sort data = nas ; by id date ;run;

data DATA3;
merge nas(in=a) bc (in=b)  ;
  by id date;
    if a;
	run; 


data fin.final_nas_poll_lpm_bc;
set data3;
run; 
