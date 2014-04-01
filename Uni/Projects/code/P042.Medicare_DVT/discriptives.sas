
/**/
/*discriptveis*/
/**/



libname cc 'P:\P042_Medicare_DVT\3.1.10.4.Work\2.Gather_data\FN008_cases\' ;

data all3;
set cc.cases;
 if race = "1" then brace=1;*white;
 if race = "2" then brace=2;*black;
 if race in ("0","3","4","5","6") then brace = 3;*other;
run; 


 
/*table discriptives*/

/*get percent sex*/

/*all*/

proc freq data=all3;
table sex ;
run; 


/*all race*/

proc freq data=all3;
table brace ;
run; 


proc means data=all3  mean std;
var age; 
run; 



proc means data=all3  mean ;
var Avg_P05300; 
run; 



proc means data=all3 mean min max  median std range  QRANGE q1 q3 n ;
var pmnew_l1 pmnewmayear temp_fmayear ;
run; 


proc means data=all3 mean  std ;
var pmnew_l1 pmnewmayear ;
where sex=1;
run; 

proc means data=all3 mean  std ;
var pmnew_l1 pmnewmayear ;
where sex=2;
run; 


proc means data=all3 mean  std ;
var pmnew_l1 pmnewmayear ;
where brace=1;
run;

proc means data=all3 mean  std ;
var pmnew_l1 pmnewmayear ;
where brace=2;
run;

proc means data=all3 mean  std ;
var pmnew_l1 pmnewmayear ;
where brace=3;
run;
