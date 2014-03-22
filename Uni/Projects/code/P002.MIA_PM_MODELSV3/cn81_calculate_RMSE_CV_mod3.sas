libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.4.Work\3.Analysis\AN002_mod1_CV\overall_random\' ;



options mprint;
%macro import(year=);


/*erros will occure in teh import due to sitecode charatcer/numeirc issues..ignore that*/

PROC IMPORT OUT= Pdataa_&year
  DATAFILE= "c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.2.MIA_PM_MODELSV3/3.1.1.4.Work/3.Analysis/AN081_files_4CV_RMSE_mod3/t&year..csv" 
    DBMS=CSV REPLACE;
	  GETNAMES=YES;
	    DATAROW=2; 
		RUN;
		 



/* summarize the results of the cross-validations */ 
data sumPdataa_&year; 
  set Pdataa_&year; 
  d = pm25-OApred; 
  absd=abs(d); 
run; 

/*

Mean absolute error (MAE)

The MAE measures the average magnitude of the errors in a set of forecasts, 
without considering their direction. It measures accuracy for continuous variables. 
The equation is given in the library references.
Expressed in words, the MAE is the average over the verification sample of the absolute 
values of the differences between forecast and the corresponding observation. 
The MAE is a linear score which means that all the individual differences are weighted equally in the average.

Root mean squared error (RMSE)

The RMSE is a quadratic scoring rule which measures the average magnitude of the error. 
The equation for the RMSE is given in both of the references. 
Expressing the formula in words, the difference between forecast and 
corresponding observed values are each squared and then averaged over the sample. 
Finally, the square root of the average is taken. Since the errors are squared before 
they are averaged, the RMSE gives a relatively high weight to large errors. 
This means the RMSE is most useful when large errors are particularly undesirable.

*/


proc summary data = sumPdataa_&year; 
  var d absd; 
  output out= Error_&year std(d)=rmse1 mean(d)= mpe1 mean(absd)= mae1; 
run; 

data Error_&year(drop = _Type_ _FREQ_); 
 set Error_&year;
  Year = &Year;
run;

data Error_&year(drop = rmse1 mpe1 mae1);
 set Error_&year;
  rmse  = round(rmse1,0.001);
  mpe   = round(mpe1, 0.0000000000001);
  mae   = round(mae1, 0.001);
run;

%MEND ;

%import(year=2000);
%import(year=2001);
%import(year=2002);
%import(year=2003);
%import(year=2004);
%import(year=2005);
%import(year=2006);
%import(year=2007);
%import(year=2008);


data Error_final;
 set Error_2000 Error_2001 Error_2002 Error_2003 Error_2004 Error_2005 Error_2006 Error_2007 Error_2008;
  label rmse = "Root mean squared error (RMSE)";
  label mpe  = "Mean prediction error";
  label mae  = "Mean absolute error (MAE)";
run;


PROC EXPORT DATA= Error_final
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.2.MIA_PM_MODELSV3\3.1.1.5.Results\RMSE\mod3.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  
