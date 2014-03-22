
PROC IMPORT OUT= WORK.zinc
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\export to SAS\zinc.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



PROC IMPORT OUT= WORK.zincsum
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\export to SAS\zincsum.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 



proc freq data=zinc ;
table crop / list;
table crop*cultivar / list;
table crop*Waterquali /list;
table crop*NitrogenAp /list;
run; 

 

proc freq data=zincsum ;
table crop / list;
table crop*cultivar / list;
table crop*Waterquali /list;
table crop*NitrogenAp /list;
run; 

 


/*zinc5l <- lmer( log(Znppm) ~  CO+iregg+nitro+(1|Paircount)+(1+CO|crop)+(1|crop/Cultivar),na.action=na.omit,*/
/*    data =  zinc)*/

proc mixed data=zinc method=reml;
class crop paircount cultivar ;
    model znppm =  CO iregg nitro/ s outpred = Zinc1;
     random int  / sub = paircount s;
		random  CO  / sub = crop s;
   		 random int  / sub = crop(Cultivar) s;
		  ods output solutionr = random;
run;



 proc mixed data=rc;
      class batch;
      model y = month / s;
      random int month / type=un sub=batch s;
      estimate 'slope b1 - slope b2' | month 1 / subject 1 -1;
   run;


 


