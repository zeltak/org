
libname aod 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.1.Raw_data\medicare admission data\' ;

data NEMC;
set aod.midstates;
run; 




PROC IMPORT OUT= WORK.zip_guid
            DATAFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\2.Gather_data\FN007_keyed_tables\MIA_zipcode_guid.dbf" 
            DBMS=DBF   REPLACE;
     GETDELETED=NO;
RUN; 	

/*clean from non study area zipcodes which have a Null-0 value for guid*/

data zip_guid(drop=zip);
set zip_guid;
if guid=0 then delete;
zipcode=zip;
run; 

/*merge cases with guid file*/

proc sort data = NEMC; by zipcode   ;run;
proc sort data = zip_guid ; by zipcode ;run;

data NEMCXYguid;
merge NEMC(in=a) zip_guid (in=b keep=zipcode guid x y)  ;
  by zipcode;
    if a;
	run; 

data NEMCXYguid;
set NEMCXYguid;
if guid=. then delete;
run; 


PROC EXPORT DATA= NEMCXYguid
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\2.Gather_data\FN001_Cases_guid_MIA\ALL.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*split files macro*/


options mprint;
%macro import(type=);

data &type;
set NEMCXYguid;
where &type=1;
run; 
 
PROC EXPORT DATA= &type
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.17.Medicare_MIA\3.1.10.4.Work\2.Gather_data\FN001_Cases_guid_MIA\&type..dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


%MEND ;


%import(type=copd   );
%import(type=ari    );
%import(type=pneum  );
%import(type=mi     );
%import(type=chf    );
%import(type=diab   );
%import(type=resp );
%import(type=cvd    );
%import(type=ihd    );
%import(type=stroke    );
%import(type=gi    );
%import(type=strisc    );
%import(type=strhem    );


