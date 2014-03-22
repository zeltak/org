/*** Data ***/

/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.1.Raw_data\MODIS_AOD\" /b';

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

/*Macro starts HERE. TO RUN IT FIRST MARK ALL THE SECTION UNTIL:
THE ENDMARK REMARK*/


options noquotelenmax;
options mprint;
%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  
  ** Get the current filename;
  data _null_; set dirlist; if _n_ = &filen then call symput('filename', buffer); run;
  %put File Number &filen. is &filename.; ** Goes to log so we can check progress; 

***  Here is where you put in the proc import procedure for instance - the "meat";

data WORK.tmp&filen.;              
 INFILE "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.1.Raw_data\MODIS_AOD\&filename."
   delimiter='09'x MISSOVER DSD lrecl=32767;
    INPUT
        @1     Year    		BEST4.
        @5     Month  		BEST3.
        @8     Day     		BEST3.
        @11    Longitude    	COMMA8.
        @19    Latitude     	COMMA8.
        @25    AOD          	COMMA6.;
RUN;

  data tmp&filen.; set tmp&filen.; site = &filen.;  ** Adding an identifier variable;
  run;

****  End of programming with the individual files – the "meat";

** Concatenating the tmp datasets into the final SAS data file;
  %if &filen. = 1 %then %do; 
              data &finalfile; set tmp&filen.;            run; %end;
  %else %do;  data &finalfile; set &finalfile tmp&filen.; run; %end;

     proc datasets lib=work; delete tmp&filen.; run;

  %end; 
%mend;

/*ENDMARK REMARK*/


****  Running the macro and defining the final data set name as here.testxls;

/*RUN THE MACRO HERE:*/

%readin(AODobs);


/*CREATE DATA VARIABLE*/

data AODobs (drop = site); 
 set AODobs; 
  Date = MDY(month,day,year); 
  format Date date9.;
run;


/*CREATE GUID AND SEASON */


/*copy x and y coordinantes from numeric to character (text) variables*/

   data AODobs_v3  ;
      set AODobs  ;
      xnym=put(Longitude,6.2); 
	  ynym=put(Latitude,6.2); 
	  run;

	  data  AODobs_v4 ;
      set  AODobs_v3 ;
      xnymx = xnym*-100; 
	  ynymx = ynym*100; 
	  run;

/*concentrate (compress) both x and y variables into one ID*/

      data  AODobs_v5;
      set  AODobs_v4;
      guid=compress(xnymx||ynymx);
	  run;

/*convert if from text to numeric*/

      data AODobs_v6;
      set  AODobs_v5;
	  drop  xnymx ynymx xnym ynym;
	  guid2=input(guid, 8.); 
      drop guid;
  	  run;


	  data AODobs_v7;
      set  AODobs_v6;
	  guid=guid2; 
      drop guid2;
  	  run;

/*	  add season*/

data AODobs_v8;
set AODobs_v7;
season=1;
if month=1 or month=2 or month=3 or month=10 or month=11 or month=12 then season=0;
run;



/*CREATE FINAL AOD FILE WITH X (long_AOD), Y (lat_AOD) AND DELETE BAD AOD VALUES*/

Data AOD_Final;
 set AODobs_v8;
   Long_AOD = Longitude; 
   Lat_AOD  = Latitude;
    	 if AOD >= 0.9     then delete;
	 if AOD <= 0.00001 then delete;
   keep Long_AOD Lat_AOD AOD Date guid season;
run;



/*export to dataset*/

libname aodf 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN001_AOD_full_dataset\' ;


data aodf.all_AOD;
set AOD_Final;
run; 






/*create grid by sorting by x,y and removing duplicates */


proc sort data = AOD_Final nodupkey Out = AODGrid(keep = Long_AOD Lat_AOD guid); by Long_AOD Lat_AOD guid; run; 





PROC EXPORT DATA= AODGrid
            OUTFILE= "c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.1.NE_PM_MODELS\3.1.1.4.Work\2.Gather_data\FN007_Key_tables\aodgridfull.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 




