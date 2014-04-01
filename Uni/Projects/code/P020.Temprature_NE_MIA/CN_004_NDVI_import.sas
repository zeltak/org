/*ALL LOG WHEN THE SCRIPS RUNS*/

/*proc printto log="nul:"; run;*/



/*START YEALRY IMPORT AND COMBINING OF MODIS LST DATA*/

/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2000\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



/*options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;*/
/*options noquotelenmax;*/

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2000\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;


%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;


data &filename;
 merge &filename._0 ;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 ; run;

data &filename (drop= &filename._0)  ;
 set &filename;
  NDVI   = (&filename._0)*0.0001;
    month = month(DATE); 
    run; 


/*append every iteration to oned file*/
proc append base=NDVI2000 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;
PROC EXPORT DATA= work.NDVI2000
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2000.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 



/*Y2001 START YEALRY IMPORT AND COMBINING OF MODIS LST DATA*/

/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2001\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



/*options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;*/
/*options noquotelenmax;*/

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2001\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;


%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;


data &filename;
 merge &filename._0 ;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 ; run;

data &filename (drop= &filename._0)  ;
 set &filename;
  NDVI   = (&filename._0)*0.0001;
    month = month(DATE); 
    run; 


/*append every iteration to oned file*/
proc append base=NDVI2001 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;
PROC EXPORT DATA= work.NDVI2001
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2001.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*Y2002 START YEALRY IMPORT AND COMBINING OF MODIS LST DATA*/

/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2002\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



/*options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;*/
/*options noquotelenmax;*/

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2002\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;


%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;


data &filename;
 merge &filename._0 ;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 ; run;

data &filename (drop= &filename._0)  ;
 set &filename;
  NDVI   = (&filename._0)*0.0001;
    month = month(DATE); 
    run; 


/*append every iteration to oned file*/
proc append base=NDVI2002 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;
PROC EXPORT DATA= work.NDVI2002
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2002.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*Y2003 START YEALRY IMPORT AND COMBINING OF MODIS LST DATA*/

/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2003\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



/*options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;*/
/*options noquotelenmax;*/

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2003\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;


%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;


data &filename;
 merge &filename._0 ;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 ; run;

data &filename (drop= &filename._0)  ;
 set &filename;
  NDVI   = (&filename._0)*0.0001;
    month = month(DATE); 
    run; 


/*append every iteration to oned file*/
proc append base=NDVI2003 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;
PROC EXPORT DATA= work.NDVI2003
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2003.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*Y2004 START YEALRY IMPORT AND COMBINING OF MODIS LST DATA*/

/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2004\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



/*options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;*/
/*options noquotelenmax;*/

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2004\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;


%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;


data &filename;
 merge &filename._0 ;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 ; run;

data &filename (drop= &filename._0)  ;
 set &filename;
  NDVI   = (&filename._0)*0.0001;
    month = month(DATE); 
    run; 


/*append every iteration to oned file*/
proc append base=NDVI2004 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;
PROC EXPORT DATA= work.NDVI2004
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2004.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*Y2005 START YEALRY IMPORT AND COMBINING OF MODIS LST DATA*/

/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2005\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



/*options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;*/
/*options noquotelenmax;*/

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2005\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;


%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;


data &filename;
 merge &filename._0 ;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 ; run;

data &filename (drop= &filename._0)  ;
 set &filename;
  NDVI   = (&filename._0)*0.0001;
    month = month(DATE); 
    run; 


/*append every iteration to oned file*/
proc append base=NDVI2005 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;
PROC EXPORT DATA= work.NDVI2005
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2005.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*Y2006 START YEALRY IMPORT AND COMBINING OF MODIS LST DATA*/

/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2006\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



/*options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;*/
/*options noquotelenmax;*/

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2006\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;


%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;


data &filename;
 merge &filename._0 ;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 ; run;

data &filename (drop= &filename._0)  ;
 set &filename;
  NDVI   = (&filename._0)*0.0001;
    month = month(DATE); 
    run; 


/*append every iteration to oned file*/
proc append base=NDVI2006 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;
PROC EXPORT DATA= work.NDVI2006
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2006.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*Y2007 START YEALRY IMPORT AND COMBINING OF MODIS LST DATA*/

/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2007\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



/*options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;*/
/*options noquotelenmax;*/

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2007\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;


%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;


data &filename;
 merge &filename._0 ;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 ; run;

data &filename (drop= &filename._0)  ;
 set &filename;
  NDVI   = (&filename._0)*0.0001;
    month = month(DATE); 
    run; 


/*append every iteration to oned file*/
proc append base=NDVI2007 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;
PROC EXPORT DATA= work.NDVI2007
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2007.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*Y2008 START YEALRY IMPORT AND COMBINING OF MODIS LST DATA*/

/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2008\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



/*options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;*/
/*options noquotelenmax;*/

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2008\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;


%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;


data &filename;
 merge &filename._0 ;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 ; run;

data &filename (drop= &filename._0)  ;
 set &filename;
  NDVI   = (&filename._0)*0.0001;
    month = month(DATE); 
    run; 


/*append every iteration to oned file*/
proc append base=NDVI2008 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;
PROC EXPORT DATA= work.NDVI2008
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2008.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*Y2009 START YEALRY IMPORT AND COMBINING OF MODIS LST DATA*/

/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2009\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



/*options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;*/
/*options noquotelenmax;*/

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2009\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;


%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;


data &filename;
 merge &filename._0 ;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 ; run;

data &filename (drop= &filename._0)  ;
 set &filename;
  NDVI   = (&filename._0)*0.0001;
    month = month(DATE); 
    run; 


/*append every iteration to oned file*/
proc append base=NDVI2009 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;
PROC EXPORT DATA= work.NDVI2009
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2009.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*Y2010 START YEALRY IMPORT AND COMBINING OF MODIS LST DATA*/

/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2010\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



/*options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;*/
/*options noquotelenmax;*/

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2010\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;


%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;


data &filename;
 merge &filename._0 ;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 ; run;

data &filename (drop= &filename._0)  ;
 set &filename;
  NDVI   = (&filename._0)*0.0001;
    month = month(DATE); 
    run; 


/*append every iteration to oned file*/
proc append base=NDVI2010 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;
PROC EXPORT DATA= work.NDVI2010
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2010.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*Y2011 START YEALRY IMPORT AND COMBINING OF MODIS LST DATA*/

/*Define where AOD file are in*/

filename DIRLIST pipe 'dir "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2011\" /b';

/** Generate List of filename base on the dir above **/

data dirlist;
     length buffer $256 ;
     infile dirlist length=reclen ;
     input buffer $varying256. reclen ;
run;

data dirlist; length try $ 100; 
 set dirlist;
 try = trim(substr(buffer, 1, 14));
run; 

/** Macro that renames Variables from a List, We use this in the following Loop **/

%macro rename1(oldvarlist, newvarlist);
  %let k=1;
  %let old = %scan(&oldvarlist, &k);
  %let new = %scan(&newvarlist, &k);
     %do %while(("&old" NE "") & ("&new" NE ""));
      rename &old = &new;
	  %let k = %eval(&k + 1);
      %let old = %scan(&oldvarlist, &k);
      %let new = %scan(&newvarlist, &k);
  %end;
%mend;



/*options nonotes nosource nosource2 NOMLOGIC NOMPRINT NOMRECALL NOSYMBOLGEN errors=0; *suppresses LOG WINDOW printing;*/
/*options noquotelenmax;*/

%macro readin(finalfile);
** Getting the number of filenames into a macro variable called list_length;
   data _null_; set dirlist end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set dirlist; if _n_ = &filen then call symput('filename', try); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


PROC IMPORT OUT= WORK.&filename      
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN001_yearly_NDVI\2011\&filename." 
			DBMS=DBF   REPLACE;
			GETDELETED=NO;
RUN; 

data WORK.&filename(drop = var1 dd newjul);


/*** Get DATA fron Julian DATA ***/

 set WORK.&filename;
   var1 = "&filename";
    dd = substr(var1,6,7);
     newjul= input(dd,julian7.);

  date=newjul;
  format DATE  date9.;

run;

proc contents data = WORK.&filename noprint out = Vars;
run;

/*** Create the macrovariables for RENAME ***/

data _null_;
 set Vars;
 if _n_ = 1 then call symput('NAME', NAME);
 if _n_ = 2 then call symput('DROP', NAME);
run;

/*** Rename the variable base on the name of the dataset ***/

data WORK.&filename(drop = &drop);
  set WORK.&filename;
  %rename1(&NAME, &filename);
run;

%end; 

%mend;



DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;


%readin(Final);


data List(keep = name); 
 set dirlist;
  name = trim(substr(buffer, 1, 12));
run; 

proc sort data = List nodupkey out = List; by name; run;

%macro MergeData;

** Getting the number of filenames into a macro variable called list_length;
 data _null_; set list end = eof; if eof then call symput('list_length',_n_); run;
   %put Number of files is &list_length ; * Writes to log so we can check progress;

** Cycling through all the files and reading the data in;
  %do filen = 1 %to &list_length;  

** Get the current filename;
data _null_; set list; if _n_ = &filen then call symput('filename', trim(left(name))); run;
  %put File Number &filen. is &filename; ** Goes to log so we can check progress; 


proc sort data = &filename._0;  by x y ;run;


data &filename;
 merge &filename._0 ;
   by x y;
run; 
quit;

proc datasets lib = work; delete &filename._0 ; run;

data &filename (drop= &filename._0)  ;
 set &filename;
  NDVI   = (&filename._0)*0.0001;
    month = month(DATE); 
    run; 


/*append every iteration to oned file*/
proc append base=NDVI2011 data = &filename;
run;

%end; 

%mend;

%MergeData;


options notes source source2;
PROC EXPORT DATA= work.NDVI2011
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi2011.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/
/*fix grid*/


options mprint;
%macro import(year=);



PROC IMPORT OUT= WORK.lst
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi&year..dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 


/*#fix clip offset*/
data lst_fix;
set lst;
xx=x+0.003191407;
yy=y+0.00374356;
run;  

PROC EXPORT DATA= lst_fix
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN006_NDVI_yearly\ndvi&year..dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 

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
%import(year=2009);
%import(year=2010);
%import(year=2011); 



/*#create a full grid*/
/*#create a full grid*/
/*#create a full grid*/
/*#create a full grid*/
/*#create a full grid*/
/*#create a full grid*/
/*#create a full grid*/
/*#create a full grid*/

 

proc sort data = lst_fix nodupkey out=grid; by xx yy; run;quit;


PROC EXPORT DATA= grid
            OUTFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\NDVI_fullgrid.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 


