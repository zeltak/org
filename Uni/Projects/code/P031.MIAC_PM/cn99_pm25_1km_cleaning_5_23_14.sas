libname naspm25 'C:\gisdata\ProjectData\NAS\nas_temp_pm25_1km';
libname naspm25s 'S:\ENVEPI\Airs\nas\NAS_PM_EXPOSURE\1x1KM_data\SasData';
libname pm25full 'S:\ENVEPI\Airs\nas\NAS_PM_EXPOSURE\1x1KM_data\NEW1995-2004\FULL PM GRID';


libname pm2510km 'S:\ENVEPI\Airs\nas\NAS_PM_EXPOSURE';

libname pm25514 'h:\pm25model_1k_5_14';
       data PM25514.mod3best_08NM                        ;
       %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
       infile '\\DROBO\Shared_Data\MAIAC_05_2014\mod3best_2008NM.csv' delimiter = ',' MISSOVER  DSD lrecl=32767 firstobs=2 ;
          informat VAR1 $5. ;
          informat day yymmdd10. ;
          informat guid best32. ;
          informat long_aod best32. ;
          informat lat_aod best32. ;
          informat predicted_m3 best32. ;
          informat predicted_m2 $2. ;
          informat PM25 $2. ;
          informat predicted_m1 $2. ;
          informat bestpred best32. ;
          format VAR1 $5. ;
          format day yymmdd10. ;
          format guid best12. ;
          format long_aod best12. ;
          format lat_aod best12. ;
          format predicted_m3 best12. ;
          format predicted_m2 $2. ;
          format PM25 $2. ;
          format predicted_m1 $2. ;
          format bestpred best12. ;
       input
                   VAR1 $
                   day
                   guid
                   long_aod
                   lat_aod
                   predicted_m3
                   predicted_m2 $
                   PM25 $
                   predicted_m1 $
                   bestpred
       ;
       if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
       run;



proc sort data = PM25514.mod3best_08NM;
by guid day;
run;

proc means noprint data = PM25514.mod3best_08NM;
by guid day;
var bestpred predicted_m3 long_aod lat_aod day;
output out = PM25514.mod3best_08NMsum N(bestpred) = cntpred N(day)= cntdays mean(bestpred) = avgpm25 mean(predicted_m3) = predicted_m3 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;
/*
data work.checkdups;
set PM25514.mod3best_08NMsum;
where minpm25 ne maxpm25;
run;
data work.checkdups;
set PM25514.mod3best_08NMsum;
where cntpred > 1;
run;

proc means noprint data = PM25514.mod3best_08NMsum ;
var avgpm25;
by guid;
output out = PM25514.mod3best_08NMnodupsum  N(avgpm25)= cntpm25 mean(avgpm25) = avg08pm25 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;

data PM25514.mod3best_08NMnodupgt75pct;
set PM25514.mod3best_08NMnodupsum;
where cntpm25 gt 273;
run;
data work.missing;
set PM25514.mod3best_08NMnodupsum;
where cntpm25 lt 366;
run;
*/
data PM25514.mod3best_09NM                        ;
       %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
       infile '\\DROBO\Shared_Data\MAIAC_05_2014\mod3best_2009NM.csv' delimiter = ',' MISSOVER  DSD lrecl=32767 firstobs=2 ;
          informat VAR1 $5. ;
          informat day yymmdd10. ;
          informat guid best32. ;
          informat long_aod best32. ;
          informat lat_aod best32. ;
          informat predicted_m3 best32. ;
          informat predicted_m2 $2. ;
          informat PM25 $2. ;
          informat predicted_m1 $2. ;
          informat bestpred best32. ;
          format VAR1 $5. ;
          format day yymmdd10. ;
          format guid best12. ;
          format long_aod best12. ;
          format lat_aod best12. ;
          format predicted_m3 best12. ;
          format predicted_m2 $2. ;
          format PM25 $2. ;
          format predicted_m1 $2. ;
          format bestpred best12. ;
       input
                   VAR1 $
                   day
                   guid
                   long_aod
                   lat_aod
                   predicted_m3
                   predicted_m2 $
                   PM25 $
                   predicted_m1 $
                   bestpred
       ;
       if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
       run;



proc sort data = PM25514.mod3best_09NM;
by guid day;
run;

proc means noprint data = PM25514.mod3best_09NM;
by guid day;
var bestpred predicted_m3 long_aod lat_aod day;
output out = PM25514.mod3best_09NMsum N(bestpred) = cntpred N(day)= cntdays mean(bestpred) = avgpm25 mean(predicted_m3) = predicted_m3 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;
/*
proc means noprint data = PM25514.mod3best_09NMsum ;
var avgpm25;
by guid;
output out = PM25514.mod3best_09NMnodupsum  N(avgpm25)= cntpm25 mean(avgpm25) = avg09pm25 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;

data PM25514.mod3best_09NMnodupgt75pct;
set PM25514.mod3best_09NMnodupsum;
where cntpm25 gt 273;
run;
data work.missing;
set PM25514.mod3best_09NMnodupsum;
where cntpm25 lt 365;
run;

     
*/
data PM25514.mod3best_10NM                        ;
       %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
       infile '\\DROBO\Shared_Data\MAIAC_05_2014\mod3best_2010NM.csv' delimiter = ',' MISSOVER  DSD lrecl=32767 firstobs=2 ;
          informat VAR1 $5. ;
          informat day yymmdd10. ;
          informat guid best32. ;
          informat long_aod best32. ;
          informat lat_aod best32. ;
          informat predicted_m3 best32. ;
          informat predicted_m2 $2. ;
          informat PM25 $2. ;
          informat predicted_m1 $2. ;
          informat bestpred best32. ;
          format VAR1 $5. ;
          format day yymmdd10. ;
          format guid best12. ;
          format long_aod best12. ;
          format lat_aod best12. ;
          format predicted_m3 best12. ;
          format predicted_m2 $2. ;
          format PM25 $2. ;
          format predicted_m1 $2. ;
          format bestpred best12. ;
       input
                   VAR1 $
                   day
                   guid
                   long_aod
                   lat_aod
                   predicted_m3
                   predicted_m2 $
                   PM25 $
                   predicted_m1 $
                   bestpred
       ;
       if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
       run;



proc sort data = PM25514.mod3best_10NM;
by guid day;
run;

proc means noprint data = PM25514.mod3best_10NM;
by guid day;
var bestpred predicted_m3 long_aod lat_aod day;
output out = PM25514.mod3best_10NMsum N(bestpred) = cntpred N(day)= cntdays mean(bestpred) = avgpm25 mean(predicted_m3) = predicted_m3 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;
/*
proc means noprint data = PM25514.mod3best_10NMsum ;
var avgpm25;
by guid;
output out = PM25514.mod3best_10NMnodupsum  N(avgpm25)= cntpm25 mean(avgpm25) = avg10pm25 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;

data PM25514.mod3best_10NMnodupgt75pct;
set PM25514.mod3best_10NMnodupsum;
where cntpm25 gt 273;
run;
*/
data PM25514.mod3best_11NM                        ;
       %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
       infile '\\DROBO\Shared_Data\MAIAC_05_2014\mod3best_2011NM.csv' delimiter = ',' MISSOVER  DSD lrecl=32767 firstobs=2 ;
          informat VAR1 $5. ;
          informat day yymmdd10. ;
          informat guid best32. ;
          informat long_aod best32. ;
          informat lat_aod best32. ;
          informat predicted_m3 best32. ;
          informat predicted_m2 $2. ;
          informat PM25 $2. ;
          informat predicted_m1 $2. ;
          informat bestpred best32. ;
          format VAR1 $5. ;
          format day yymmdd10. ;
          format guid best12. ;
          format long_aod best12. ;
          format lat_aod best12. ;
          format predicted_m3 best12. ;
          format predicted_m2 $2. ;
          format PM25 $2. ;
          format predicted_m1 $2. ;
          format bestpred best12. ;
       input
                   VAR1 $
                   day
                   guid
                   long_aod
                   lat_aod
                   predicted_m3
                   predicted_m2 $
                   PM25 $
                   predicted_m1 $
                   bestpred
       ;
       if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
       run;



proc sort data = PM25514.mod3best_11NM;
by guid day;
run;

proc means noprint data = PM25514.mod3best_11NM;
by guid day;
var bestpred predicted_m3 long_aod lat_aod day;
output out = PM25514.mod3best_11NMsum N(bestpred) = cntpred N(day)= cntdays mean(bestpred) = avgpm25 mean(predicted_m3) = predicted_m3 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;

proc means noprint data = PM25514.mod3best_11NMsum ;
var avgpm25;
by guid;
output out = PM25514.mod3best_11NMnodupsum  N(avgpm25)= cntpm25 mean(avgpm25) = avg11pm25 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;


data _null_;
     %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
     %let _EFIREC_ = 0;     /* clear export record count macro variable */
     file 'F:\pm25model_1k_5_14\mod3best_09NMnodupsum.csv' delimiter=',' DSD DROPOVER lrecl=32767;
     if _n_ = 1 then        /* write column names or labels */
      do;
        put
           "guid"
        ','
           "_TYPE_"
        ','
           "_FREQ_"
        ','
           "cntpm25"
        ','
           "avg09pm25"
        ','
           "long_aod"
        ','
           "lat_aod"
        ;
      end;
    set  PM25514.Mod3best_09nmnodupsum   end=EFIEOD;
        format guid best12. ;
        format _TYPE_ best12. ;
        format _FREQ_ best12. ;
        format cntpm25 best12. ;
        format avg09pm25 best12. ;
        format long_aod best12. ;
        format lat_aod best12. ;
      do;
        EFIOUT + 1;
        put guid @;
        put _TYPE_ @;
        put _FREQ_ @;
        put cntpm25 @;
        put avg09pm25 @;
        put long_aod @;
        put lat_aod ;
        ;
      end;
     if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
     if EFIEOD then call symputx('_EFIREC_',EFIOUT);
     run;





data PM25514.mod3best_11NMnodupgt75pct;
set PM25514.mod3best_11NMnodupsum;
where cntpm25 gt 273;
run;


data PM25514.mod3best_03NM                        ;
       %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
       infile '\\DROBO\Shared_Data\MAIAC_05_2014\mod3best_2003NM.csv' delimiter = ',' MISSOVER  DSD lrecl=32767 firstobs=2 ;
          informat VAR1 $5. ;
          informat day yymmdd10. ;
          informat guid best32. ;
          informat long_aod best32. ;
          informat lat_aod best32. ;
          informat predicted_m3 best32. ;
          informat predicted_m2 $2. ;
          informat PM25 $2. ;
          informat predicted_m1 $2. ;
          informat bestpred best32. ;
          format VAR1 $5. ;
          format day yymmdd10. ;
          format guid best12. ;
          format long_aod best12. ;
          format lat_aod best12. ;
          format predicted_m3 best12. ;
          format predicted_m2 $2. ;
          format PM25 $2. ;
          format predicted_m1 $2. ;
          format bestpred best12. ;
       input
                   VAR1 $
                   day
                   guid
                   long_aod
                   lat_aod
                   predicted_m3
                   predicted_m2 $
                   PM25 $
                   predicted_m1 $
                   bestpred
       ;
       if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
       run;



proc sort data = PM25514.mod3best_03NM;
by guid day;
run;

proc means noprint data = PM25514.mod3best_03NM;
by guid day;
var bestpred predicted_m3 long_aod lat_aod day;
output out = PM25514.mod3best_03NMsum N(bestpred) = cntpred N(day)= cntdays mean(bestpred) = avgpm25 mean(predicted_m3) = predicted_m3 min(bestpred) = minpm25 max(bestpred) = maxpm25 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;

proc means noprint data = PM25514.mod3best_03NMsum ;
var avgpm25 predicted_m3 long_aod lat_aod;
by guid;
output out = PM25514.mod3best_03NMnodupsum  N(avgpm25)= cntpm25 mean(avgpm25) = avg03pm25 mean(predicted_m3) = predicted_m3 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;

data PM25514.mod3best_03NMnodupgt75pct;
set PM25514.mod3best_03NMnodupsum;
where cntpm25 gt 273;
run;

data PM25514.mod3best_04NM                        ;
       %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
       infile '\\DROBO\Shared_Data\MAIAC_05_2014\mod3best_2004NM.csv' delimiter = ',' MISSOVER  DSD lrecl=32767 firstobs=2 ;
          informat VAR1 $5. ;
          informat day yymmdd10. ;
          informat guid best32. ;
          informat long_aod best32. ;
          informat lat_aod best32. ;
          informat predicted_m3 best32. ;
          informat predicted_m2 $2. ;
          informat PM25 $2. ;
          informat predicted_m1 $2. ;
          informat bestpred best32. ;
          format VAR1 $5. ;
          format day yymmdd10. ;
          format guid best12. ;
          format long_aod best12. ;
          format lat_aod best12. ;
          format predicted_m3 best12. ;
          format predicted_m2 $2. ;
          format PM25 $2. ;
          format predicted_m1 $2. ;
          format bestpred best12. ;
       input
                   VAR1 $
                   day
                   guid
                   long_aod
                   lat_aod
                   predicted_m3
                   predicted_m2 $
                   PM25 $
                   predicted_m1 $
                   bestpred
       ;
       if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
       run;



proc sort data = PM25514.mod3best_04NM;
by guid day;
run;

proc means noprint data = PM25514.mod3best_04NM;
by guid day;
var bestpred predicted_m3 long_aod lat_aod day;
output out = PM25514.mod3best_04NMsum N(bestpred) = cntpred N(day)= cntdays mean(bestpred) = avgpm25 mean(predicted_m3) = predicted_m3 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;

proc means noprint data = PM25514.mod3best_04NMsum ;
var avgpm25;
by guid;
output out = PM25514.mod3best_04NMnodupsum  N(avgpm25)= cntpm25 mean(avgpm25) = avg04pm25 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;

data PM25514.mod3best_04NMnodupgt75pct;
set PM25514.mod3best_04NMnodupsum;
where cntpm25 gt 273;
run;

data PM25514.mod3best_05NM                        ;
       %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
       infile '\\DROBO\Shared_Data\MAIAC_05_2014\mod3best_2005NM.csv' delimiter = ',' MISSOVER  DSD lrecl=32767 firstobs=2 ;
          informat VAR1 $5. ;
          informat day yymmdd10. ;
          informat guid best32. ;
          informat long_aod best32. ;
          informat lat_aod best32. ;
          informat predicted_m3 best32. ;
          informat predicted_m2 $2. ;
          informat PM25 $2. ;
          informat predicted_m1 $2. ;
          informat bestpred best32. ;
          format VAR1 $5. ;
          format day yymmdd10. ;
          format guid best12. ;
          format long_aod best12. ;
          format lat_aod best12. ;
          format predicted_m3 best12. ;
          format predicted_m2 $2. ;
          format PM25 $2. ;
          format predicted_m1 $2. ;
          format bestpred best12. ;
       input
                   VAR1 $
                   day
                   guid
                   long_aod
                   lat_aod
                   predicted_m3
                   predicted_m2 $
                   PM25 $
                   predicted_m1 $
                   bestpred
       ;
       if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
       run;



proc sort data = PM25514.mod3best_05NM;
by guid day;
run;

proc means noprint data = PM25514.mod3best_05NM;
by guid day;
var bestpred predicted_m3 long_aod lat_aod day;
output out = PM25514.mod3best_05NMsum N(bestpred) = cntpred N(day)= cntdays mean(bestpred) = avgpm25 mean(predicted_m3) = predicted_m3 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;

proc means noprint data = PM25514.mod3best_05NMsum ;
var avgpm25;
by guid;
output out = PM25514.mod3best_05NMnodupsum  N(avgpm25)= cntpm25 mean(avgpm25) = avg05pm25 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;

data PM25514.mod3best_05NMnodupgt75pct;
set PM25514.mod3best_05NMnodupsum;
where cntpm25 gt 273;
run;

data PM25514.mod3best_06NM                        ;
       %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
       infile '\\DROBO\Shared_Data\MAIAC_05_2014\mod3best_2006NM.csv' delimiter = ',' MISSOVER  DSD lrecl=32767 firstobs=2 ;
          informat VAR1 $5. ;
          informat day yymmdd10. ;
          informat guid best32. ;
          informat long_aod best32. ;
          informat lat_aod best32. ;
          informat predicted_m3 best32. ;
          informat predicted_m2 $2. ;
          informat PM25 $2. ;
          informat predicted_m1 $2. ;
          informat bestpred best32. ;
          format VAR1 $5. ;
          format day yymmdd10. ;
          format guid best12. ;
          format long_aod best12. ;
          format lat_aod best12. ;
          format predicted_m3 best12. ;
          format predicted_m2 $2. ;
          format PM25 $2. ;
          format predicted_m1 $2. ;
          format bestpred best12. ;
       input
                   VAR1 $
                   day
                   guid
                   long_aod
                   lat_aod
                   predicted_m3
                   predicted_m2 $
                   PM25 $
                   predicted_m1 $
                   bestpred
       ;
       if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
       run;



proc sort data = PM25514.mod3best_06NM;
by guid day;
run;

proc means noprint data = PM25514.mod3best_06NM;
by guid day;
var bestpred predicted_m3 long_aod lat_aod day;
output out = PM25514.mod3best_06NMsum N(bestpred) = cntpred N(day)= cntdays mean(bestpred) = avgpm25 mean(predicted_m3) = predicted_m3  min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;

proc means noprint data = PM25514.mod3best_06NMsum ;
var avgpm25;
by guid;
output out = PM25514.mod3best_06NMnodupsum  N(avgpm25)= cntpm25 mean(avgpm25) = avg06pm25 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;

data PM25514.mod3best_06NMnodupgt75pct;
set PM25514.mod3best_06NMnodupsum;
where cntpm25 gt 273;
run;

data PM25514.mod3best_07NM                        ;
       %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
       infile '\\DROBO\Shared_Data\MAIAC_05_2014\mod3best_2007NM.csv' delimiter = ',' MISSOVER  DSD lrecl=32767 firstobs=2 ;
          informat VAR1 $5. ;
          informat day yymmdd10. ;
          informat guid best32. ;
          informat long_aod best32. ;
          informat lat_aod best32. ;
          informat predicted_m3 best32. ;
          informat predicted_m2 $2. ;
          informat PM25 $2. ;
          informat predicted_m1 $2. ;
          informat bestpred best32. ;
          format VAR1 $5. ;
          format day yymmdd10. ;
          format guid best12. ;
          format long_aod best12. ;
          format lat_aod best12. ;
          format predicted_m3 best12. ;
          format predicted_m2 $2. ;
          format PM25 $2. ;
          format predicted_m1 $2. ;
          format bestpred best12. ;
       input
                   VAR1 $
                   day
                   guid
                   long_aod
                   lat_aod
                   predicted_m3
                   predicted_m2 $
                   PM25 $
                   predicted_m1 $
                   bestpred
       ;
       if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
       run;



proc sort data = PM25514.mod3best_07NM;
by guid day;
run;

proc means noprint data = PM25514.mod3best_07NM;
by guid day;
var bestpred predicted_m3 long_aod lat_aod day;
output out = PM25514.mod3best_07NMsum N(bestpred) = cntpred N(day)= cntdays mean(bestpred) = avgpm25 mean(predicted_m3) = predicted_m3 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;

proc means noprint data = PM25514.mod3best_07NMsum ;
var avgpm25;
by guid;
output out = PM25514.mod3best_07NMnodupsum  N(avgpm25)= cntpm25 mean(avgpm25) = avg07pm25 min(long_aod) = long_aod min(lat_aod) = lat_aod;
run;

data PM25514.mod3best_07NMnodupgt75pct;
set PM25514.mod3best_07NMnodupsum;
where cntpm25 gt 273;
run;

data _null_;
     %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
     %let _EFIREC_ = 0;     /* clear export record count macro variable */
     file 'F:\pm25model_1k_5_14\mod3best_10NMnodupsum.csv' delimiter=',' DSD DROPOVER lrecl=32767;
     if _n_ = 1 then        /* write column names or labels */
      do;
        put
           "guid"
        ','
           "_TYPE_"
        ','
           "_FREQ_"
        ','
           "cntpm25"
        ','
           "avg10pm25"
        ','
           "long_aod"
        ','
           "lat_aod"
        ;
      end;
    set  PM25514.Mod3best_10nmnodupsum   end=EFIEOD;
        format guid best12. ;
        format _TYPE_ best12. ;
        format _FREQ_ best12. ;
        format cntpm25 best12. ;
        format avg10pm25 best12. ;
        format long_aod best12. ;
        format lat_aod best12. ;
      do;
        EFIOUT + 1;
        put guid @;
        put _TYPE_ @;
        put _FREQ_ @;
        put cntpm25 @;
        put avg10pm25 @;
        put long_aod @;
        put lat_aod ;
        ;
      end;
     if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
     if EFIEOD then call symputx('_EFIREC_',EFIOUT);
     run;
	 
	 data _null_;
     %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
     %let _EFIREC_ = 0;     /* clear export record count macro variable */
     file 'F:\pm25model_1k_5_14\mod3best_11NMnodupsum.csv' delimiter=',' DSD DROPOVER lrecl=32767;
     if _n_ = 1 then        /* write column names or labels */
      do;
        put
           "guid"
        ','
           "_TYPE_"
        ','
           "_FREQ_"
        ','
           "cntpm25"
        ','
           "avg11pm25"
        ','
           "long_aod"
        ','
           "lat_aod"
        ;
      end;
    set  PM25514.Mod3best_11nmnodupsum   end=EFIEOD;
        format guid best12. ;
        format _TYPE_ best12. ;
        format _FREQ_ best12. ;
        format cntpm25 best12. ;
        format avg11pm25 best12. ;
        format long_aod best12. ;
        format lat_aod best12. ;
      do;
        EFIOUT + 1;
        put guid @;
        put _TYPE_ @;
        put _FREQ_ @;
        put cntpm25 @;
        put avg11pm25 @;
        put long_aod @;
        put lat_aod ;
        ;
      end;
     if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
     if EFIEOD then call symputx('_EFIREC_',EFIOUT);
     run;
	 
	 data _null_;
     %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
     %let _EFIREC_ = 0;     /* clear export record count macro variable */
     file 'F:\pm25model_1k_5_14\mod3best_03NMnodupsum.csv' delimiter=',' DSD DROPOVER lrecl=32767;
     if _n_ = 1 then        /* write column names or labels */
      do;
        put
           "guid"
        ','
           "_TYPE_"
        ','
           "_FREQ_"
        ','
           "cntpm25"
        ','
           "avg03pm25"
        ','
           "long_aod"
        ','
           "lat_aod"
        ;
      end;
    set  PM25514.Mod3best_03nmnodupsum   end=EFIEOD;
        format guid best12. ;
        format _TYPE_ best12. ;
        format _FREQ_ best12. ;
        format cntpm25 best12. ;
        format avg03pm25 best12. ;
        format long_aod best12. ;
        format lat_aod best12. ;
      do;
        EFIOUT + 1;
        put guid @;
        put _TYPE_ @;
        put _FREQ_ @;
        put cntpm25 @;
        put avg03pm25 @;
        put long_aod @;
        put lat_aod ;
        ;
      end;
     if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
     if EFIEOD then call symputx('_EFIREC_',EFIOUT);
     run;
	 
	 data _null_;
     %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
     %let _EFIREC_ = 0;     /* clear export record count macro variable */
     file 'F:\pm25model_1k_5_14\mod3best_04NMnodupsum.csv' delimiter=',' DSD DROPOVER lrecl=32767;
     if _n_ = 1 then        /* write column names or labels */
      do;
        put
           "guid"
        ','
           "_TYPE_"
        ','
           "_FREQ_"
        ','
           "cntpm25"
        ','
           "avg04pm25"
        ','
           "long_aod"
        ','
           "lat_aod"
        ;
      end;
    set  PM25514.Mod3best_04nmnodupsum   end=EFIEOD;
        format guid best12. ;
        format _TYPE_ best12. ;
        format _FREQ_ best12. ;
        format cntpm25 best12. ;
        format avg04pm25 best12. ;
        format long_aod best12. ;
        format lat_aod best12. ;
      do;
        EFIOUT + 1;
        put guid @;
        put _TYPE_ @;
        put _FREQ_ @;
        put cntpm25 @;
        put avg04pm25 @;
        put long_aod @;
        put lat_aod ;
        ;
      end;
     if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
     if EFIEOD then call symputx('_EFIREC_',EFIOUT);
     run;
	 
	 data _null_;
     %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
     %let _EFIREC_ = 0;     /* clear export record count macro variable */
     file 'F:\pm25model_1k_5_14\mod3best_05NMnodupsum.csv' delimiter=',' DSD DROPOVER lrecl=32767;
     if _n_ = 1 then        /* write column names or labels */
      do;
        put
           "guid"
        ','
           "_TYPE_"
        ','
           "_FREQ_"
        ','
           "cntpm25"
        ','
           "avg05pm25"
        ','
           "long_aod"
        ','
           "lat_aod"
        ;
      end;
    set  PM25514.Mod3best_05nmnodupsum   end=EFIEOD;
        format guid best12. ;
        format _TYPE_ best12. ;
        format _FREQ_ best12. ;
        format cntpm25 best12. ;
        format avg05pm25 best12. ;
        format long_aod best12. ;
        format lat_aod best12. ;
      do;
        EFIOUT + 1;
        put guid @;
        put _TYPE_ @;
        put _FREQ_ @;
        put cntpm25 @;
        put avg05pm25 @;
        put long_aod @;
        put lat_aod ;
        ;
      end;
     if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
     if EFIEOD then call symputx('_EFIREC_',EFIOUT);
     run;
	 
	 data _null_;
     %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
     %let _EFIREC_ = 0;     /* clear export record count macro variable */
     file 'F:\pm25model_1k_5_14\mod3best_06NMnodupsum.csv' delimiter=',' DSD DROPOVER lrecl=32767;
     if _n_ = 1 then        /* write column names or labels */
      do;
        put
           "guid"
        ','
           "_TYPE_"
        ','
           "_FREQ_"
        ','
           "cntpm25"
        ','
           "avg06pm25"
        ','
           "long_aod"
        ','
           "lat_aod"
        ;
      end;
    set  PM25514.Mod3best_06nmnodupsum   end=EFIEOD;
        format guid best12. ;
        format _TYPE_ best12. ;
        format _FREQ_ best12. ;
        format cntpm25 best12. ;
        format avg06pm25 best12. ;
        format long_aod best12. ;
        format lat_aod best12. ;
      do;
        EFIOUT + 1;
        put guid @;
        put _TYPE_ @;
        put _FREQ_ @;
        put cntpm25 @;
        put avg06pm25 @;
        put long_aod @;
        put lat_aod ;
        ;
      end;
     if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
     if EFIEOD then call symputx('_EFIREC_',EFIOUT);
     run;

data _null_;
     %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
     %let _EFIREC_ = 0;     /* clear export record count macro variable */
     file 'F:\pm25model_1k_5_14\mod3best_07NMnodupsum.csv' delimiter=',' DSD DROPOVER lrecl=32767;
     if _n_ = 1 then        /* write column names or labels */
      do;
        put
           "guid"
        ','
           "_TYPE_"
        ','
           "_FREQ_"
        ','
           "cntpm25"
        ','
           "avg07pm25"
        ','
           "long_aod"
        ','
           "lat_aod"
        ;
      end;
    set  PM25514.Mod3best_07nmnodupsum   end=EFIEOD;
        format guid best12. ;
        format _TYPE_ best12. ;
        format _FREQ_ best12. ;
        format cntpm25 best12. ;
        format avg07pm25 best12. ;
        format long_aod best12. ;
        format lat_aod best12. ;
      do;
        EFIOUT + 1;
        put guid @;
        put _TYPE_ @;
        put _FREQ_ @;
        put cntpm25 @;
        put avg07pm25 @;
        put long_aod @;
        put lat_aod ;
        ;
      end;
     if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
     if EFIEOD then call symputx('_EFIREC_',EFIOUT);
     run;


data pm25514.mod3best_05nmnodup (keep = guid day predpm25 long_aod lat_aod);
set pm25514.mod3best_05nmsum;
rename avgpm25 = predpm25;
run;
data pm25514.mod3best_11nmnodup (keep = guid day predpm25 long_aod lat_aod);
set pm25514.mod3best_11nmsum;
rename avgpm25 = predpm25;
run;


