data NYC;
set mods.fig5;
where  station="KVADAYT" ;
run;
/*create data for 2008*/
PROC IMPORT OUT= m1_met_2008 (keep= date station tempc)
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN003_WUNCDC yearly\met2008.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
PROC IMPORT OUT= stn_closest_XXYY_2008
            DATAFILE= "f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\2.Gather_data\FN007_Key_tables\yearly_met_xy\fullgrid_stn_2008.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 
/*to leave only THE 1 closest sat data point to station in each day*/
proc sort data=stn_closest_XXYY_2008; by station  dist;
data stn_closest_XXYY_2008; set stn_closest_XXYY_2008; by station  dist;
if first.station;
run;
proc sort data = m1_met_2008; by station   ;run;
proc sort data = stn_closest_XXYY_2008 ; by station ;run;
data m1_met_2008;
merge m1_met_2008(in=a) stn_closest_XXYY_2008 (in=b)  ;
  by station;
    if a;
	run; 
data m1_met_2008 (drop= xx yy);
set m1_met_2008;
glong= round(xx,0.00001);
glat= round(yy,0.00001);
run; 
libname mods 'f:\Uni\Projects\P020_Temprature_NE_MIA\3.Work\3.Analysis\AN_001_mods\' ;
proc sql;
  create table m3_cor_resxy as
   select *
    from m1_met_2008 left join mods.Fintmpc_2008
     on m1_met_2008.glong = Fintmpc_2008.glong and m1_met_2008.glat = Fintmpc_2008.glat and  m1_met_2008.date = Fintmpc_2008.date;
run;

data mods.fig5;
set m3_cor_resxy;
run; 

/*create 2 cities*/

data NYC;
set mods.fig5;
where  station="KNYC" ;
run; 

proc means data=NYC n min max mean std nmiss;
var ; 
run; 

symbol1 v=none i=sm1 c=red h=1 w=1;
symbol2 v=none i=sm1 c=black h=1 w=1;

proc gplot data = m3_cor_resxy;
 plot fintemp*date tempc*date / overlay grid;
  where station="MVA033";
run;
quit;

proc corr data = m3_cor_resxy;
 var fintemp tempc;
   where station="MVA033";
   run; 




data BOS;
set m3_cor_resxy;
where station="725090" or
station="KBOS"   or
station="KMAARLI" or
station="KMACAMB" or
station="KMADEDH" or
station="KMALEXI" or
station="KMAMALD" or
station="KMAMEDF" or
station="KMAMELR" or
station="KMANEED" or
station="KMAWALT" or
station="KMAWELL" or
station="KMAWINT" or
station="KMAWOBU";
run;

data NYC;
set mods.fig5;
where  station="KFDK" ;
run; 

proc means data=NYC n min max mean std nmiss;
var ; 
run; 
