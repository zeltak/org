libname NOAA  "C:\Models\3.Data\Meteo NOAA";
libname WU    "C:\Models\3.Data\WU-IT";
libname Final "C:\Models\3.Data\Meteo NOAA + WU Italy";

%macro year;

%do year = 2000 %to 2011;

data NOAA&year(keep = USAF WBAN Date Temperature Station_Name Lat_Station Long_Station);
 set NOAA.Italy_&year;
  Temperature  = Temp_C;
  Station_Name = NAME;
  Lat_Station  = Lat;
  Long_Station = Lon;
run;

data Monitor_&year(keep = Station Date Temperature Station_Name Lat_Station Long_Station);
 set WU.Monitor_&year;
run;

data unico&year; set NOAA&year Monitor_&year; run;

proc sort data = unico&year out = station&year(keep = Station_Name Lat_Station Long_Station) nodupkey; by Lat_Station Long_Station; run;

proc sort data = station&year; by Station_Name; run;

data station&year;
  set station&year;
    ID_station + 1;
   by Station_Name;
  if first.Station_Name then count = 1;
run;

proc sort data = unico&year; by Station_Name; run;

data unico&year(drop =  count Station);
 merge unico&year station&year;
  by Station_Name;
run;
quit;

data Final.unico&year;
 retain id_station Station_Name Lat_Station Long_Station Date Temperature;
  set unico&year;
   if Station_Name = "LAMPEDUSA" then delete;
run;

proc datasets lib = work; delete Noaa&year Station&year Monitor_&year ;run;

%end;

%mend;

%year;

proc sort data = Final.Unico2005 nodupkey out = Final.Monitor_Station(keep = id_station USAF WBAN Station_Name Lat_Station Long_Station);  by ID_station; run;


