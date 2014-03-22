libname data "C:\Models\3.Data\New Monitors";

%macro year;

%do year = 2000 %to 2011;

proc freq data = data.unico&year;
 table station / list;
  ods output onewayfreqs = data.Station&year;
run;

%end;

%mend;

%year;



/**** Creo la Serie completa *****/

data seriesj;
 input Date ddmmyy10. Value;
  format Date ddmmyy10.;
cards;
01/01/2000 1
31/12/2011 1
run;

proc expand data = seriesj out=daily to=day method=step;
  convert Value  = daily_Value;
  id date;
run;

option mprint;
%macro year(year = );

data daily_&year;
 set daily;
  if "01JAN&year"d <= date <= "31DEC&year"d;
run;


data OneWayFreqs_&year(keep = Station);
 set data.Station&year; 
run;


data id_elenco(keep = elenco elenco_new date);
  length elenco $ 30000. elenco_new $ 30000. ;
   retain elenco_new;
   set daily_&year;
     if _n_ = 1 then do;
        elenco = trim(left(Date));
        elenco_new = elenco;
                     output;
     end;
     if _n_ > 1 then do;
      elenco = trim(left(elenco_new))||" " || trim(left(Date));
      elenco_new = elenco;
       call symputx("Lista",elenco_new);
      output;
     end;
run;

DM 'ODSRESULTS' CLEAR EDITOR; ODS HTML CLOSE; 
DM 'CLEAR LOG; CLEAR OUTPUT; PGM OFF' LISTING;

%macro full(List = );

%let j=1;

%do %while (%scan(&List,&j) ne);
 %let Date = %scan(&List,&j);

data Daily&date;
 set daily_&year;
  where date = &date;
run;

data Daily&date;
  if _N_ = 1 then set Daily&date;
 set OneWayFreqs_&year;
run;

proc append base = Final_&year data = Daily&date force;
run;

proc datasets lib=work; delete id_elenco Daily&date; run;

%let j=%eval(&j+1);
%end;

%mend full;

%full(List = &Lista);

%mend;


%year(year = 2000);
%year(year = 2001);
%year(year = 2002);
%year(year = 2003);
%year(year = 2004);
%year(year = 2005);
%year(year = 2006);
%year(year = 2007);
%year(year = 2008);
%year(year = 2009);
%year(year = 2010);
%year(year = 2011);


option mprint;

%macro year(year = );

proc sort data = Check.Unico&year;  by date station; run;
proc sort data = Final_&year;       by date station; run;

data data.Italy_&year(drop = year daily_Value);
 merge Check.Unico&year(in=a) Final_&year(in=b);   
 by date station; 
 if b;
run;

proc datasets lib = work; delete Final_&year OneWayFreqs_&year Italy_&year Daily_&year; run;quit;

%mend;

%year(year = 2000);
%year(year = 2001);
%year(year = 2002);
%year(year = 2003);
%year(year = 2004);
%year(year = 2005);
%year(year = 2006);
%year(year = 2007);
%year(year = 2008);
%year(year = 2009);
%year(year = 2010);
%year(year = 2011);

proc datasets lib = work; delete Daily Italy List Seriesj Station; run;quit;

