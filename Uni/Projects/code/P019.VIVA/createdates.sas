




* Create dataset 'vivacwy.vivaexpdates';
        * variable 'date' has each day from Nov 13 1998 to Sep 1 2003 as an observation, for each aid;
        * variable 'hrint' has hour from 0 to 23 for each date;

data vivacwy.vivaexpdays;
  set vivacwy.exposuredates;
  by aid;
  retain date;
  if first.aid then do;
    date=mdy(11,12,1998);

          do while (date <= mdy(9,1,2003));
        /*do while (date <= mdy(3,31,1999));*/

                /*hrint = 0;
                        do while  (hrint < 24);
                                output;
                                hrint = hrint+1;
                        end;*/
                        output;
                        date=date+1;
          end;
        end;
format date mmddyy10.;
run;
