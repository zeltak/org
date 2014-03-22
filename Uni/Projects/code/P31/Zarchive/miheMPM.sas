/* Preparing grid cells for stage 3 modeling. 
1. Make the list of PM stations within 10 Km of each grid cell using the generate near table tool.
2. Using those list, calculate the daily mean of them
3. Merge with predictions from stage 2. */

libname aod 'G:\Mihye\Thesis 2\AOD'; /* To make the date list */
libname stage2 'G:\Mihye\Thesis 2\Stage2_Predictions'; /* Predictions from stage 2 */
libname mpm 'C:\Data\Thesis 2\Data\Stage 3\Mean PM';
libname stage3 'C:\Data\Thesis 2\Data\Stage 3\Stage3_Grids';
libname PM 'C:\Data\Thesis 2\Data\PM25';

/* proc import out=Grid datafile='C:\Data\Thesis 2\Data\Grids\GID.dbf' dbms=dbf replace; getdeleted=no; run; 
proc sort data=grid; by gid; run;
If 1 is not met and want to all empy grid cells */

/* 1. Bring the table which has the PM station list for each grid cells. Made by ArcGIS using the Generate Near Table tool. */
proc import out=PM_100Km datafile='P:\P031_MIAC_PM\3.Work\2.Gather_data\FN015_MPMGID_PM_Stns_100Km.dbf' dbms=dbf replace; getdeleted=no; run;


XXXX100km dbf is all guid within 100km of sitecode see image of mihye

option mprint;
%macro stage3_grid();
	%do i=2004 %to 2004;
		proc datasets library=mpm; delete pm_10km_ave_&i;

		/* 2. Making date list for each year for one grand loop: Start */
		proc sort data =aod.aod_mod2_&i nodupkey out=AOD_Date(keep=date); by date; run;
XXXXX create a list needs to be afull TS every day every grid for study areea


		

		data date_list(keep = list list_new date);
			length list $ 30000. list_new $ 30000.;
			retain list_new;
			set aod_date;
			if _n_ = 1 then do;
			list = trim(left(date));
			list_new=list;
			call symputx("List", list_new);
			output;
			end;
			if _n_ > 1 then do;
				list = trim(left(list_new))||" " || trim(left(date));
				list_new = list;
				call symputx("List", list_new);
				output;
			end;
		run;
XXXX creates a list of dates



		%let Type=;
		%let DateList=;
		%let Cycle=;
		%let ID=;


/* 2. Making date list for each year for one grand loop: End */



/* 3. Calculate the daily mean of PM2.5 measurement within 100 km of each grid cell: Start 
				 (If there was no PM2.5 stations within 100 Km OR no measurement of a specific day, just ignore them) */
				/* data grid_pm;	merge grid(in=a) pm_100km(in=b); by gid; date=&date; run;  If 1 is not met and want to all empy grid cells */

%macro mean_neighbors(DateList=);
			%let j=1;

			%do %while (%scan(&DateList, &j) ne);
 			%let Cycle=%scan(&DateList, &j);

			%macro AOD(date=);


			/* dm 'clear log'; */



				
				data PM_&date;
					length Sitecode $9.;
					set pm.pm_all(rename=(sitecode=sitecode2));
					if date=&date;
					sitecode=sitecode2;
					keep date sitecode pm25;
				run;

				proc sort data=pm_100km; by sitecode; run;
				proc sort data=pm_&date; by sitecode; run;

				data Grid_Actual_PM; merge pm_100km(in=a) pm_&date(in=b); by sitecode; if a; run;

				proc summary nway data=grid_actual_pm;
					class gid;
					var pm25;
					output out=pm_10km_ave(drop=_type_ _freq_) mean=mpm;
				run;

				data pm_10km_ave; set pm_10km_ave; format Date date9.; date=&date; if mpm^=.; run;

				proc append base=mpm.pm_10km_ave_&i data=pm_10km_ave; run;
			
			%mend AOD;
			%AOD(date = &Cycle);

			%let j=%eval(&j+1);
			%end;
		%mend;
		%mean_neighbors(DateList=&List);

	/* 3. Calculate the daily mean of PM2.5 measurement within 10 Km of each grid cell: End */



		/* 4. Merge with the predictions from stage 2: Again, discard grid cells with no mean PM: Start */
		proc sort data=mpm.pm_10km_ave_&i; by date gid; run;
		proc sort data=stage2.stage2_&i; by date gid; run;

		data stage3.grids_stage3_&i;
			merge mpm.pm_10km_ave_&i(in=a) stage2.stage2_&i(in=b);
			by date gid;
			if a and b;
		run;
		/* 4. Merge with the predictions from stage 2: Again, discard grid cells with no mean PM: End */

	%end;
%mend;
%stage3_grid();


/* Appendix: check whether worked right or not: Start */
/*
proc print data=pm_100km;
	where gid=720541;
run;

data pm;
	set pm.pm_all;
	if sitecode in ('131210032', '131210039', '131210048') and date='01jan04'd;
run;
/* Appendix: check whether worked right or not: End */

