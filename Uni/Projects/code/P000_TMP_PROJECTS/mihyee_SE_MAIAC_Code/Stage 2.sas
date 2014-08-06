%put %sysfunc(getoption(work));

libname mod1 'C:\Data\Thesis 2\Data\6. 1st Stage Model';
libname S2_Data 'E:\Data\Thesis 2\Stage 2\Stage 2 Data';
libname S2_Pred 'E:\Data\Thesis 2\Stage 2\Pred 2';

%macro stage2();
	%do i=2003 %to 2011;
		%do j=1 %to 5;
			data stage2;
				set s2_data.stage2_&i;
				if group_r=&j and water=0 and aod<1.5 and pop>9;
			run;

			data Stage1_&i._&j;
				set mod1.stage1_&i._ipw(where=(group_r=&j));
				if water=0 and pm25<80 and aod<1.5 and pop>9;
			run;

			proc mixed data=stage1_&i._&j method=reml plots(maxpoints=none);
				class date region2;
				weight ipw_n;
				model pm25=aod temp dewp slp visib wdsp ah_gm3 ndvi pcturb_1km elev_m mjrrdden_1km pop_10km nei05nonpntcntypm25
 										pbl emsn_pt canopy01 dist_a1 dist_air dist_port dist_rail dist_rd dist_bldg pm10_pt nox aod*pbl/s;
				random intercept aod temp/subject=date s;
				random intercept aod/subject=date(region2) s;
				ods output  SolutionF=sol_Fix_m1_&i._&j;
				ods output  SolutionR=sol_Ran_m1_&i._&j;
			run;
			quit;

			/*Step s1*/
			/*1)GET THE Overall AOD TEMP SLOPES + INTERCEPT */
			data check_s1_int(keep=date Ovr_Int);
				set sol_ran_m1_&i._&j;
				if effect= "Intercept" and date>0;
				if region2<0;
				Ovr_Int = Estimate;
			run;

			data  check_s1_AOD(keep=date Ovr_AOD);
				set Sol_Ran_m1_&i._&j;
				if Effect= "aod" and date>0;
				if region2<0;
				Ovr_AOD = Estimate;
			run;

			data check_s1_Temp(keep = date Ovr_Temp);
				set Sol_Ran_m1_&i._&j;
				if Effect= "TEMP" and date>0;
				if region2<0;
				Ovr_Temp=Estimate;
			run;

			proc sort data = check_s1_Int;  by date;run;
			proc sort data = check_s1_AOD;  by date;run;
			proc sort data = check_s1_Temp; by date;run;

			data mean_s1;
				merge check_s1_Int check_s1_AOD check_s1_Temp;
				by date;
			run;

			/*** Join the Overall slope and intercept with 200% dataset ***/
			proc sort data=stage2; by date;run;
			proc sort data = mean_s1; by date;run;

			data stage2;
				merge stage2(in=a) mean_s1(in=b);
				by date;
				if a;
			run; 

			/* 2)GET THE region specific AOD TEMP SLOPES + INTERCEPT */
			data transp_1_s1;
				set  Sol_Ran_m1_&i._&j;
				keep Effect region2 date Estimate;
				if region2 > 0;
			run;

			proc sort data=transp_1_s1; by date region2; run;

			proc transpose data=transp_1_s1 out=transp_1_s1;
				by date region2;
				id Effect;
			run;

			data transp_1_s1(drop=AOD Intercept);
				set transp_1_s1;
				reg_AOD=AOD;
				reg_Int=Intercept;
			run;

			/*** Join the region specific slope and intercept with 200% dataset ***/
			proc sort data=stage2; by date region2; run;
			proc sort data=transp_1_s1; by date region2;run;

			data stage2(drop=_name_);
				merge stage2(in=a) transp_1_s1(in=b);
				by date region2;
				if a;
			run; 

			/* Assign Fixed Effect */
			proc transpose data = Sol_Fix_m1_&i._&j prefix=fix_ out=transp_3_s1;
				id Effect;
			run;

			data transp_3_s1(drop=_label_);
				set transp_3_s1;
				if _name_ = "Estimate";
			run;

			data stage2;
				set stage2;
				if _n_=1 then set transp_3_s1;
			run ;

			data stage2;
				set stage2;
				if reg_int=. then reg_int=0;
				if reg_aod=. then reg_aod=0;
				if ovr_int=. then ovr_int = 0;
				if ovr_aod=. then ovr_aod=0;
				if ovr_temp=. then ovr_temp=0;
				AOD_PBL=aod*pbl;
			run;

			data s2_pred.Stage2_Pred_&i._&j;
				set stage2;
				Pred2=fix_intercept+
							aod*fix_aod+
							temp*fix_temp+
							dewp*fix_DEWP+
							slp*fix_SLP+
							visib*fix_VISIB+
							wdsp*fix_WDSP+
							ah_GM3*fix_ah_gm3+
							ndvi*fix_NDVI+
							pcturb_1km*fix_pcturb_1km+
							elev_m*fix_elev_m+
							mjrrdden_1km*fix_Mjrrdden_1km+
							pop_10km*fix_Pop_10km+
							nei05nonpntcntypm25*fix_nei05nonpntcntypm25+
							pbl*fix_pbl+
							emsn_pt*fix_Emsn_Pt+
							canopy01*fix_Canopy01+
							dist_a1*fix_Dist_A1+
							dist_air*fix_Dist_Air+
							dist_port*fix_Dist_Port+
							dist_rail*fix_Dist_Rail+
							dist_rd*fix_Dist_Rd+
							dist_bldg*fix_Dist_Bldg+
							pm10_Pt*fix_PM10_Pt+ 
							nox*fix_NOX+
							aod_pbl*fix_aod_pbl+
							oVR_int+
							AOD*OVR_aod+
							temp*OVR_temp+
							reg_int+
							reg_aod*AOD;
				if pred2>0;
				* keep date lat_aod long_aod gid region2 group_r pop pred2;
			run;

			dm 'clear output';
		%end;
	%end;
%mend;
%stage2;

data _null_;
	call sound(659,200); call sound(587,200); call sound(523,200); call sound(587,200);	call sound(659,200);
	call sound(659,200); call sound(659,200); call sound(587,200); call sound(587,200); call sound(587,200);
	call sound(659,200); call sound(659,200); call sound(659,200); call sound(659,200); call sound(587,200);
	call sound(523,200); call sound(587,200); call sound(659,200); call sound(659,200); call sound(659,200);
	call sound(587,200); call sound(587,200); call sound(659,200); call sound(587,200); call sound(523,200);
run;
							/* pctop_1km*fix_pctop_1km+ */
