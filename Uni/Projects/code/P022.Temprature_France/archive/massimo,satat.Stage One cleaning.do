
set mem 500m

***** OBJECTIVE OF THE FIRST DO. FILE IS TO OPEN THE ORIGINAL STAGE 0NE FILE, CLEAN IT AND 
***** CREATE A REVISED FILE WITH DATA CHECKING AND EXCLUSION CRITERIA

***** Note: if the file to be opened has all variables in capital letters, open it in Stata 12 and apply the following:
*     rename  POINT_X_SITE- PM25_2005, lower
***** then save it in Stata 11 and proceed.


*use "F:\SATELLITE\stage one\2010\aod_stage_one_2010.dta", clear
use "F:\SATELLITE\stage one\2009\stage_one_2009.dta", clear
count
des, short
* 169,433 x 166 in 2010
* 183,960 x 166 in 2009


*** FIRST PART: CHECK AOD AND PM10, AND EXCLUDE ANOMALOUS OBSERVATIONS

* WARNING: FORE SOME YEARS (EX. 2010) ONLY DAYS WITH PM10 DATA ARE IN THE FILE, WHILE FOR OTHER YEARS (EX. 2009)
*          FULL TIME SERIES FOR ALL SITES ARE IN THE FILE, INCLUDING DAYS WITHOUT PM10 VALUE!

sum aod, det
*hist aod
scalar define aod_50th = r(p50)
scalar define aod_90th = r(p90)

* Count strange AOD values
count if aod>=1.2& aod<.
* 7 in 2010

sum pm10 if aod!=., det
scalar define pm10_50th = r(p50)
scalar define pm10_90th = r(p90)

corr aod pm10

* Count if aod smaller than median and PM10 greater than 90th pct
count if aod<aod_50th & (pm10>=pm10_90th & pm10<.)
corr aod pm10 if (aod>=aod_50th | pm10<pm10_90th)

* Count if PM10 smaller than median and AOD greater than 90th pct
count if (aod>aod_90th & aod<.) & pm10<pm10_50th
corr aod pm10 if (aod<=aod_90th | pm10>=pm10_50th)

* Compute correlations without anomalous observations
corr aod pm10 if (aod>=aod_50th | pm10<pm10_90th) & (aod<=aod_90th | pm10>=pm10_50th)
corr aod pm10 if ((aod>=aod_50th | pm10<pm10_90th) & (aod<=aod_90th | pm10>=pm10_50th)) & aod<1.2

* Count how many observations there are for each monitor with available AOD values. Then I will exclude monitors with < 30 observations
bysort site: egen conta_aod=count(aod)

* Create an inclusion/exclusion variable based on previous criteria

* create new variables and initialize to zero
gen excl_obs=0

* put var to 1 when either aod or PM10 is missing
replace excl_obs=1 if aod==. | pm10==.

* put var to 2 when AOD below median and PM10 above 90th pct
replace excl_obs=2 if aod<aod_50th & (pm10>=pm10_90th & pm10<.)

* put var to 3 when PM10 below median and AOD above 90th pct
replace excl_obs=3 if (aod>aod_90th & aod<.) & pm10<pm10_50th

* put var to 4 when AOD very large (above 1200)
replace excl_obs=4 if excl_obs==0 & aod>1.2

* put var to 5 when available observations for one monitor below 30
replace excl_obs=5 if excl_obs==0 & conta_aod<30

* table of the new variable
tab excl_obs, m

drop conta_aod

count if excl_obs==0
* 62,368 record included in the STAGE 1 analysis for 2010
corr pm10 aod if excl_obs==0
* 0.3476 in 2010


*** SECOND PART: I START NOW WITH DESCRIPTIVES OF SPATIAL PREDICTORS OF THE CELLS


* Type of zona
tab zona if excl_obs==0
tab nome_zona if excl_obs==0

* Population
sum restot if excl_obs==0
count if restot==0 & excl_obs==0
count if restot==.

* Indicator of presence of point emission sources
tab emip if excl_obs==0, m

* summary of point and areal emissions
sum  so2_2000p- pm10_2010a if excl_obs==0
* NH3 point emissions are always missing for 2000 and 2010, thus only 2005 data are valid
* All other point emissions seems unvalid for 2000 (too small) and 2005 (too big). Therefore I use only 2010 data
* All areal emissions variables look OK, therefore I keep only 2010 data

* I drop 2000 and 2005 point emissions for all emissions (except NH3), then I replace to zero when missing
drop  so2_2000p so2_2005p nox_2000p nox_2005p co_2000p co_2005p nh3_2000p nh3_2010p pm10_2000p pm10_2005p
sum  so2_2010p nox_2010p co_2010p nh3_2005p pm10_2010p if excl_obs==0
foreach var of varlist so2_2010p nox_2010p co_2010p nh3_2005p pm10_2010p {
replace `var'=0 if `var'==.
}

* I drop 2000 and 2005 areal emissions
drop  so2_2000a so2_2005a nox_2000a nox_2005a co_2000a co_2005a nh3_2000a nh3_2005a pm10_2000a pm10_2005a

* Land cover variables: summary and replace to zero when missing. Also replace to 100 when > 100
set more off
sum pct* if excl_obs==0, det
foreach var of varlist pct_deciduous- pct_low_dev {
replace `var'=0 if `var'==.
replace `var'=100 if `var'>100 & `var'<.
}

* Impervious surfaces
sum isa if excl_obs==0

* Roads: summary and replace to zero when missing
sum  length_a1 length_a23 length_oth if excl_obs==0
foreach var of varlist length_a1 length_a23 length_oth {
replace `var'=0 if `var'==.
}

* Proximity to features
sum  near_airport near_port near_sea near_a1 near_a2 near_a3 if excl_obs==0
tab flag_sea if excl_obs==0
tab flag_sea_buffer if excl_obs==0
tab flag_lake if excl_obs==0
tab flag_lake_buffer if excl_obs==0

* Elevation
sum elevation if excl_obs==0

* PM2.5 dispersion model 4x4 (year 2005)
sum pm25_2005
sum pm25_2005 if excl_obs==0



*** THIRD PART: I START NOW WITH DESCRIPTIVES OF SPATIAL PREDICTORS OF THE SURROUNDING CELLS ("r_" variables)


* Number of surrounding cells (max 8)
tab  r_ncells, m
tab  r_ncells if excl_obs==0, m

* Population: sum and mean of surrounding cells
sum  r_sum_restot r_mean_restot if excl_obs==0

* Summary of point emissions (sum over the surrounding cells), then I drop 2000 and 2005 data and replace missing to zero
sum  r_sum_so2_2000p- r_sum_pm10_2010p if excl_obs==0
drop r_sum_so2_2000p r_sum_so2_2005p r_sum_nox_2000p r_sum_nox_2005p r_sum_co_2000p r_sum_co_2005p r_sum_nh3_2000p r_sum_nh3_2005p r_sum_pm10_2000p r_sum_pm10_2005p 
foreach var of varlist  r_sum_so2_2010p r_sum_nox_2010p r_sum_co_2010p r_sum_nh3_2010p r_sum_pm10_2010p {
replace `var'=0 if `var'==.
}

* Summary of point emissions (mean over the surrounding cells), then I drop 2000 and 2005 data and replace missing to zero
sum  r_mean_so2_2000p- r_mean_pm10_2010p if excl_obs==0
drop r_mean_so2_2000p r_mean_so2_2005p r_mean_nox_2000p r_mean_nox_2005p r_mean_co_2000p r_mean_co_2005p r_mean_nh3_2000p r_mean_nh3_2005p r_mean_pm10_2000p r_mean_pm10_2005p
foreach var of varlist r_mean_so2_2010p r_mean_nox_2010p r_mean_co_2010p r_mean_nh3_2010p r_mean_pm10_2010p {
replace `var'=0 if `var'==.
}

* Summary of areal emissions (mean over the surrounding cells), then I drop 2000 and 2005 data
sum  r_mean_so2_2000a- r_mean_pm10_2010a if excl_obs==0
drop  r_mean_so2_2000a r_mean_so2_2005a r_mean_nox_2000a r_mean_nox_2005a r_mean_co_2000a r_mean_co_2005a r_mean_nh3_2000a r_mean_nh3_2005a r_mean_pm10_2000a r_mean_pm10_2005a

* Land cover variables: summary and replace to zero when missing
sum r_pct_deciduous- r_pct_low_dev if excl_obs==0
foreach var of varlist r_pct_deciduous- r_pct_low_dev {
replace `var'=0 if `var'==.
}
sum r_sum_area_deci r_sum_area_ever r_sum_area_crop r_sum_area_past r_sum_area_shru r_sum_area_hdev r_sum_area_ldev  if excl_obs==0
foreach var of varlist r_sum_area_deci r_sum_area_ever r_sum_area_crop r_sum_area_past r_sum_area_shru r_sum_area_hdev r_sum_area_ldev {
replace `var'=0 if `var'==.
}

* Roads: summary and replace to zero when missing
sum r_sum_length_a1 r_sum_length_a23 r_sum_length_oth if excl_obs==0
foreach var of varlist  r_sum_length_a1 r_sum_length_a23 r_sum_length_oth {
replace `var'=0 if `var'==.
}
sum  r_mean_length_a1 r_mean_length_a23 r_mean_length_oth if excl_obs==0
foreach var of varlist r_mean_length_a1 r_mean_length_a23 r_mean_length_oth {
replace `var'=0 if `var'==.
}

* Impervious surfaces
sum  r_sum_isa r_mean_isa if excl_obs==0

* Elevation
sum  r_mean_elevation if excl_obs==0


*** FOURTH PART: I START NOW WITH DESCRIPTIVES OF SPATIO-TEMPORAL PREDICTORS


* Temperature
count if temp_c==. & excl_obs==0
* only 39 missing in 2010
* only 172 missing in 2009

* Other meteo variables
count if speed_ms==. & excl_obs==0
count if visib_km==. & excl_obs==0
count if rh==. & excl_obs==0
* only 137, 2 and 22 missing, respectively, in 2010
* only 253, 50 and 132 missing, respectively, in 2009

* Other spatio-temporal variables
sum pbl ndvi r_sum_ndvi r_mean_ndvi dust if excl_obs==0
replace dust=0 if dust==.

gen day=doy(date)
sum day


* I rescale AOD values by 1000 
rename  aod aod1000
gen aod=aod1000/1000
sum aod


codebook site
codebook site if excl_obs==0
* 545 different monitors, of which 528 with at least 30 daily AOD valid observations in 2010
* 488 different monitors, of which 501 with at least 30 daily AOD valid observations in 2009

* Now I create a variable IDSITE ranging from 1 to max.number of sites
sort site date
egen idsite = group(site) 


*** I ORDER AND SAVE THE TEMPORARY FILE

order idsite site longitude latitude point_x_site point_y_site desc_comune-desc_monitor date day pm10 pm25 idcell point_x_cell point_y_cell zona nome_zona 
order idsite-nome_zona restot emip near_emip so2_2010p-pm10_2010p so2_2010a-pm10_2010a pct_deciduous pct_evergreen pct_crop pct_pasture-pct_low_dev
order idsite-pct_low_dev isa length_a1-length_oth near_airport near_port near_sea near_a1 near_a2 near_a3 flag_sea flag_lake elevation pm25_2005
order idsite-pm25_2005 r_ncells r_sum_restot r_mean_restot r_sum_so2_2010p-r_sum_pm10_2010p r_mean_so2_2010p-r_mean_pm10_2010p  r_mean_so2_2010a- r_mean_pm10_2010a
order idsite-r_mean_pm10_2010a r_sum_isa r_mean_isa r_sum_length_a1-r_sum_length_oth r_mean_length_a1-r_mean_length_oth
order idsite-r_mean_length_oth r_sum_area_deci-r_mean_elevation aod aod1000 idcell_buffer distance flag_sea_buffer flag_lake_buffer
order idsite-flag_lake_buffer temp_c speed_ms visib_km rh stn_temp dist_temp stn_speed_ms dist_speed stn_visib_km dist_visib stn_rh dist_rh
order idsite-dist_rh pbl ndvi r_sum_ndvi r_mean_ndvi dust dust_area excl_obs

sum if excl_obs==0

sort idsite day

*save "F:\SATELLITE\stage one\2010\buttare1.dta", replace
save "F:\SATELLITE\stage one\2009\buttare1.dta", replace
clear


*** FIFTH PART: I CREATE FULL TIME-SERIES FOR ALL MONITORS


set obs 365
egen day = seq(), f(1) t(365)
*expand 545
expand 504
count

bysort day: gen idsite=_n
order idsite day
sort idsite day
*save "F:\SATELLITE\stage one\2010\buttare2.dta", replace
save "F:\SATELLITE\stage one\2009\buttare2.dta", replace

*merge idsite day using "F:\SATELLITE\stage one\2010\buttare1.dta", update
merge idsite day using "F:\SATELLITE\stage one\2009\buttare1.dta", update
tab _m
drop _merge

sort idsite day
gen mm=month(date)
gen dd=day(date)

gen season=1
replace season=2 if mm>=3 & mm<=5
replace season=3 if mm>=6 & mm<=8
replace season=4 if mm>=9 & mm<=11
replace season=. if mm==.

label var season "1=winter (mm 12-2), 2=spring (mm 3-5) , 3=summer (mm 6-8), 4=fall (mm 9-11)"
tab season, m

order  idsite site-date day mm dd season


*** SIXTH PART: I CREATE LOG VARIABLES FOR ALL CONTINUOUS POSITIVE VARIABLES. SINCE SEVERAL OF THESE VARIABLES HAVE
***             0 VALUES, THEN I REPLACE IT TO 0.01 AND CREATE THE LOG

set more off
sum if excl_obs==0 
foreach var of varlist pm10 pm25 restot so2_2010a nox_2010a co_2010a nh3_2010a pm10_2010a near_airport near_port near_sea near_a1 near_a2 near_a3 pm25_2005 r_sum_restot r_mean_restot r_mean_so2_2010a r_mean_nox_2010a r_mean_co_2010a r_mean_nh3_2010a r_mean_pm10_2010a r_mean_length_oth aod pbl {
gen `var'_buttare=`var'
replace `var'_buttare=0.01 if `var'_buttare==0
gen log_`var'=ln(`var'_buttare)
drop `var'_buttare
}

sum log_* if excl_obs==0


*** I SAVE THE FINAL FILE READY FOR THE ANALYSIS


*save "F:\SATELLITE\stage one\2010\aod_stage_one_2010_analysis.dta", replace
save "F:\SATELLITE\stage one\2009\aod_stage_one_2009_analysis.dta", replace


*** I ERASE THE "BUTTARE" FILES

*erase "F:\SATELLITE\stage one\2010\buttare1.dta"
*erase "F:\SATELLITE\stage one\2010\buttare2.dta"
erase "F:\SATELLITE\stage one\2009\buttare1.dta"
erase "F:\SATELLITE\stage one\2009\buttare2.dta"
