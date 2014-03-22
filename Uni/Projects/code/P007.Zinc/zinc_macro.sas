
*This SAS program is for Sam Myers, to calculate % at risk of inadequate zinc intake, can be used for Monte Carlo simulation
as well as analyses based on FACE and Summary estimates and the Mean (95% CI);

*Import necessary datasets;
*Import FAOFBS_T;
proc import datafile = 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\Myers CO2 UCDavis HSPH\FAOFBS_T_121004.dta'
OUT= faofbs_t
dbms = stata REPLACE;
run;
*Import krw_nutrient excel spreadsheet;
proc import datafile = 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\Myers CO2 UCDavis HSPH\summ_ic_kcal.xls' 
OUT = krw_nutrient
dbms = xls REPLACE;
sheet = krw_nutrient;
run;
       
proc import datafile = 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\Myers CO2 UCDavis HSPH\FAOFBS_wpp2010.xls'
OUT= wpp2010_b
dbms = xls REPLACE;
sheet = wpp2010_b;
run;
proc import datafile = 'c:\Users\ekloog\Documents\$Doc\3.PostDoc\3.1.Projetcs\3.1.7.Zinc\3.1.7.4.Work\2.Gather_data\Myers CO2 UCDavis HSPH\FAOFBS_wpp2010.xls'
OUT= wpp2010_prb
dbms = xls REPLACE;
sheet = wpp2010_prb;
run;


*We could add Myers data into the aforementioned excel spreadsheet (krw_nutrient), or create another spreadsheet
or program that would do the calculations for us;

proc sort data=work.faofbs_t;
by itemcode1 ;
run;
proc sort data=work.krw_nutrient;
by itemcode1;
run;
data work.krw_nutrient (drop = c_item_number _name_ rcgramamt rckcal rczn rcphyt);
set work.krw_nutrient;
run;
data work.faofbs_krw;
merge work.faofbs_t work.krw_nutrient;
by itemcode1;
run;
data work.faofbs_krw;
set work.faofbs_krw;
if item = '' then delete;
run;






kcalrczn
kcalrcphyt
are only things that change
 
*********************Now we need to calculate Zn and phytate intake based on extraction and processing estimates;

*need to do these calculations using Myers data (kcalrczn and kcalrcphyt based on CO2 changes);
data work.faofbs_krw;
set work.faofbs_krw;
if itemcode1 = 25112 then do; mgZn_100kcal = (kcalrczn)*(Extract1)*(Extr_Zn) + (kcalrczn)*(1-Extract1); 
mgPhyt_100kcal = (kcalrcphyt)*(Extract1)*(Extr_Phyt)*(Ferment1)*(Ferm_Phyt) + (kcalrcphyt)*(Extract1)*(Extr_Phyt)*(1-Ferment1) 
+ (kcalrcphyt)*(1-Extract1); end; *Wheat; *Whole grain wheat is NEVER fermented;
else if itemcode1 = 25170 or itemcode1 = 25180 or itemcode1 = 25141 then do;  mgZn_100kcal = (kcalrczn)*(Extract1)*(Extr_Zn) + 
(kcalrczn)*(1-Extract1); mgPhyt_100kcal = (kcalrcphyt)*(Extract1)*(Extr_Phyt)*(Ferment1)*(Ferm_Phyt) + (kcalrcphyt)*(Extract1)*(Extr_Phyt)*(1-Ferment1) + 
(kcalrcphyt)*(1-Extract1)*(Ferment1)*(Ferm_Phyt) + (kcalrcphyt)*(1-Extract1)*(1-Ferment1); end; *Millet and sorghum and maize; 
*Both whole grain and extracted millet and sorghum can be fermented;
else if itemcode1 = 25230 or itemcode1 = 25340 or itemcode1 = 25202 then do; mgZn_100kcal = kcalrczn; mgPhyt_100kcal =
(kcalrcphyt)*(Ferment1)*(Ferm_Phyt) + (kcalrcphyt)*(1-Ferment1); end; *Cassava and other roots, there is no effect on zinc so only effects on phytate due to
fermentation, there is no extraction that affects phytate;
else if itemcode1 = 25460 or itemcode1 = 25470 or itemcode1 = 25490 or itemcode1 = 25550 then do; mgZn_100kcal = kcalrczn; mgPhyt_100kcal =
(kcalrcphyt)*(Extr_phyt); end; *This is for beans; 
else do; mgZn_100kcal = kcalrczn; mgPhyt_100kcal = kcalrcphyt; end; *This is for cereals other and all other itemcodes that do not have
extraction or processing estimates applied to them;
run;

*Calculate amt. of zinc and phytate received per capita per day per food commodity; 
*This is calculated by multiplying kcal per capita per day of food by mg Zn in 100kcal and dividing by 100kcal;

data work.faofbs_krw;
set work.faofbs_krw;
krw_mgZn_cap_d = (mgZn_100kcal / 100 ) * kcal_cap_d;
krw_mgPhyt_cap_d = (mgPhyt_100kcal / 100) *kcal_cap_d;
run;

*Create mean estimate for 2003-2007 FAOFBS data;
*2005 time frame, brackets dates from 2003-2007;
proc sort data=work.faofbs_krw;
by continent_code continent region_code region country_code country iso_code imputeregion imputecountry
itemcode1 itemcodeB itemcode item;
run;

proc summary data=work.faofbs_krw;
var  kcal_cap_d kcalrczn kcalrcphyt extract1 extract2 extr_zn extr_phyt ferment1 ferm_phyt mgzn_100kcal mgphyt_100kcal
krw_mgZn_cap_d krw_mgPhyt_cap_d ;
by continent_code continent region_code region country_code country iso_code imputeregion imputecountry
itemcode1 itemcodeB itemcode item;
where year IN ('y2003', 'y2004', 'y2005', 'y2006', 'y2007');
output out=faofbs_krw2005 mean = ;
run;

data work.faofbs_krw2005;
set work.faofbs_krw2005;
year = 'b2005';
run;


***Now transpose data to get foods as variables and kcal_cap_d, mgZn_cap_d and mgphyt_cap_d on separate lines, sorted by 
country;

proc sort data=work.faofbs_krw2005;
by continent_code continent region_code region country_code country iso_code imputeregion imputecountry year;
run;

*Transpose data ;
proc transpose data=work.faofbs_krw2005 out = work.faofbs_krw2005;
by continent_code continent region_code region country_code country iso_code imputeregion imputecountry year;
id itemcode1;
var kcal_cap_d krw_mgZn_cap_d krw_mgphyt_cap_d ;
run;
 *Add item labels to variables;

 data work.faofbs_krw2005;
 set work.faofbs_krw2005;
 label _1 = 'Alcohol, Non-Food'
_2=    'Alcoholic Beverages +'  _3  =  'Animal Fats +' _4   = 'Animal Products +' _8  =  'Aquatic Products, Other +'
_19   = 'Cereals - Excluding Beer +' _37  =  'Fish, Seafood +' _39  =  'Fruits - Excluding Wine +'
_41   = 'Grand Total +'  _51 =   'Meat +' _56 =   'Miscellaneous +' _61 =   'Oilcrops +' _81 =   'Pulses +'
_94 =   'Spices +' _96  =  'Starchy Roots +' _97 =   'Stimulants +' _98 =   'Sugar & Sweeteners +' _103  =  'Sugarcrops +'
_111 =   'Vegetable Oils +'  _112  =  'Vegetables +' _114   = 'Vegetal Products +' _25112 =   'Wheat' _25130  =  'Barley'
_25141 =    'Maize' _25150 =     'Rye' _25160 =   'Oats' _25170  =  'Millet' _25180 =   'Sorghum' _25201  =  'Cereals, Other'
_25202 = 'Cereals, Other, Teff' _25310  =  'Potatoes' _25320 =   'Cassava'  _25330 =   'Sweet Potatoes'  _25340  =  'Roots, Other'
_25350 =   'Yams' _25360 =   'Sugar Cane' _25370 =   'Sugar Beet' _25410 =   'Sugar, Non-Centrifugal'  _25420 =   'Sugar (Raw Equivalent)' 
_25430  =  'Sweeteners, Other' _25460 =   'Beans'  _25470 =   'Peas'  _25490 =   'Pulses, Other' _25510 =   'Tree Nuts'
_25550 =   'Soyabeans' _25560 =   'Groundnuts (Shelled Eq)' _25570 =   'Sunflowerseed' _25600  =  'Coconuts - Incl Copra'
_25610 =   'Sesameseed' _25630 =   'Olives' _25700  =  'Oilcrops, Other' _25710  =  'Soyabean Oil'  _25720  =  'Groundnut Oil'
_25730  =  'Sunflowerseed Oil' _25740 =   'Rape and Mustard Oil' _25750 =   'Cottonseed Oil' _25760  =  'Palmkernel Oil'
_25770 =   'Palm Oil' _25780 =   'Coconut Oil' _25790 =   'Sesameseed Oil' _25800 =  'Olive Oil' _25810  =  'Ricebran Oil'
_25820  =  'Maize Germ Oil' _25860  =  'Oilcrops Oil, Other' _26010  =  'Tomatoes'  _26020 =   'Onions' _26050 =   'Vegetables, Other'
_26110 =   'Oranges, Mandarines'    _26120 =    'Lemons, Limes' _26130  =  'Grapefruit' _26140  =  'Citrus, Other'
_26150 =   'Bananas' _26160 =   'Plantains' _26170 =   'Apples' _26180  =  'Pineapples' _26190 =   'Dates' _26200=    'Grapes'
_26250 =   'Fruits, Other' _26300=    'Coffee' _26330 =   'Cocoa Beans' _26350=    'Tea' _26400 =   'Pepper' _26410 =   'Pimento'
_26420  =  'Cloves' _26450 =   'Spices, Other' _26550 =   'Wine' _26560  =  'Beer' _26570  =  'Beverages, Fermented'
_26580 =   'Beverages, Alcoholic' _27310  =  'Bovine Meat' _27320  =  'Mutton & Goat Meat' _27330 =   'Pigmeat'
_27340 =   'Poultry Meat' _27350 =   'Meat, Other' _27360  =  'Offals, Edible' _27370  =  'Fats, Animals, Raw'
_27401 =   'Butter, Ghee' _27430 =   'Cream' _27440 =   'Eggs' _27450 =   'Honey' _27610  =  'Freshwater Fish'
_27620 =  'Demersal Fish' _27630 =   'Pelagic Fish' _27640 =    'Marine Fish, Other' _27650 =   'Crustaceans'
_27660 =   'Cephalopods' _27670 =   'Molluscs, Other'   _27690 =   'Aquatic Animals, Others'
_27750  =  'Aquatic Plants' _27810  =  'Fish, Body Oil' _27820   = 'Fish, Liver Oil' _28050  =  'Rice (Milled Equivalent)'
_28480  =  'Milk - Excluding Butter' _99970  =  'Palmkernals' _99980  =  'Cottonseed' _99990 =   'Rape and Mustard Seed';
run; *_27680 =   'Meat, Aquatic Mammals';

*Create calculated total of kcal_cap_d, mgZn_cap_d and mgphyt_cap_d based on individual food commodities;
data work.faofbs_krw2005;
set work.faofbs_krw2005;
calc_tot = SUM (OF _25112 -- _25202);
run;

proc sort data=work.faofbs_krw2005;
by continent_code continent region_code region country_code country iso_code imputeregion imputecountry year;
run;

proc transpose data=work.faofbs_krw2005 out = work.faofbs_krwyear;
by continent_code continent region_code region country_code country iso_code imputeregion imputecountry year;
id _name_;
var calc_tot;
run;


*****Merging pop_total data (from work.wpp2010_b) with faofbs_year dataset;
*Because right now FAOFBS_krwyear Kcal, Zn and Phytate are calculated on a per capita basis and we want to know how much
zinc is in the national food supply according to these calculations;


proc sort data=work.faofbs_krwyear;
by country_code year;
run;

proc sort data=work.wpp2010_b;
by country_code year;
run;

data work.faofbs_krwyear (rename = (_name_ = nut_cap_d));
merge work.faofbs_krwyear work.wpp2010_b (keep = country_code pop_tot1000 year);
by country_code year;
run;
*Now we need to merge the population numbers with the faofbs_krwyear dataset from above;

proc sort data=work.faofbs_krwyear;
by continent_code continent region_code region country_code country iso_code year;
run;

proc sort data=work.wpp2010_prb;
by continent_code continent region_code region country_code country iso_code year;
run;

data work.faofbs_krwyear;
merge work.faofbs_krwyear wpp2010_prb (keep=continent_code continent region_code region country_code country iso_code year 
PrZn_mean);
by continent_code continent region_code region country_code country iso_code year;
run;

*Calculation phytate:Zn ratios here now;
data work.faofbs_krwyear;
set work.faofbs_krwyear;
krw_P_Zn = (krw_mgphyt_cap_d/660)/(krw_mgZn_cap_d/65.4);
run;

   *** Application of Miller equation;

*Have kcal,zinc, and phytate values as a single observation;

/*Miller version 2010 (Hambidge et al. AJCN 2010;91 (suppl):1478s-83S*/
data work.faofbs_krwyear;
set work.faofbs_krwyear;
      amax2010 = .091;
      kr2010 = .033;
      kp2010 = .68;
	  run;

data work.faofbs_krwyear;
set work.faofbs_krwyear;
      krw_millernum2010 = amax2010+(krw_mgZn_cap_d/65.38)+kr2010*(1+(krw_mgphyt_cap_d/660.08)/kp2010);
      krw_absznmiller = 65.38*0.5*(krw_millernum2010-SQRT(krw_millernum2010**2-4*amax2010*(krw_mgZn_cap_d/65.38)));
      krw_fazmiller = krw_absznmiller/krw_mgZn_cap_d;
	run;



data work.faofbs_krwdef;
set work.faofbs_krwyear;
      krw_przn_earpctmiller = 100*krw_absznmiller/(przn_mean);
      krw_przn_pctdefmiller = 100*probnorm( (przn_mean-krw_absznmiller)/(.25*krw_absznmiller));
run;

*proc export data=work.faofbs_zndef
dbms = stata replace
label 
outfile = 'C:\Users\Ryan\Documents\FAO FBS 2011\FAOFBS_Zndef';
*run;




















