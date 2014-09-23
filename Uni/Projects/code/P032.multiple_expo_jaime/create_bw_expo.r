fullbw<-fread("/media/NAS/Uni/Projects/P011.BirthW_NE/3.1.11.4.Work/3.Analysis/2.R_analysis/bw_diab37.csv",colClasses=c(FIPS="character",tract="character"))
l=seq(names(fullbw));names(l)=names(fullbw);
l
str(fullbw$FIPS)



#subset data
fullbw.s<-fullbw[,c("byob","birthw","lbw","sex","plur","bdob","kess","tden","age_centered","age_centered_sq","FIPS",
"cig_preg", "cig_pre", "med_income", "p_ospace", "gender","pcturban","adtmean","dist_A1", "dist_pemis","prev_400","diab",  "hyper" ,"lungd", "diab_other", "prevpret","edu_group","MRN","ges_calc","uniqueid_y","sinetime","costime"),with=FALSE]


setnames(fullbw.s, c("bdob", "byob","uniqueid_y"), c("birthdate", "birthyear","id"))


# merge in other covariates
setkey(fullbw.s,id)
setkey(gestlong.pm.lags ,id)
bwfull.pm <- merge(fullbw.s, gestlong.pm.lags)
head(bwfull.pm)
setkey(bwfull.pm, id)
setkey(gestlong.pm.lags.clin ,id)
bwfull.pmc <- merge(bwfull.pm, gestlong.pm.lags.clin)



