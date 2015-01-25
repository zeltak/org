
###############
#LIBRARIES
###############


library(Rcpp)
library(lme4)
library(foreign)
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
LIBRARYlibrary(Hmisc)
library(mgcv)
library(gdata)
library(car)
library(dplyr)
library(glmnet)
library(gamm4)


### READ THE DATASET WITH FULL TIME SERIES FOR 545 SITES: 545*365=198,925 OBSERVATIONS

#data <- read.dta("J:\\Common_AREA\\CONTROLLATI\\Satellite\\stage one\\Italy_stage_one_2010_max_new3.dta")
data <- read.dta("J:\\Common_AREA\\CONTROLLATI\\Satellite\\stage one\\Italy_stage_one_2010_max_new3.dta")



### ONLY CONSIDER OBSERVATIONS WITH NON-MISSING AOD AND PM10, AND WITH EXCLUSIONS OF SOME DAILY VALUES WITH STRANGE AOD AND PM

mod1<-subset(data, excl_obs==0)
names(mod1)
dim(mod1)


#######################################################################################################################################
#######################################################################################################################################

##### ITAI'S CODE


##### FROM HERE........................



###############
#TABLES
###############
#create main CV table
mod1table <- data.frame(model=character(40), r2002=numeric(40), r2003=numeric(40),
                        r2004=numeric(40),r2005=numeric(40),
                        r2006=numeric(40),r2007=numeric(40),
                        r2008=numeric(40),r2009=numeric(40),
                        r2010=numeric(40),r2011=numeric(40),
                        r2012=numeric(40), r2013=numeric(40),mean=numeric(40))

#name columns

mod1table$model<- c("allyears","mod1CV_R2","mod1CV_int","mod1CV_int_SE",
                    "mod1CV_Slope","mod1CV_Slope SE","mod1CV_RMSPE",
                    "mod1CV_spatial","mod1CV_temporal","mod1CV_RMSPE_spatial",
                    "mod1CVLPM_R2","mod1CVLPM_int","mod1CVLPM_int_SE",
                    "mod1CVLPM_Slope","mod1CVLPM_Slope_SE","mod1CVLPM_RMSPE",
                    "mod1CVLPM_spatial","mod1CVLPM_temporal","mod1CVLPM_RMSPE_spatial",
                    "mod2_R2","mod3a_pre_gam","mod3b_post_gam","mod3_pm_mod3","mod3_int",
                    "mod3_int_SE","mod3_Slope","mod3_Slope SE","mod3_RMSPE",
                    "mod3_spatial","mod3_temporal","mod3_RMSPE_spatial",
                    "mod3LPM_pm_mod3LPM","mod3LPM_int","mod3LPM_int_SE","mod3LPM_Slope",
                    "mod3LPM_Slope SE","mod3LPM_RMSPE","mod3LPM_spatial","mod3LPM_temporal","mod3LPM_RMSPE_spatial")

# mod1<-fread("/home/zeltak/Downloads/sas/mass.csv")
names(mod1)
summary(mod1$pm10)
#add date
# mod1[, day:=as.Date(strptime(date, "%d/%m/%Y"))]
mod1$aod<-mod1$aod*0.001
summary(mod1$aod
pm25.m1<- filter(mod1, PM25 >= 0)
pm10.m1<- filter(mod1, PM10 >= 0)

########
##pm10
########

# summary(lm(PM25~aod, mod1pm25))

summary(lm(pm10~aod, mod1))

#lme formulas
#base raw
m1.formula <- as.formula(pm10~aod+(1+aod|day)) #0.80
#base+regions
m1t.formula <- as.formula(PM10~ aod+(1+aod|day/reg_num))#0.836
# #base+covars
# m1t.formula <- as.formula(PM10~ aod+elev+tden+pden+dist2rail+dist2A1+dist2water+Temp+RH+NDVI+season+MeanPbl+p_os+p_dev+p_dos+p_farm+p_for+p_ind+(1+aod|day))#0.8193
# #base+reg+covars
# m1t.formula <- as.formula(PM10~ aod+WS+elev+tden+pden+dist2rail+dist2A1+dist2water+Temp+RH+ndvi+season+MeanPbl+p_os+p_dev+p_dos+p_farm+p_for+p_ind+(1+aod|day/reg_num)) # 0.8832
#base+reg+covars+interactions

m1.formula <- as.formula(PM10~ aod+DUST+ELEVATION+NDVI+PBL+RESTOT+NEAR_EMIP+LENGTH_A1+(1+aod|day))


out.m1 <- lmer(m1.formula, data=mod1, na.action=na.exclude)
summary(out.m1)
mod1$predicted <- predict(out.m1)
summary(lm(pm10~predicted,data=mod1))


##### TO HERE........................


#######################################################################################################################################
#######################################################################################################################################


##### IN THE FOLLOWING PART I TRY TO RUN ELASTIC NET MODELS. THESE MODELS ARE VERY FLEXIBLE AND THEY ARE AIMED AT IDENTIFYING THE
##### RELEVANT PREDICTORS OUT OF A LONG LIST OF VARIABLES POTENTIALLY CORRELATED. TO DO SO, IT IS BETTER TO WORK ON LOGARITHMIC 
##### SCALE. AFTER CREATING LOG VARIABLES AND RUNNING A FULL MODEL, I CHECK THE OUTPUT AND SEE HOW MANY VARIABLES ARE ESTIMATED
##### A COEFFICINET > 0 (KEEP THEM) AND HOW MANY ARE SHRINKED TO ZERO (DROP THEM).
##### AS A NEXT STEP, I RUN THE USUAL GLM MODEL WITH THE SO-IDENTIFIED SET OF PREDICTORS, AND COMPARE R2 cv AND NOT cv.
##### IF THEY DIFFER MUCH, THEN IT MEANS THAT THE MODEL IS OVERFITTED AND I HAVE TO FIND A WAY TO SHRINK THE LESS RELEVANT VARS.
##### WE'LL SEE IT LATER.


### DEFINE THE OUTCOME VARIABLE (LOG_PM10) AS Y
y <- mod1[,105]


### DEFINE THE MATRIX WITH FULL PREDICTORS AS X

#x <- as.matrix(mod1[,c(106:114,116:123,125,127:128)])
#x <- mod1[,c(106:114,116:123,125,127:128)]
#x <- as.matrix(mod1[,c(106:114,116:123,124,128)])
#x <- as.matrix(mod1[,c(26,27,28,29,36:42,44,45,47,48,52,55:59,61,62,107:114,116:123,125,127:128)])
x <- as.matrix(mod1[,c(26,27,28,29,36:42,44,45,47,48,52,55:59,61,62,107:114,116:123,125,127)])
dim(x)
summary(x)


### APPLY A CROSS-VALIDATED FORMULATION OF ELASTIC NET MODELS (SEE HELP), AND TRY WITH DIFFERENT ALPHA PARAMETERS (PENALIZATIONS)
### THERE I SELECT THE VLAUES WIT LOWEST LAMBDA AND, BASED ON THIS VALUE, I MAKE PREDICTIONS AND CHECK R-SQUARED

foldid=sample(rep(seq(10),length=length(y)))
alpha=matrix(nrow=10,ncol=1,NA)
rsq=matrix(nrow=10,ncol=1,NA)
for (i in 1:10) {
prova1 <- cv.glmnet(x, y, alpha = 0.1*i, foldid)
summary(prova1)
alpha[i]<-prova1$lambda.min
predicted <- predict(prova1,newx=x,s="lambda.min")
print(summary(lm(y~predicted)))
rsq[i]<-summary(try)$r.squared
}

alpha
rsq

names(prova1)
prova1$lambda

prova1 <- cv.glmnet(x, y, alpha = 0.5)
plot(prova1)
prova1$lambda.min
prova1$nzero

predicted <- predict(prova1,newx=x,s="lambda.min")
try=lm(y~predicted)
summary(try)
try$df.residual


##### APPARENTLY, NONE OF THE COEFFICIENT IS SHRINKED TO ZERO, SO THEY ALL CONTRIBUTE SOMEWHAT.
##### IN THE NEXT LINES, I DEFINE A GLM MODEL WITH ALL OF THEM, PLUS OTHER PREDICTORS


# x <- as.matrix(mod1[,c(26,27,28,29,36:42,44,45,47,48,52,55:59,61,62,107:114,116:123,125,127:128)])
# names(mod1[,c(26,27,28,29,36:42,44,45,47,48,52,55:59,61,62,107:114,116:123,125,127:128)])


# [1] "so2_2010p"             "nox_2010p"             "co_2010p"              "nh3_2005p"             "pct_deciduous"        
# [6] "pct_evergreen"         "pct_crop"              "pct_pasture"           "pct_shrub"             "pct_high_dev"         
# [11] "pct_low_dev"           "length_a1"             "length_a23"            "near_airport"          "near_port"            
# [16] "elevation"             "r_sum_so2_2010p"       "r_sum_nox_2010p"       "r_sum_co_2010p"        "r_sum_nh3_2010p"      
# [21] "r_sum_pm10_2010p"      "r_sum_length_a1"       "r_sum_length_a23"      "log_so2_2010a"         "log_nox_2010a"        
# [26] "log_co_2010a"          "log_nh3_2010a"         "log_pm10_2010a"        "log_near_a1"           "log_near_a2"          
# [31] "log_near_a3"           "log_r_mean_restot"     "log_r_mean_so2_2010a"  "log_r_mean_nox_2010a"  "log_r_mean_co_2010a"  
# [36] "log_r_mean_nh3_2010a"  "log_r_mean_pm10_2010a" "log_r_mean_length_oth" "log_aod"               "log_ndvi"             
# [41] "log_r_mean_ndvi"       "log_pbl"              


x <- as.matrix(mod1[,c(26,27,28,29,36:42,44,45,47,48,52,55:59,61,62,107:114,116:123,125,127)])
names(mod1[,c(26,27,28,29,36:42,44,45,47,48,52,55:59,61,62,107:114,116:123,125,127)])


[1] "so2_2010p"             "nox_2010p"             "co_2010p"              "nh3_2005p"             "pct_deciduous"         "pct_evergreen"        
[7] "pct_crop"              "pct_pasture"           "pct_shrub"             "pct_high_dev"          "pct_low_dev"           "length_a1"            
[13] "length_a23"            "near_airport"          "near_port"             "elevation"             "r_sum_so2_2010p"       "r_sum_nox_2010p"      
[19] "r_sum_co_2010p"        "r_sum_nh3_2010p"       "r_sum_pm10_2010p"      "r_sum_length_a1"       "r_sum_length_a23"      "log_so2_2010a"        
[25] "log_nox_2010a"         "log_co_2010a"          "log_nh3_2010a"         "log_pm10_2010a"        "log_near_a1"           "log_near_a2"          
[31] "log_near_a3"           "log_r_mean_restot"     "log_r_mean_so2_2010a"  "log_r_mean_nox_2010a"  "log_r_mean_co_2010a"   "log_r_mean_nh3_2010a" 
[37] "log_r_mean_pm10_2010a" "log_r_mean_length_oth" "log_ndvi"              "log_r_mean_ndvi"       "aod"                  ##### IN THE FOLLOWING MODEL, I PUT A LARGE LIST OF PREDICTORS, PLUS RANDOM INTERCEPTS BY DAY, ALL NESTED BY GEOGRAPHICAL AREAS (N=64869)


m2.formula <- as.formula(log_pm10 ~ log_aod+log_r_mean_restot+log_pbl+log_ndvi+elevation+dust+near_emip+
                                    log_near_a1+log_near_a2+log_near_a3+near_airport+near_port+length_a1+length_a23+
                                    r_sum_length_a1+r_sum_length_a23+log_r_mean_length_oth+
                                    pct_deciduous+pct_evergreen+pct_crop+pct_pasture+pct_shrub+pct_high_dev+pct_low_dev+
                                    so2_2010p+nox_2010p+co_2010p+nh3_2005p+r_sum_so2_2010p+r_sum_nox_2010p+r_sum_co_2010p+r_sum_nh3_2010p+
                                    log_so2_2010a+log_nox_2010a+log_co_2010a+log_nh3_2010a+log_pm10_2010a+
                                    log_r_mean_so2_2010a+log_r_mean_nox_2010a+log_r_mean_co_2010a+log_r_mean_nh3_2010a+log_r_mean_pm10_2010a +
                                    (1+aod|day/geog_area))

out.m2 <- lmer(m2.formula, data=mod1, na.action=na.exclude)
summary(out.m2)
mod1$pred2 <- predict(out.m2)
summary(lm(log_pm10~pred2,data=mod1))

# 0.5853



############################################################### SKIP THE FOLLOWING PART ##############################################################


##### IN THE FOLLOWING MODEL, I RUN EXACTLY THE SAME MODEL, BUT ONLY ON THE SUBSET OF OBSERVATIONS WITH LAG1_AOD NOT MISSING (N=35843).
##### I DO SO TO CHECK WHETHER THE INCLUSION OF THIS VARIABLE WILL IMPROVE SUBSTANTIALLY THE R2


mod2<-subset(mod1,log_aod_l1!="NA")
dim(mod2)

out.m3 <- lmer(m2.formula, data=mod2, na.action=na.exclude)
summary(out.m3)
mod2$pred3 <- predict(out.m3)
summary(lm(log_pm10~pred3,data=mod2))

# 0.5859


##### NOW I ADD AOD_LAG1 TO SEE IF IT IMPROVES THE FITTING

m3.formula <- as.formula(log_pm10 ~ log_aod+log_aod_l1+log_r_mean_restot+log_pbl+log_ndvi+elevation+dust+near_emip+
                           log_near_a1+log_near_a2+log_near_a3+near_airport+near_port+length_a1+length_a23+
                           r_sum_length_a1+r_sum_length_a23+log_r_mean_length_oth+
                           pct_deciduous+pct_evergreen+pct_crop+pct_pasture+pct_shrub+pct_high_dev+pct_low_dev+
                           so2_2010p+nox_2010p+co_2010p+nh3_2005p+r_sum_so2_2010p+r_sum_nox_2010p+r_sum_co_2010p+r_sum_nh3_2010p+
                           log_so2_2010a+log_nox_2010a+log_co_2010a+log_nh3_2010a+log_pm10_2010a+
                           log_r_mean_so2_2010a+log_r_mean_nox_2010a+log_r_mean_co_2010a+log_r_mean_nh3_2010a+log_r_mean_pm10_2010a +
                           (1+aod|day/geog_area))

out.m4 <- lmer(m3.formula, data=mod2, na.action=na.exclude)
summary(out.m4)
mod2$pred4 <- predict(out.m4)
summary(lm(log_pm10~pred4,data=mod2))

# 0.5853


##### APPARENTLY, AT LEAST ON THE 35843 OBSERVATIONS, AOD_LAG1 DOES NOT CONTRIBUTE.



############################################################### AND START BACK FROM HERE ##############################################################


##### AS THE NEXT STEP, I RUN CROSS-VALIDATION MODEL AND COMPUTE CV-R-SQUARED


###############
#MOD1 CV
###############


# splitdf <- function(dataframe, seed=NULL, trainfrac=0.1) {
#   if (trainfrac<=0 | trainfrac>=1) stop("Training fraction must be between 0 and 1, not inclusive")
#   if (!is.null(seed)) set.seed(seed)
#   index <- 1:nrow(dataframe)
#   trainindex <- sample(index, trunc(length(index)/(1/trainfrac)))
#   trainset <- dataframe[trainindex, ]
#   testset <- dataframe[-trainindex, ]
#   list(trainset=trainset,testset=testset)
# }

#function to create Cross validated 90% and 10% splits

splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}



#s1
splits_s1 <- splitdf(mod1)
mod1d_10_s1 <- splits_s1$trainset
mod1d_90_s1 <- splits_s1$testset
out_90_s1 =  lmer(m2.formula, data=mod1d_90_s1)
mod1d_10_s1$predicted <- predict(object=out_90_s1, newdata=mod1d_10_s1, allow.new.levels=TRUE, re.form=NULL )

#s2
splits_s2 <- splitdf(mod1)
mod1d_10_s2 <- splits_s2$trainset
mod1d_90_s2 <- splits_s2$testset
out_90_s2 =  lmer(m2.formula,data =  mod1d_90_s2)
mod1d_10_s2$predicted <- predict(object=out_90_s2, newdata=mod1d_10_s2, allow.new.levels=TRUE,REform=NULL )

#s3
splits_s3 <- splitdf(mod1)
mod1d_10_s3 <- splits_s3$trainset
mod1d_90_s3 <- splits_s3$testset
out_90_s3 =  lmer(m2.formula,data =  mod1d_90_s3)
mod1d_10_s3$predicted <- predict(object=out_90_s3,newdata=mod1d_10_s3,allow.new.levels=TRUE,REform=NULL )

#s4
splits_s4 <- splitdf(mod1)
mod1d_10_s4 <- splits_s4$trainset
mod1d_90_s4 <- splits_s4$testset
out_90_s4 =  lmer(m2.formula,data =  mod1d_90_s4)
mod1d_10_s4$predicted <- predict(object=out_90_s4,newdata=mod1d_10_s4,allow.new.levels=TRUE,REform=NULL )

#s5
splits_s5 <- splitdf(mod1)
mod1d_10_s5 <- splits_s5$trainset
mod1d_90_s5 <- splits_s5$testset
out_90_s5 =  lmer(m2.formula,data =  mod1d_90_s5)
mod1d_10_s5$predicted <- predict(object=out_90_s5,newdata=mod1d_10_s5,allow.new.levels=TRUE,REform=NULL )

#s6
splits_s6 <- splitdf(mod1)
mod1d_10_s6 <- splits_s6$trainset
mod1d_90_s6 <- splits_s6$testset
out_90_s6 =  lmer(m2.formula,data =  mod1d_90_s6)
mod1d_10_s6$predicted <- predict(object=out_90_s6,newdata=mod1d_10_s6,allow.new.levels=TRUE,REform=NULL )

#s7
splits_s7 <- splitdf(mod1)
mod1d_10_s7 <- splits_s7$trainset
mod1d_90_s7 <- splits_s7$testset
out_90_s7 =  lmer(m2.formula,data =  mod1d_90_s7)
mod1d_10_s7$predicted <- predict(object=out_90_s7,newdata=mod1d_10_s7,allow.new.levels=TRUE,REform=NULL )

#s8
splits_s8 <- splitdf(mod1)
mod1d_10_s8 <- splits_s8$trainset
mod1d_90_s8 <- splits_s8$testset
out_90_s8 =  lmer(m2.formula,data =  mod1d_90_s8)
mod1d_10_s8$predicted <- predict(object=out_90_s8,newdata=mod1d_10_s8,allow.new.levels=TRUE,REform=NULL )

#s9
splits_s9 <- splitdf(mod1)
mod1d_10_s9 <- splits_s9$trainset
mod1d_90_s9 <- splits_s9$testset
out_90_s9 =  lmer(m2.formula,data =  mod1d_90_s9)
mod1d_10_s9$predicted <- predict(object=out_90_s9,newdata=mod1d_10_s9,allow.new.levels=TRUE,REform=NULL )

#s10
splits_s10 <- splitdf(mod1)
mod1d_10_s10 <- splits_s10$trainset
mod1d_90_s10 <- splits_s10$testset
out_90_s10 =  lmer(m2.formula,data =  mod1d_90_s10)
mod1d_10_s10$predicted <- predict(object=out_90_s10,newdata=mod1d_10_s10,allow.new.levels=TRUE,REform=NULL )



####BIND ALL 10% into 1 dataset

mod1CV_all<- data.table(rbind(mod1d_10_s1,mod1d_10_s2,mod1d_10_s3,mod1d_10_s4,mod1d_10_s5,mod1d_10_s6,mod1d_10_s7,mod1d_10_s8,mod1d_10_s9, mod1d_10_s10))
dim(mod1CV_all)
names(mod1CV_all)
# saveRDS(mod1CV_all_2003, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1CV_all_2003_pred.m1.rds")


# cleanup (remove from WS) objects from CV
# rm(list = ls(pattern = "mod1d|out_|splits_"))

summary(mod1CV_reg <- lm(mod1CV_all$log_pm10~mod1CV_all$predicted))


# 0.5460


#######################################################################################################################################
#######################################################################################################################################

##### ADDITIONAL ITAI'S CODE


##### FROM HERE........................



mod1table$r2003[2] <-summary(mod1CV_reg)$r.squared #R2
mod1table$r2003[3] <-summary(mod1CV_reg)$coef[1,1] #intercept
mod1table$r2003[4] <-summary(mod1CV_reg)$coef[1,2] #intercept SE
mod1table$r2003[5] <-summary(mod1CV_reg)$coef[2,1] #Slope
mod1table$r2003[6] <-summary(mod1CV_reg)$coef[2,2] #Slope SE
#rmspe
mod1table$r2003[7]<- sqrt(mean(mod1CV_reg$residual^2))


#spatial
m1CV_agg <- (mod1CV_all[, j=list(mean(PM25, na.rm = TRUE),mean(predicted, na.rm = TRUE)),by = SiteCode])  
# Rename column
setnames(m1CV_agg,"V1","barpm")
setnames(m1CV_agg,"V2","barpred")
mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
mod1table$r2003[8] <- summary(mod1_spatial)$r.squared

#temporal
setkey(m1CV_agg ,SiteCode)
setkey(mod1CV_all,SiteCode)
dat <- merge(mod1CV_all,m1CV_agg, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2003[9] <-summary(mod_temporal)$r.squared
#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2003[10]<- sqrt(mean(dat$spatresid^2))



##### TO HERE........................


#######################################################################################################################################
#######################################################################################################################################



#####  I TRY NOW TO REPLICATE PREVIOUS MODEL BUT USING ORIGINAL VARIABLES, NOT LOG ONES

# NON-CV MODEL FIRST



m4.formula <- as.formula(pm10 ~ aod+r_mean_restot+pbl+ndvi+elevation+dust+near_emip+
                                near_a1+near_a2+near_a3+near_airport+near_port+length_a1+length_a23+
                                r_sum_length_a1+r_sum_length_a23+r_mean_length_oth+
                                pct_deciduous+pct_evergreen+pct_crop+pct_pasture+pct_shrub+pct_high_dev+pct_low_dev+
                                so2_2010p+nox_2010p+co_2010p+nh3_2005p+r_sum_so2_2010p+r_sum_nox_2010p+r_sum_co_2010p+r_sum_nh3_2010p+
                                so2_2010a+nox_2010a+co_2010a+nh3_2010a+pm10_2010a+
                                r_mean_so2_2010a+r_mean_nox_2010a+r_mean_co_2010a+r_mean_nh3_2010a+r_mean_pm10_2010a +
                               (1+aod|day/geog_area))

out.m4 <- lmer(m4.formula, data=mod1, na.action=na.exclude)
summary(out.m4)
mod1$pred4 <- predict(out.m4)
summary(lm(log_pm10~pred4,data=mod1))

# 0.5049


########### IT DOESN'T IMPROVE, SO I STAY WITH THE LOG-VARIABLES



##### SOME TRIALS ON RESIDUALS FROM PREVIOUS MODEL

mod1$resid2 <- residuals(out.m2)
summary(mod1$resid2)
hist(mod1$resid2)
res.by.site<- tapply(mod1$resid2, mod1$site, mean)
summary(res.by.site)
hist(res.by.site)

res.extreme<- ifelse(res.by.site>abs(.2),1,0)
head(res.extreme)

try <- mod1[order(mod1$site),]
names(try)
head(try)

try2 <- as.data.frame(names(res.extreme),res.extreme)

write.dta(mod1,file="J:\\Common_AREA\\CONTROLLATI\\Satellite\\stage one\\stage_one_modpred.dta", version=11)




### NOTES

### I WENT TO STATA AND CHECKED SOME PREDICOTRS VS THE THREE-CALSS RESIDUALS I HAVE CALCULATED. I NOTICED THAT AVG. VALUES OF
#   elevation, pct_high_dev, near_a1, length_a1, pbl
### ACROSS THE THREE LEVELS OF RESIDUALS ARE VERY DIFFERENT, SO I CONCLUDE THAT I SHOULD MODEL THESE VARIABLES BETTER



library(gamm4)

m4.formula <- as.formula(log_pm10 ~ log_aod*as.factor(season)+log_r_mean_restot+s(log_pbl)+log_ndvi+ns(elevation,3)+dust+near_emip+
                        ns(log_near_a1,3)+log_near_a2+log_near_a3+near_airport+near_port+ns(length_a1,3)+length_a23+
                        r_sum_length_a1+r_sum_length_a23+log_r_mean_length_oth+
                        pct_deciduous+pct_evergreen+pct_crop+pct_pasture+pct_shrub+pct_high_dev+pct_low_dev+
                        so2_2010p+nox_2010p+co_2010p+nh3_2005p+r_sum_so2_2010p+r_sum_nox_2010p+r_sum_co_2010p+r_sum_nh3_2010p+
                        log_so2_2010a+log_nox_2010a+log_co_2010a+log_nh3_2010a+log_pm10_2010a+
                        log_r_mean_so2_2010a+log_r_mean_nox_2010a+log_r_mean_co_2010a+log_r_mean_nh3_2010a+log_r_mean_pm10_2010a,
                        random=~(1+aod|day/geog_area))
# For some reason this formula with GAMM4 and penalized splines does not work. I move to natural splines


library(splines)

### after trying put all 4 as splines, we see that pct_high_dev and length_a1 are linear


### Next, we add season and put NEAR_EMIP as log

m5.formula <- as.formula(log_pm10 ~ log_aod*as.factor(season)+log_r_mean_restot+ns(log_pbl,3)+log_ndvi+ns(elevation,3)+dust+log(near_emip)+
                        ns(log_near_a1,3)+log_near_a2+log_near_a3+near_airport+near_port+length_a1+length_a23+
                        r_sum_length_a1+r_sum_length_a23+log_r_mean_length_oth+
                        pct_deciduous+pct_evergreen+pct_crop+pct_pasture+pct_shrub+pct_high_dev+pct_low_dev+
                        so2_2010p+nox_2010p+co_2010p+nh3_2005p+r_sum_so2_2010p+r_sum_nox_2010p+r_sum_co_2010p+r_sum_nh3_2010p+
                        log_so2_2010a+log_nox_2010a+log_co_2010a+log_nh3_2010a+log_pm10_2010a+
                        log_r_mean_so2_2010a+log_r_mean_nox_2010a+log_r_mean_co_2010a+log_r_mean_nh3_2010a+log_r_mean_pm10_2010a+
                        (1+aod|day/geog_area))

out.m5 <- lmer(m5.formula, data=mod1, na.action=na.exclude)
summary(out.m5)
mod1$pred5 <- predict(out.m5)
mod1$resid5 <- residuals(out.m5)
summary(lm(log_pm10~pred5,data=mod1))

write.dta(mod1,file="J:\\Common_AREA\\CONTROLLATI\\Satellite\\stage one\\stage_one_modpred.dta", version=11)

# 0.5875: Not a big improvement...


#### In the next steo I add further predictors: flag_sea+flag_lake+isa+as.factor(desc_zone)+as.factor(desc_monitor)


m6.formula <- as.formula(log_pm10 ~ log_aod*as.factor(season)+log_r_mean_restot+ns(log_pbl,3)+log_ndvi+ns(elevation,3)+dust+log(near_emip)+
                           ns(log_near_a1,3)+log_near_a2+log_near_a3+near_airport+near_port+length_a1+length_a23+
                           r_sum_length_a1+r_sum_length_a23+log_r_mean_length_oth+flag_sea+flag_lake+isa+as.factor(desc_zone)+as.factor(desc_monitor)+
                           pct_deciduous+pct_evergreen+pct_crop+pct_pasture+pct_shrub+pct_high_dev+pct_low_dev+
                           so2_2010p+nox_2010p+co_2010p+nh3_2005p+r_sum_so2_2010p+r_sum_nox_2010p+r_sum_co_2010p+r_sum_nh3_2010p+
                           log_so2_2010a+log_nox_2010a+log_co_2010a+log_nh3_2010a+log_pm10_2010a+
                           log_r_mean_so2_2010a+log_r_mean_nox_2010a+log_r_mean_co_2010a+log_r_mean_nh3_2010a+log_r_mean_pm10_2010a+
                           (1+aod|day/geog_area))
out.m6 <- lmer(m6.formula, data=mod1, na.action=na.exclude)
summary(out.m6)
mod1$pred6  <- predict(out.m6)
mod1$resid6 <- residuals(out.m6)
summary(lm(log_pm10~pred6,data=mod1))


### R2: 0.6032. GOOD. Now I try with CV


#s1
splits_s1 <- splitdf(mod1)
mod1d_10_s1 <- splits_s1$trainset
mod1d_90_s1 <- splits_s1$testset
out_90_s1 =  lmer(m6.formula, data=mod1d_90_s1)
mod1d_10_s1$predicted <- predict(object=out_90_s1, newdata=mod1d_10_s1, allow.new.levels=TRUE, re.form=NULL )

#s2
splits_s2 <- splitdf(mod1)
mod1d_10_s2 <- splits_s2$trainset
mod1d_90_s2 <- splits_s2$testset
out_90_s2 =  lmer(m6.formula,data =  mod1d_90_s2)
mod1d_10_s2$predicted <- predict(object=out_90_s2, newdata=mod1d_10_s2, allow.new.levels=TRUE,REform=NULL )

#s3
splits_s3 <- splitdf(mod1)
mod1d_10_s3 <- splits_s3$trainset
mod1d_90_s3 <- splits_s3$testset
out_90_s3 =  lmer(m6.formula,data =  mod1d_90_s3)
mod1d_10_s3$predicted <- predict(object=out_90_s3,newdata=mod1d_10_s3,allow.new.levels=TRUE,REform=NULL )

#s4
splits_s4 <- splitdf(mod1)
mod1d_10_s4 <- splits_s4$trainset
mod1d_90_s4 <- splits_s4$testset
out_90_s4 =  lmer(m6.formula,data =  mod1d_90_s4)
mod1d_10_s4$predicted <- predict(object=out_90_s4,newdata=mod1d_10_s4,allow.new.levels=TRUE,REform=NULL )

#s5
splits_s5 <- splitdf(mod1)
mod1d_10_s5 <- splits_s5$trainset
mod1d_90_s5 <- splits_s5$testset
out_90_s5 =  lmer(m6.formula,data =  mod1d_90_s5)
mod1d_10_s5$predicted <- predict(object=out_90_s5,newdata=mod1d_10_s5,allow.new.levels=TRUE,REform=NULL )

#s6
splits_s6 <- splitdf(mod1)
mod1d_10_s6 <- splits_s6$trainset
mod1d_90_s6 <- splits_s6$testset
out_90_s6 =  lmer(m6.formula,data =  mod1d_90_s6)
mod1d_10_s6$predicted <- predict(object=out_90_s6,newdata=mod1d_10_s6,allow.new.levels=TRUE,REform=NULL )

#s7
splits_s7 <- splitdf(mod1)
mod1d_10_s7 <- splits_s7$trainset
mod1d_90_s7 <- splits_s7$testset
out_90_s7 =  lmer(m6.formula,data =  mod1d_90_s7)
mod1d_10_s7$predicted <- predict(object=out_90_s7,newdata=mod1d_10_s7,allow.new.levels=TRUE,REform=NULL )

#s8
splits_s8 <- splitdf(mod1)
mod1d_10_s8 <- splits_s8$trainset
mod1d_90_s8 <- splits_s8$testset
out_90_s8 =  lmer(m6.formula,data =  mod1d_90_s8)
mod1d_10_s8$predicted <- predict(object=out_90_s8,newdata=mod1d_10_s8,allow.new.levels=TRUE,REform=NULL )

#s9
splits_s9 <- splitdf(mod1)
mod1d_10_s9 <- splits_s9$trainset
mod1d_90_s9 <- splits_s9$testset
out_90_s9 =  lmer(m6.formula,data =  mod1d_90_s9)
mod1d_10_s9$predicted <- predict(object=out_90_s9,newdata=mod1d_10_s9,allow.new.levels=TRUE,REform=NULL )

#s10
splits_s10 <- splitdf(mod1)
mod1d_10_s10 <- splits_s10$trainset
mod1d_90_s10 <- splits_s10$testset
out_90_s10 =  lmer(m6.formula,data =  mod1d_90_s10)
mod1d_10_s10$predicted <- predict(object=out_90_s10,newdata=mod1d_10_s10,allow.new.levels=TRUE,REform=NULL )



####BIND ALL 10% into 1 dataset

mod1CV_all<- data.table(rbind(mod1d_10_s1,mod1d_10_s2,mod1d_10_s3,mod1d_10_s4,mod1d_10_s5,mod1d_10_s6,mod1d_10_s7,mod1d_10_s8,mod1d_10_s9, mod1d_10_s10))
dim(mod1CV_all)
names(mod1CV_all)
# saveRDS(mod1CV_all_2003, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1CV_all_2003_pred.m1.rds")
# cleanup (remove from WS) objects from CV
# rm(list = ls(pattern = "mod1d|out_|splits_"))

summary(mod1CV_reg <- lm(mod1CV_all$log_pm10~mod1CV_all$predicted))


# 0.5629



# Replacing GEOG_AREA with NOME_ZONA increases R2 (not cross-validated) from 0.6032 to 0.6152


####################################################################################################################################################
# The following model does not converge
m.formula <- as.formula(log_pm10 ~ log_aod*as.factor(season)+log_r_mean_restot+ns(log_pbl,3)+log_ndvi+ns(elevation,3)+dust+log(near_emip)+
                        ns(log_near_a1,3)+log_near_a2+log_near_a3+near_airport+near_port+length_a1+length_a23+
                        r_sum_length_a1+r_sum_length_a23+log_r_mean_length_oth+flag_sea+flag_lake+isa+as.factor(desc_zone)+as.factor(desc_monitor)+
                        pct_deciduous+pct_evergreen+pct_crop+pct_pasture+pct_shrub+pct_high_dev+pct_low_dev+
                        so2_2010p+nox_2010p+co_2010p+nh3_2005p+r_sum_so2_2010p+r_sum_nox_2010p+r_sum_co_2010p+r_sum_nh3_2010p+
                        log_so2_2010a+log_nox_2010a+log_co_2010a+log_nh3_2010a+log_pm10_2010a+
                        log_r_mean_so2_2010a+log_r_mean_nox_2010a+log_r_mean_co_2010a+log_r_mean_nh3_2010a+log_r_mean_pm10_2010a+
                        ns(rh,3)+ns(speed_ms,3)+ns(visib_km,3)+temp_c+
                        (1+aod|day/nome_zona) + (1+temp_c|day))
out.m <- lmer(m.formula, data=mod1, na.rm=TRUE)
summary(out.m)
mod1$pred.prova  <- predict(out.m)
mod1$resid.prova <- residuals(out.m)
summary(lm(log_pm10~pred.prova,data=mod1))
####################################################################################################################################################


##### FROM NOW ON, I WILL INCLUDE METEOROLOGICAL PARAMETERS AS WELL


# Now I rescale some predictors to make them all more homogeneous

mod2b$elevation.10<-mod1$elevation/10

mod1$nox_2010p.100<-mod1$nox_2010p/100
mod1$so2_2010p.100<-mod1$so2_2010p/100
mod1$r_sum_nox_2010p.100<-mod1$r_sum_nox_2010p/100
mod1$r_sum_so2_2010p.100<-mod1$r_sum_so2_2010p/100

mod1$near_airport.1000<-mod1$near_airport/1000
mod1$near_port.1000<-mod1$near_port/1000
mod1$length_a1.1000<-mod1$length_a1/1000
mod1$length_a23.1000<-mod1$length_a23/1000
mod1$r_sum_length_a1.1000<-mod1$r_sum_length_a1/1000
mod1$r_sum_length_a23.1000<-mod1$r_sum_length_a23/1000


# In order to have non-missing observations for deriving predictions and residuals, I remove the missing ones here

mod2<-mod1[!is.na(mod1$rh) & !is.na(mod1$speed_ms) & !is.na(mod1$temp_c) & !is.na(mod1$visib_km),]
dim(mod2)


##### In the next model, which I call generally "M.FORMULA", I add meteo parameters.

m.formula <- as.formula(  log_pm10 ~   log_aod + as.factor(nome_zona) + log_aod:as.factor(nome_zona) + 
                            flag_sea + flag_lake + as.factor(desc_zone) + as.factor(desc_monitor) + 
                            dust + ns(log_pbl,3) + log_restot + log_ndvi + as.factor(season) + ns(elevation.10,3) + isa +
                            ns(log_near_a1,3) + log_near_a2 + log_near_a3 + near_airport.1000 + near_port.1000 +
                            length_a1.1000 + length_a23.1000 + r_sum_length_a1.1000 + r_sum_length_a23.1000 + log_r_mean_length_oth +
                            pct_deciduous + pct_evergreen + pct_crop + pct_pasture + pct_shrub + pct_high_dev + pct_low_dev +
                            log(near_emip) + so2_2010p.100 + nox_2010p.100 + nh3_2005p + r_sum_so2_2010p.100 + r_sum_nox_2010p.100 + r_sum_nh3_2010p +
                            log_so2_2010a + log_nox_2010a + log_co_2010a + log_nh3_2010a + log_pm10_2010a +
                            ns(rh,3) + ns(speed_ms,3) + ns(visib_km,3) + temp_c +
                            (1+aod+temp_c|day/nome_zona))
out.m <- lmer(m.formula, data=mod2, na.rm=TRUE)
summary(out.m)
mod2$pred.prova  <- predict(out.m)
#mod2$resid.prova <- residuals(out.m)
summary(lm(log_pm10~pred.prova,data=mod2))


##### The model does not converge possibly because of extreme correlations. Therefore I go to Stata and check the correlations.
##### I noticed that these variables display highest correlations: log_co_2010a, log_pm10_2010a, r_sum_so2_2010p, so2_2010p, log_r_mean_length_oth
##### I drop them from the model

m.formula <- as.formula(  log_pm10 ~ 	log_aod + as.factor(nome_zona) + log_aod:as.factor(nome_zona) + 
                            flag_sea + flag_lake + as.factor(desc_zone) + as.factor(desc_monitor) + 
                            dust + ns(log_pbl,3) + log_restot + log_ndvi + as.factor(season) + ns(elevation.10,3) + isa +
                            ns(log_near_a1,3) + log_near_a2 + log_near_a3 + near_airport.1000 + near_port.1000 +
                            length_a1.1000 + length_a23.1000 + r_sum_length_a1.1000 + r_sum_length_a23.1000 + 
                            pct_deciduous + pct_evergreen + pct_crop + pct_pasture + pct_shrub + pct_high_dev + pct_low_dev +
                            log(near_emip) + nox_2010p.100 + nh3_2005p + r_sum_nox_2010p.100 + r_sum_nh3_2010p +
                            log_so2_2010a + log_nox_2010a + log_nh3_2010a + 
                            ns(rh,3) + ns(speed_ms,3) + ns(visib_km,3) + temp_c +
                            (1+aod+temp_c|day/nome_zona))
out.m <- lmer(m.formula, data=mod2, na.rm=TRUE)
summary(out.m)
mod2$pred.prova  <- predict(out.m)
#mod2$resid.prova <- residuals(out.m)
summary(lm(log_pm10~pred.prova,data=mod2))


### The model still does not converge. Since there is no evidence of non-linearity of the meteo pars, I put them all as linear.
### Furthermore, I put TEMP as spline and remove it from the random component



#### 1st MODEL: AOD*NOME_ZONA			R2 = 0.6128			CV-R2 = 0.5670
#### 2nd MODEL: AOD*SEASON			  R2 = 0.6132			CV-R2 = 0.5613
#### 3rd MODEL: AOD*PBL				    R2 = 0.6130			CV-R2 = 0.5620
#### 4th MODEL: AOD*WIND_SPEED		R2 = 0.6124			CV-R2 = 0.5666


m.formula <- as.formula( log_pm10 ~ 	log_aod + as.factor(nome_zona) + log_aod:as.factor(nome_zona) + as.factor(season) + ns(log_pbl,3) + ns(speed_ms,3) +
#m.formula <- as.formula(  log_pm10 ~ log_aod + as.factor(nome_zona) + log_aod:as.factor(season) + as.factor(season) + ns(log_pbl,3) + ns(speed_ms,3) +
#m.formula <- as.formula( log_pm10 ~ 	log_aod + as.factor(nome_zona) + log_aod:log_pbl + as.factor(season) + log_pbl + ns(speed_ms,3) +
#m.formula <- as.formula( log_pm10 ~ 	log_aod + as.factor(nome_zona) + log_aod:speed_ms + as.factor(season) + ns(log_pbl,3) + speed_ms +
                          flag_sea + flag_lake + as.factor(desc_zone) + as.factor(desc_monitor) + 
                          dust + log_restot + log_ndvi + ns(elevation.10,3) + isa +
                          ns(log_near_a1,3) + log_near_a2 + log_near_a3 + near_airport.1000 + near_port.1000 +
                          length_a1.1000 + length_a23.1000 + r_sum_length_a1.1000 + r_sum_length_a23.1000 + 
                          pct_deciduous + pct_evergreen + pct_crop + pct_pasture + pct_shrub + pct_high_dev + pct_low_dev +
                          log(near_emip) + nox_2010p.100 + nh3_2005p + r_sum_nox_2010p.100 + r_sum_nh3_2010p +
                          log_so2_2010a + log_nox_2010a + log_nh3_2010a +
                          ns(rh10,3) + ns(visib_km,3) + ns(temp_c,3) +
                          (1+aod|day/nome_zona))
out.m <- lmer(m.formula, data=mod2, na.rm=TRUE)
summary(out.m)
mod2$pred  <- predict(out.m)
mod2$resid <- residuals(out.m)
summary(lm(log_pm10~pred,data=mod2))

write.dta(mod2,file="E:\\stage one\\stage_one_modpred.dta", version=11)


### Run CV models for each of the 4 models:


#s1
splits_s1 <- splitdf(mod2)
mod2d_10_s1 <- splits_s1$trainset
mod2d_90_s1 <- splits_s1$testset
out_90_s1 =  lmer(m.formula, data=mod2d_90_s1)
mod2d_10_s1$predicted <- predict(object=out_90_s1, newdata=mod2d_10_s1, allow.new.levels=TRUE, re.form=NULL )

#s2
splits_s2 <- splitdf(mod2)
mod2d_10_s2 <- splits_s2$trainset
mod2d_90_s2 <- splits_s2$testset
out_90_s2 =  lmer(m.formula,data =  mod2d_90_s2)
mod2d_10_s2$predicted <- predict(object=out_90_s2, newdata=mod2d_10_s2, allow.new.levels=TRUE,REform=NULL )

#s3
splits_s3 <- splitdf(mod2)
mod2d_10_s3 <- splits_s3$trainset
mod2d_90_s3 <- splits_s3$testset
out_90_s3 =  lmer(m.formula,data =  mod2d_90_s3)
mod2d_10_s3$predicted <- predict(object=out_90_s3,newdata=mod2d_10_s3,allow.new.levels=TRUE,REform=NULL )

#s4
splits_s4 <- splitdf(mod2)
mod2d_10_s4 <- splits_s4$trainset
mod2d_90_s4 <- splits_s4$testset
out_90_s4 =  lmer(m.formula,data =  mod2d_90_s4)
mod2d_10_s4$predicted <- predict(object=out_90_s4,newdata=mod2d_10_s4,allow.new.levels=TRUE,REform=NULL )

#s5
splits_s5 <- splitdf(mod2)
mod2d_10_s5 <- splits_s5$trainset
mod2d_90_s5 <- splits_s5$testset
out_90_s5 =  lmer(m.formula,data =  mod2d_90_s5)
mod2d_10_s5$predicted <- predict(object=out_90_s5,newdata=mod2d_10_s5,allow.new.levels=TRUE,REform=NULL )

#s6
splits_s6 <- splitdf(mod2)
mod2d_10_s6 <- splits_s6$trainset
mod2d_90_s6 <- splits_s6$testset
out_90_s6 =  lmer(m.formula,data =  mod2d_90_s6)
mod2d_10_s6$predicted <- predict(object=out_90_s6,newdata=mod2d_10_s6,allow.new.levels=TRUE,REform=NULL )

#s7
splits_s7 <- splitdf(mod2)
mod2d_10_s7 <- splits_s7$trainset
mod2d_90_s7 <- splits_s7$testset
out_90_s7 =  lmer(m.formula,data =  mod2d_90_s7)
mod2d_10_s7$predicted <- predict(object=out_90_s7,newdata=mod2d_10_s7,allow.new.levels=TRUE,REform=NULL )

#s8
splits_s8 <- splitdf(mod2)
mod2d_10_s8 <- splits_s8$trainset
mod2d_90_s8 <- splits_s8$testset
out_90_s8 =  lmer(m.formula,data =  mod2d_90_s8)
mod2d_10_s8$predicted <- predict(object=out_90_s8,newdata=mod2d_10_s8,allow.new.levels=TRUE,REform=NULL )

#s9
splits_s9 <- splitdf(mod2)
mod2d_10_s9 <- splits_s9$trainset
mod2d_90_s9 <- splits_s9$testset
out_90_s9 =  lmer(m.formula,data =  mod2d_90_s9)
mod2d_10_s9$predicted <- predict(object=out_90_s9,newdata=mod2d_10_s9,allow.new.levels=TRUE,REform=NULL )

#s10
splits_s10 <- splitdf(mod2)
mod2d_10_s10 <- splits_s10$trainset
mod2d_90_s10 <- splits_s10$testset
out_90_s10 =  lmer(m.formula,data =  mod2d_90_s10)
mod2d_10_s10$predicted <- predict(object=out_90_s10,newdata=mod2d_10_s10,allow.new.levels=TRUE,REform=NULL )



####BIND ALL 10% into 1 dataset

mod2CV_all<- data.table(rbind(mod2d_10_s1,mod2d_10_s2,mod2d_10_s3,mod2d_10_s4,mod2d_10_s5,mod2d_10_s6,mod2d_10_s7,mod2d_10_s8,mod2d_10_s9, mod2d_10_s10))
dim(mod2CV_all)
names(mod2CV_all)
# saveRDS(mod2CV_all_2003, "/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod2CV_all_2003_pred.m1.rds")
# cleanup (remove from WS) objects from CV
# rm(list = ls(pattern = "mod2d|out_|splits_"))

summary(mod2CV_reg <- lm(mod2CV_all$log_pm10~mod2CV_all$predicted))
a<-summary(mod2CV_reg <- lm(mod2CV_all$log_pm10~mod2CV_all$predicted))




####################################################################################################################################################
# I try again putting the temperature in the random component but only with random slopes by day, not random intercepts

# Now the model converges and the R2 is 0.6135


m.formula <- as.formula(  log_pm10 ~   log_aod + as.factor(nome_zona) + log_aod:as.factor(nome_zona) + 
                            flag_sea + flag_lake + as.factor(desc_zone) + as.factor(desc_monitor) + 
                            dust + ns(log_pbl,3) + log_restot + log_ndvi + as.factor(season) + ns(elevation.10,3) + isa +
                            ns(log_near_a1,3) + log_near_a2 + log_near_a3 + near_airport.1000 + near_port.1000 +
                            length_a1.1000 + length_a23.1000 + r_sum_length_a1.1000 + r_sum_length_a23.1000 + 
                            pct_deciduous + pct_evergreen + pct_crop + pct_pasture + pct_shrub + pct_high_dev + pct_low_dev +
                            log(near_emip) + nox_2010p.100 + nh3_2005p + r_sum_nox_2010p.100 + r_sum_nh3_2010p +
                            log_so2_2010a + log_nox_2010a + log_nh3_2010a + 
                            rh + speed_ms + visib_km + temp_c +
                            (1+aod|day/nome_zona) + (0+temp_c|day) )
out.m <- lmer(m.formula, data=mod2b, na.rm=TRUE)
summary(out.m)
mod2$pred.prova  <- predict(out.m)
#mod2$resid.prova <- residuals(out.m)
summary(lm(log_pm10~pred.prova,data=mod2))

# Apparently, the temperature as random does not contribute (we should check the CV-R2) and it takes a lot of time, so from now on I consider my base model
# the first one of those tried before.
####################################################################################################################################################




# I try the following steps:

# 1. Identify monitoring sites with very low or high residuals, and check for variables with different avg. values across levels of residuals
# 2. Once identified these variables, try to put some of them in interaction with AOD (make three classes for them) and others as splines
# 3. Run predictions VS observed by monitors and identify the monitors with poorest performance and plot them
# 4. Again, check for variables with different avg. values across levels of residuals


##### POINT 1.

m.formula <- as.formula( log_pm10 ~   log_aod + as.factor(nome_zona) + log_aod:as.factor(nome_zona) + as.factor(season) + ns(log_pbl,3) + ns(speed_ms,3) +
                         flag_sea + flag_lake + as.factor(desc_zone) + as.factor(desc_monitor) + 
                         dust + log_restot + log_ndvi + ns(elevation.10,3) + isa +
                         ns(log_near_a1,3) + log_near_a2 + log_near_a3 + near_airport.1000 + near_port.1000 +
                         length_a1.1000 + length_a23.1000 + r_sum_length_a1.1000 + r_sum_length_a23.1000 + 
                         pct_deciduous + pct_evergreen + pct_crop + pct_pasture + pct_shrub + pct_high_dev + pct_low_dev +
                         log(near_emip) + nox_2010p.100 + nh3_2005p + r_sum_nox_2010p.100 + r_sum_nh3_2010p +
                         log_so2_2010a + log_nox_2010a + log_nh3_2010a +
                         ns(rh10,3) + ns(visib_km,3) + ns(temp_c,3) +
                           (1+aod|day/nome_zona))
out.m <- lmer(m.formula, data=mod2, na.rm=TRUE)
summary(out.m)
mod2$pred  <- predict(out.m)
mod2$resid <- residuals(out.m)
summary(lm(log_pm10~pred,data=mod2))

write.dta(mod2,file="F:\\stage one\\stage_one_modpred.dta", version=11)


##### POINT 2.

# In Stata I have: a) identified the variables, 2) created for each variable a corresponding var called "var_cl3" in terciles

mod2b <- read.dta("F:\\stage one\\stage_one_modpred.dta")

m2.formula <- as.formula( log_pm10 ~   log_aod + as.factor(nome_zona) + log_aod:as.factor(nome_zona) + as.factor(season) + ns(log_pbl,2) + ns(speed_ms,2) +
                           flag_sea + flag_lake + as.factor(desc_zone) + as.factor(desc_monitor) + 
                           dust + ns(log_restot,2) + log_ndvi + as.factor(elevation_10_cl3) + log_aod:as.factor(elevation_10_cl3) + 
                           as.factor(isa_cl3) + log_aod:as.factor(isa_cl3)+
                           ns(log_near_a1,2) + log_near_a2 + log_near_a3 + near_airport_1000 + ns(near_port_1000,2) +
                           ns(length_a1_1000,2) + ns(length_a23_1000,2) + r_sum_length_a1_1000 + r_sum_length_a23_1000 + 
                           as.factor(pct_deciduous_cl3) + log_aod:as.factor(pct_deciduous_cl3) +
                           pct_evergreen + pct_crop + pct_pasture + pct_shrub + ns(pct_high_dev,2) + ns(pct_low_dev,2) +
                           log(near_emip) + nox_2010p_100 + nh3_2005p + r_sum_nox_2010p_100 + r_sum_nh3_2010p +
                           log_so2_2010a + log_nox_2010a + log_nh3_2010a +
                           ns(rh10,2) + ns(visib_km,2) + ns(temp_c,2) +
                           (1+aod|day/nome_zona))
out.m <- lmer(m2.formula, data=mod2b, na.rm=TRUE)
summary(out.m)
mod2b$pred2  <- predict(out.m)
mod2b$resid2 <- residuals(out.m)
summary(lm(log_pm10~pred2,data=mod2b))

# I simplify the previous model based on the summary and then run CV model


m3.formula <- as.formula( log_pm10 ~   log_aod + as.factor(nome_zona) + log_aod:as.factor(nome_zona) + as.factor(season) + ns(log_pbl,2) + ns(speed_ms,2) +
                            flag_sea + flag_lake + as.factor(desc_zone) + as.factor(desc_monitor) + 
                            dust + ns(log_restot,2) + log_ndvi + as.factor(elevation_10_cl3) + log_aod:as.factor(elevation_10_cl3) + 
                            as.factor(isa_cl3) + log_aod:as.factor(isa_cl3)+
                            log_near_a1 + log_near_a2 + log_near_a3 + near_airport_1000 + near_port_1000 +
                            length_a1_1000 + ns(length_a23_1000,2) + r_sum_length_a1_1000 + r_sum_length_a23_1000 + 
                            pct_deciduous + pct_evergreen + pct_crop + pct_pasture + pct_shrub + pct_high_dev + pct_low_dev +
                            log(near_emip) + nox_2010p_100 + nh3_2005p + r_sum_nox_2010p_100 + r_sum_nh3_2010p +
                            log_so2_2010a + log_nox_2010a + log_nh3_2010a +
                            rh + ns(visib_km,2) + ns(temp_c,2) +
                            (1+aod|day/nome_zona))
out.m <- lmer(m3.formula, data=subset(mod2b,elevation<=600, na.rm=TRUE))

data3<- subset(mod2b,elevation<=600)
summary(out.m)
pred3  <- predict(out.m)
resid3 <- residuals(out.m)
summary(lm(log_pm10~pred3,data=data3))



#### Try LOMBARDIA


lomb <- subset(mod2b,desc_region=="LOMBARDIA")
dim(lomb)



m3.formula <- as.formula( log_pm10 ~   log_aod +  as.factor(season) + ns(log_pbl,2) + ns(speed_ms,2) +
                            flag_sea + flag_lake + as.factor(desc_zone) + as.factor(desc_monitor) + 
                            dust + ns(log_restot,2) + log_ndvi + elevation_10+ 
                            as.factor(isa_cl3) + log_aod:as.factor(isa_cl3)+
                            log_near_a1 + log_near_a2 + log_near_a3 + near_airport_1000 + near_port_1000 +
                            length_a1_1000 + ns(length_a23_1000,2) + r_sum_length_a1_1000 + r_sum_length_a23_1000 + 
                            pct_deciduous + pct_evergreen + pct_crop + pct_pasture + pct_shrub + pct_high_dev + pct_low_dev +
                            log(near_emip) + nox_2010p_100 + nh3_2005p + r_sum_nox_2010p_100 + r_sum_nh3_2010p +
                            log_so2_2010a + log_nox_2010a + log_nh3_2010a +
                            rh + ns(visib_km,2) + ns(temp_c,2) +
                            (1+aod+temp_c|day))
out.m <- lmer(m3.formula, data=mod2b)

names(out.m)
mod2b$pred3  <- predict(out.m)
resid3 <- residuals(out.m)
summary(lm(log_pm10~pred3,data=mod2b))
mod3.formula<-as.formula(log_pm10 ~   log_aod+(1+aod+temp_c|day))

library(plyr)


modelList <- dlply(mod2b, "nome_zona", function(x) lmer(mod3.formula, data=x))

mod2b<-as.data.table(mod2b)


mod2b[nome_zona == "Alto adriatico"]$pred <- predict(modelList[[1]])
summary(lm(log_pm10~pred,data=mod2b[nome_zona == "Alto adriatico"])) 



mod2b[nome_zona == "Alto tirreno"]$pred <- predict(modelList[[2]])
summary(lm(log_pm10~pred,data=mod2b[nome_zona == "Alto tirreno"])) 


mod2b[nome_zona == "Appennino"]$pred <- predict(modelList[[3]])
summary(lm(log_pm10~pred,data=mod2b[nome_zona == "Appennino"]))

mod2b[nome_zona == "Arco alpino"]$pred <- predict(modelList[[4]])
summary(lm(log_pm10~pred,data=mod2b[nome_zona == "Arco alpino"]))



mod2b<-as.data.table(mod2b)
mod2b[nome_zona == "Basso adriatico e ionio"]$pred <- predict(modelList[[5]])
summary(lm(log_pm10~pred,data=mod2b[nome_zona == "Basso adriatico e ionio"]))

mod2b[nome_zona == "Basso tirreno e Sicilia"]$pred <- predict(modelList[[6]])
summary(lm(log_pm10~pred,data=mod2b[nome_zona == "Basso tirreno e Sicilia"]))

mod2b[nome_zona == "Medio tirreno "]$pred <- predict(modelList[[7]])
summary(lm(log_pm10~pred,data=mod2b[nome_zona == "Medio tirreno "]))

mod2b[nome_zona == "Pianura padana"]$pred <- predict(modelList[[8]])
summary(lm(log_pm10~pred,data=mod2b[nome_zona == "Pianura padana"]))


mod2b[nome_zona == "Sardegna"]$pred <- predict(modelList[[9]])
summary(lm(log_pm10~pred,data=mod2b[nome_zona == "Sardegna"])) 



modelList <- dlply(mod2b, C("site", function(x) lm(m1.formula, data=x)) 
pm25.year<-t(as.data.table(lapply(modelList, function(x) summary(x)$r.squared))) 
8pm25.year 

m1.formula<-(log_pm10 ~log_aod) 

#################BAD STN PM25
rawdf <- ddply(mod2b, c("site"), 
               function(x) {
                 mod1 <- lm(m1.formula, data=x)
                 data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                            nsamps = length(summary(mod1)$resid))
               })
rawdf



rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.001]
bad[,badid := paste(site,sep="")]
#################BAD STN
mod2b[,badid := paste(site,sep="-")]
####Take out bad stations
mod2b <- mod2b[!(mod2b$badid %in% bad$badid), ] 


