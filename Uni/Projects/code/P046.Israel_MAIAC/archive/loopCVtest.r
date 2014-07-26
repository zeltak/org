
###############
#LIBS
###############
#install.packages("Matrix", repos = "http://cran.rstudio.com/", type="source")
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)


###############
#TABLES
###############
#create main CV table
mod1table <- data.frame(type=character(50), rallyears=numeric(50),
                        r2002=numeric(50),r2003=numeric(50),r2004=numeric(50),r2005=numeric(50),
                        r2006=numeric(50),r2007=numeric(50),
                        r2008=numeric(50),r2009=numeric(50),
                        r2010=numeric(50),r2011=numeric(50),
                        r2012=numeric(50),mean=numeric(50))



#name columns

mod1table$type<- c("mod1_R2","it1","it1","it1","it1","it1","it1","it1","it1","it1","it1","mod1CV_R2","mod1CV_int","mod1CV_int_SE",
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






###############
#DATA
###############

###############
#MOD1 PM10
###############
pm10.m1<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.pm10all_reg.RDS")  
summary(pm10.m1)

#clean data and exclude bad values
#pm10.m1$logroad<-log(pm10.m1$Mjrrdden_1 +.1)
#pm10.m1$regf<-as.character(pm10.m1$reg_num)

##terra
pm10.m1 <-  pm10.m1[complete.cases(pm10.m1$PM10),]
pm10.m1 <-  pm10.m1[complete.cases(pm10.m1$aod),]
# pm10.m1 <-  pm10.m1[complete.cases(pm10.m1$Temp),]
# pm10.m1 <-  pm10.m1[complete.cases(pm10.m1$RH),]
# pm10.m1 <-  pm10.m1[complete.cases(pm10.m1$WS),]


#base model for stage 1
#m1.formula <- as.formula(PM10~ aod+(1+aod|day)) #0.80
m1.formula <- as.formula(PM10~ aod+Dust+elev+tden+pden+dist2rail+dist2A1+dist2water+ndvi+season+MeanPbl+p_os+p_dev+p_dos+p_farm+p_for+p_ind+(1+aod|day/reg_num))


#full model 1
out.pm10.m1 = lmer(m1.formula ,data =  pm10.m1,na.action = na.exclude)
#generate prediction
pm10.m1$predicted <- predict(out.pm10.m1)
#get overall R2
mod1_reg <- lm(pm10.m1$PM10~pm10.m1$predicted)
summary(mod1_reg)$r.squared
#0.859


##################
#Cross Validation
##################

for (Yr in 4:4) {
  pm10.m1.y=pm10.m1[which(pm10.m1$c==(2000+Yr)),]
  
  n=dim(pm10.m1.y)[1]
  kk=floor(n*0.9) #create train and test datasets.

  DFT  <-  data.frame(type=character(11),it1=numeric(11),it2=numeric(11),it3=numeric(11),it4=numeric(11),
                                      it5=numeric(11),it6=numeric(11),it7=numeric(11),
                                      it8=numeric(11),it9=numeric(11),
                                      it10=numeric(11),mean=numeric(11))
  
DFT$type<- c("R2","intercept","intercept_SE","Slope","Slope_SE","RMSPE","spatialCV","temporalCv","","","")
  
  
  
  
  for (II in 2:11) {
    ran_row=sample(1:n,kk ) #random sample of 90% of the data (NAN's excluded)
    train=pm10.m1.y[ran_row,]
    test=(pm10.m1.y[-ran_row,])
    
    
    out_90 =  lmer(m1.formula,data =  train,na.action = na.exclude)
    test$predicted <- predict(object=out_90,newdata=test,allow.new.levels=TRUE,re.form=NULL  )
    

    
    outlm <- lm(test$PM10~test$predicted)
    
    DFT[1,II] <-summary(outlm)$r.squared #R2
    DFT[2,II] <-summary(outlm)$coef[1,1] #intercept
    DFT[3,II] <-summary(outlm)$coef[1,2] #intercept SE
    DFT[4,II] <-summary(outlm)$coef[2,1] #Slope
    DFT[5,II] <-summary(outlm)$coef[2,2] #Slope SE
    DFT[6,II] <-sqrt(mean(outlm$residual^2)) #RMSPE
    

    #spatial
    m1CV_agg <- (test[, j=list(barpm=mean(PM10, na.rm = TRUE),barpred=mean(predicted, na.rm = TRUE)),by = stn])  
    mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
    DFT[7,II] <- summary(mod1_spatial)$r.squared
    
    #temporal
    setkey(m1CV_agg ,stn)
    setkey(test,stn)
    dat <- merge(test,m1CV_agg, all.x = T)
    dat$delpm <-dat$PM10-dat$barpm
    dat$delpred <-dat$predicted-dat$barpred
    mod_temporal <- lm(delpm ~ delpred, data=dat)
    mod1table$rallyears[9] <-summary(mod_temporal)$r.squared
    #rmspe_spatial (RMSPE of spatial predictions)
    dat$spatresid<-dat$barpm-dat$barpred
    DFT[8,II]<- sqrt(mean(dat$spatresid^2))    

    
  }
# Renmae DFT each time 
DFT[,11]<-rowMeans(DFT[,2:11])
#DFT[,11]<-rowMedians(as.matrix(DFT[,1:10])
assign(paste0("DF", Yr) , DFT)
  
}

















###############
#MOD1 pm25
###############
pm25.m1<-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN008_model_prep/mod1.pm25all_reg.RDS")  
summary(pm25.m1)

#clean data and exclude bad values
#pm25.m1$logroad<-log(pm25.m1$Mjrrdden_1 +.1)
#pm25.m1$regf<-as.character(pm25.m1$reg_num)

##terra
pm25.m1 <-  pm25.m1[complete.cases(pm25.m1$PM25),]
pm25.m1 <-  pm25.m1[complete.cases(pm25.m1$aod),]
# pm25.m1 <-  pm25.m1[complete.cases(pm25.m1$Temp),]
# pm25.m1 <-  pm25.m1[complete.cases(pm25.m1$RH),]
# pm25.m1 <-  pm25.m1[complete.cases(pm25.m1$WS),]


#base model for stage 1
#m1.formula <- as.formula(pm25~ aod+(1+aod|day)) #0.80
m1.formula <- as.formula(PM25~ aod+Dust+elev+tden+pden+dist2rail+dist2A1+dist2water+ndvi+season+MeanPbl+p_os+p_dev+p_dos+p_farm+p_for+p_ind+(1+aod|day/reg_num))




##################
#Cross Validation
##################

for (Yr in 2:12) {
  pm25.m1.y=pm25.m1[which(pm25.m1$c==(2000+Yr)),]
  
  n=dim(pm25.m1.y)[1]
  kk=floor(n*0.9) #create train and test datasets.
  
  DFT25  <-  data.frame(type=character(11),overall=numeric(11),it1=numeric(11),it2=numeric(11),it3=numeric(11),it4=numeric(11),
                        it5=numeric(11),it6=numeric(11),it7=numeric(11),
                        it8=numeric(11),it9=numeric(11),
                        it10=numeric(11),mean=numeric(11))
  
  DFT25$type<- c("R2","CVR2","intercept","intercept_SE","Slope","Slope_SE","RMSPE","spatialCV","temporalCv","RMSPE_spatial","")
  
  
  #full model 1
  out.pm25.m1 = lmer(m1.formula ,data =  pm25.m1.y,na.action = na.exclude)
  #generate prediction
  pm25.m1.y$predicted <- predict(out.pm25.m1)
  #get overall R2
  mod1_reg <- lm(pm25.m1.y$PM25~pm25.m1.y$predicted)
  DFT25[1,2]<-summary(mod1_reg)$r.squared
  DFT25[1,13]<-dim(pm25.m1.y)[1]
  
  
  
  for (II in 3:12) {
    ran_row=sample(1:n,kk ) #random sample of 90% of the data (NAN's excluded)
    train=pm25.m1.y[ran_row,]
    test=(pm25.m1.y[-ran_row,])
    
    
    out_90 =  lmer(m1.formula,data =  train,na.action = na.exclude)
    test$predicted <- predict(object=out_90,newdata=test,allow.new.levels=TRUE,re.form=NULL  )
    
    
    
    outlm <- lm(test$PM25~test$predicted)
    
    DFT25[2,II] <-summary(outlm)$r.squared #R2
    DFT25[3,II] <-summary(outlm)$coef[1,1] #intercept
    DFT25[4,II] <-summary(outlm)$coef[1,2] #intercept SE
    DFT25[5,II] <-summary(outlm)$coef[2,1] #Slope
    DFT25[6,II] <-summary(outlm)$coef[2,2] #Slope SE
    DFT25[7,II] <-sqrt(mean(outlm$residual^2)) #RMSPE
    
    
    #spatial
    m1CV_agg <- (test[, j=list(barpm=mean(PM25, na.rm = TRUE),barpred=mean(predicted, na.rm = TRUE)),by = stn])  
    mod1_spatial <- lm(barpm ~ barpred, data=m1CV_agg)
    DFT25[8,II] <- summary(mod1_spatial)$r.squared
    
    #temporal
    setkey(m1CV_agg ,stn)
    setkey(test,stn)
    dat <- merge(test,m1CV_agg, all.x = T)
    dat$delpm <-dat$PM25-dat$barpm
    dat$delpred <-dat$predicted-dat$barpred
    mod_temporal <- lm(delpm ~ delpred, data=dat)
    DFT25[9,II] <-summary(mod_temporal)$r.squared
    #rmspe_spatial (RMSPE of spatial predictions)
    dat$spatresid<-dat$barpm-dat$barpred
    DFT25[10,II]<- sqrt(mean(dat$spatresid^2))    
    
    
  }
  # Renmae DFT25 each time 
  DFT25[,13]<-rowMeans(DFT25[,3:12])
  #DFT25[,11]<-rowMedians(as.matrix(DFT25[,1:10])
  assign(paste0("DFT25", Yr) , DFT25)
  assign(paste0("fit_mod1_25", Yr) , mod1_reg)
}

