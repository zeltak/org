###############
#LIBS
###############
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
library(car)
library(dplyr)
library(ggmap)
library(broom)
library(splines)
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")



###############
#TABLES
###############
#create main CV table
mod1table <- data.frame(type=character(43), r2003=numeric(43),
                        r2004=numeric(43),r2005=numeric(43),
                        r2006=numeric(43),r2003=numeric(43),
                        r2008=numeric(43),r2009=numeric(43),
                        r2010=numeric(43),r2011=numeric(43),
                        r2012=numeric(43),mean=numeric(43))

#name columns

mod1table$type<- c("mod1_R2","mod_RMSPE","mod1_spatial","mod1_temporal","mod1CV_R2","mod1CV_int","mod1CV_int_SE",
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





### import data
m1.2003 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2003.rds")

### subset to aqua and apply alexei cleaning methods
m1.2003<-m1.2003[MaskAdjacency == "000" & UN > 0 & UN < 0.04] 

################# clean BAD STN PM25 and check if improved model?
rawdf <- ddply(m1.2003, c( "stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ aod, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad<- rawdf[R2< 0.05]
bad[,badid := paste(stn,sep="-")]
#################BAD STN
m1.2003[,badid := paste(stn,sep="-")]
####Take out bad stations
m1.2003 <- m1.2003[!(m1.2003$badid %in% bad$badid), ] 

#check it does better
m1.formula <- as.formula(PM25~ aod+(1+aod|day))
x<-  lmer(m1.formula,data=m1.2003)
m1.2003$predicted <- predict(x)
glance(lm(PM25~predicted,data=m1.2003))

#get rid of missing
m1.2003 <- na.omit(m1.2003)



m1.2003[,elev.s:= scale(elev)]
m1.2003[,tden.s:= scale(tden)]
m1.2003[,pden.s:= scale(pden)]
m1.2003[,dist2A1.s:= scale(dist2A1)]
m1.2003[,dist2water.s:= scale(dist2water)]
m1.2003[,dist2rail.s:= scale(dist2rail)]
m1.2003[,Dist2road.s:= scale(Dist2road)]
m1.2003[,ndvi.s:= scale(ndvi)]
m1.2003[,MeanPbl.s:= scale(MeanPbl)]
m1.2003[,p_ind.s:= scale(p_ind)]
m1.2003[,p_for.s:= scale(p_for)]
m1.2003[,p_farm.s:= scale(p_farm)]
m1.2003[,p_dos.s:= scale(p_dos)]
m1.2003[,p_dev.s:= scale(p_dev)]
m1.2003[,p_os.s:= scale(p_os)]
m1.2003[,Temp.s:= scale(Temp)]
m1.2003[,WD.s:= scale(WD)]
m1.2003[,WS.s:= scale(WS)]
m1.2003[,RH.s:= scale(RH)]
m1.2003[,Rain.s:= scale(Rain)]

m1.2003[,zid := 1] #create id variable



#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
m1.2003[,elev.s :=  elev.s*-1]
m1.2003[,p_os.s :=  p_os.s*-1]
m1.2003[,WS.s :=  WS.s*-1]
m1.2003[,p_for.s :=  p_for.s*-1]
m1.2003[,p_farm.s :=  p_farm.s*-1]
m1.2003[,ndvi.s :=  ndvi.s*-1]
m1.2003[,dist2A1.s :=  dist2A1.s*-1]
m1.2003[,dist2water.s :=  dist2water.s*-1]
m1.2003[,dist2rail.s :=  dist2rail.s*-1]
m1.2003[,Dist2road.s :=  Dist2road.s*-1]

#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables
attach(m1.2003)
s.mat <- cbind(elev.s,tden.s,pden.s)
t.mat <- cbind(MeanPbl.s,Temp.s)
detach(m1.2003)


#function to create the matrix (st.mat) of interaction between the space variables and time variables

  st.mat <-NULL
  count <- 0
#start of actual function
for (i in 1: ncol(t.mat)){
  for (j in 1: ncol(s.mat)){ 
st.mat <- cbind (st.mat, t.mat[,i]*s.mat[,j])
}
}

#get means for each variable types
t.avgs <-apply(t.mat,1,mean)
s.avgs <-apply(s.mat,1,mean)
st.avgs <-apply(st.mat,1,mean)

m1.2003 <- data.frame(m1.2003,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)




#lme mixed model
#m1.formula <- as.formula(PM25~ aod+(1+aod|day))
m1.formula <- as.formula(PM25~ aod+
                        Temp.s+WD.s+RH.s+WS.s+Dust+Rain+MeanPbl.s #temporal
                        +elev.s+tden.s+pden.s+dist2rail.s+dist2A1.s+Dist2road.s+dist2water.s+ndvi.s+season #spatial
                        +p_os.s+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  #land use
                         +(1+aod|day/reg_num))




#    model pm25 = aod Temp_F_x t_avgs s_avgs  st_avgs  / s outpred=pdataA_&year;
#     random int aod Temp_F_x / sub = date s ;
#   random int aod  / sub = date(reg_id) s;
#     random x1--x18 / sub = zid type=toep(1) s;




#lme mixed model
#m1.formula <- as.formula(PM25~ aod+(1+aod|day))
m1.formula <- as.formula(PM25~ aod+t.avgs+s.avgs+st.avgs+(1+aod|day/reg_num) )
m1.formula <- as.formula(PM25~ aod+(1+aod|day) )



m1.fit.2003 <-  lmer(m1.formula,data=m1.2003,weights=normwt)
m1.2003$predicted <- predict(m1.fit.2003)
summary(lm(PM25~predicted,data=m1.2003))$r.squared 


######## CV
#s1
splits_s1 <- splitdf(m1.2003)
mod1d_10_s1 <- splits_s1$trainset
mod1d_90_s1 <- splits_s1$testset
out_90_s1 =  lmer(m1.formula,data =  mod1d_90_s1,weights=normwt)
mod1d_10_s1$predicted <- predict(object=out_90_s1,newdata=mod1d_10_s1,allow.new.levels=TRUE,re.form=NULL )


#s2
splits_s2 <- splitdf(m1.2003)
mod1d_10_s2 <- splits_s2$trainset
mod1d_90_s2 <- splits_s2$testset
out_90_s2 =  lmer(m1.formula,data =  mod1d_90_s2,weights=normwt)
mod1d_10_s2$predicted <- predict(object=out_90_s2,newdata=mod1d_10_s2,allow.new.levels=TRUE,re.form=NULL )

#s3
splits_s3 <- splitdf(m1.2003)
mod1d_10_s3 <- splits_s3$trainset
mod1d_90_s3 <- splits_s3$testset
out_90_s3 =  lmer(m1.formula,data =  mod1d_90_s3,weights=normwt)
mod1d_10_s3$predicted <- predict(object=out_90_s3,newdata=mod1d_10_s3,allow.new.levels=TRUE,re.form=NULL )

#s4
splits_s4 <- splitdf(m1.2003)
mod1d_10_s4 <- splits_s4$trainset
mod1d_90_s4 <- splits_s4$testset
out_90_s4 =  lmer(m1.formula,data =  mod1d_90_s4,weights=normwt)
mod1d_10_s4$predicted <- predict(object=out_90_s4,newdata=mod1d_10_s4,allow.new.levels=TRUE,re.form=NULL )

#s5
splits_s5 <- splitdf(m1.2003)
mod1d_10_s5 <- splits_s5$trainset
mod1d_90_s5 <- splits_s5$testset
out_90_s5 =  lmer(m1.formula,data =  mod1d_90_s5,weights=normwt)
mod1d_10_s5$predicted <- predict(object=out_90_s5,newdata=mod1d_10_s5,allow.new.levels=TRUE,re.form=NULL )


#s6
splits_s6 <- splitdf(m1.2003)
mod1d_10_s6 <- splits_s6$trainset
mod1d_90_s6 <- splits_s6$testset
out_90_s6 =  lmer(m1.formula,data =  mod1d_90_s6,weights=normwt)
mod1d_10_s6$predicted <- predict(object=out_90_s6,newdata=mod1d_10_s6,allow.new.levels=TRUE,re.form=NULL )


#s7
splits_s7 <- splitdf(m1.2003)
mod1d_10_s7 <- splits_s7$trainset
mod1d_90_s7 <- splits_s7$testset
out_90_s7 =  lmer(m1.formula,data =  mod1d_90_s7,weights=normwt)
mod1d_10_s7$predicted <- predict(object=out_90_s7,newdata=mod1d_10_s7,allow.new.levels=TRUE,re.form=NULL )

#s8
splits_s8 <- splitdf(m1.2003)
mod1d_10_s8 <- splits_s8$trainset
mod1d_90_s8 <- splits_s8$testset
out_90_s8 =  lmer(m1.formula,data =  mod1d_90_s8,weights=normwt)
mod1d_10_s8$predicted <- predict(object=out_90_s8,newdata=mod1d_10_s8,allow.new.levels=TRUE,re.form=NULL )

#s9
splits_s9 <- splitdf(m1.2003)
mod1d_10_s9 <- splits_s9$trainset
mod1d_90_s9 <- splits_s9$testset
out_90_s9 =  lmer(m1.formula,data =  mod1d_90_s9,weights=normwt)
mod1d_10_s9$predicted <- predict(object=out_90_s9,newdata=mod1d_10_s9,allow.new.levels=TRUE,re.form=NULL )

#s10
splits_s10 <- splitdf(m1.2003)
mod1d_10_s10 <- splits_s10$trainset
mod1d_90_s10 <- splits_s10$testset
out_90_s10 =  lmer(m1.formula,data =  mod1d_90_s10,weights=normwt)
mod1d_10_s10$predicted <- predict(object=out_90_s10,newdata=mod1d_10_s10,allow.new.levels=TRUE,re.form=NULL )
####BIND ALL 10% into 1 dataset
mod1CV_all<- data.table(rbind(mod1d_10_s1,mod1d_10_s2,mod1d_10_s3,mod1d_10_s4,mod1d_10_s5,mod1d_10_s6,mod1d_10_s7,mod1d_10_s8,mod1d_10_s9, mod1d_10_s10))


mod1CV_reg <- lm(mod1CV_all$PM25~mod1CV_all$predicted)
summary(mod1CV_reg)$r.squared #R2
