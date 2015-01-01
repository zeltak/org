library(pls)
library(reshape)
library(lme4)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)

m1_2003_st<-  read.csv("c:/Users/ekloog/Documents/tmp/m1_2003_st.csv", header=T)  


#create base dataset

###functions

#cross validation
#create CV table
mod1table <- data.frame(type=character(9), r2003=numeric(9),r2004=numeric(9),r2005=numeric(9),r2006=numeric(9),r2007=numeric(9),r2008=numeric(9),r2009=numeric(9),r2010=numeric(9),r2011=numeric(9),r2012=numeric(9),mean=numeric(9))

#name columns
mod1table$type <- c("N_R2","N_int","N_int_se","N_slope","N_slope_se","RMSPE","spatial","temporal","RMSPE_spatial")



#PLS regression
p=plsr(PM25 ~ dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+NOXsum+PM10sum+SO2sum +  pm25stge30_15k  +  pm25stlt30_3k  +    pm10stge30_15k  +  pm10stlt30_3k  +   noxstge30_15k  +   noxstlt30_3k  +     so2stge30_15k  +   so2stlt30_3k  ,ncomp=10,data = m1_2003_st)
#to decide number of components
plot(RMSEP(p), legendpos = "topright")
plot(R2(p))
R2(p)


#cast and meld (transpose)
mydat <- data.frame(p$coef)
mydat<- rename(mydat, c(PM25.1.comps="p1",PM25.2.comps="p2",PM25.3.comps="p3",PM25.4.comps="p4",PM25.5.comps="p5",PM25.6.comps="p6",PM25.7.comps="p7")) 
mydat["rowname"]<-row.names(mydat)
mydat$fake<-1
melt1<-melt(mydat, id=c("fake", "rowname")) 
cast1<-cast(melt1, fake ~ rowname+variable)
names(cast1)


#joing to mod1 dataset
m1_2003_st<-as.data.table(m1_2003_st)
m1_2003_st$fake<-1
cast1<-as.data.table(cast1)
setkey(m1_2003_st,fake)
setkey(cast1,fake)
merged2003 <- merge(m1_2003_st, cast1, all.x = T)

#calculate new variales for mod1
attach(merged2003)

merged2003$p1fin<-dist_PE*dist_PE_p1 +  pcturb_1km*pcturb_1km_p1+  logroad*logroad_p1+  nei05nonpntcntypm25*nei05nonpntcntypm25_p1+  pop_sqkm*pop_sqkm_p1+  elev_m*elev_m_p1+  NOXsum*NOXsum_p1+  PM10sum*PM10sum_p1+ SO2sum*SO2sum_p1

merged2003$p2fin<-dist_PE*dist_PE_p2 +  pcturb_1km*pcturb_1km_p2+  logroad*logroad_p2+  nei05nonpntcntypm25*nei05nonpntcntypm25_p2+  pop_sqkm*pop_sqkm_p2+  elev_m*elev_m_p2+  NOXsum*NOXsum_p2+  PM10sum*PM10sum_p2+ SO2sum*SO2sum_p2

merged2003$p3fin<-dist_PE*dist_PE_p3 +  pcturb_1km*pcturb_1km_p3+  logroad*logroad_p3+  nei05nonpntcntypm25*nei05nonpntcntypm25_p3+  pop_sqkm*pop_sqkm_p3+  elev_m*elev_m_p3+  NOXsum*NOXsum_p3+  PM10sum*PM10sum_p3+ SO2sum*SO2sum_p3

merged2003$p4fin<-dist_PE*dist_PE_p4 +  pcturb_1km*pcturb_1km_p4+  logroad*logroad_p4+  nei05nonpntcntypm25*nei05nonpntcntypm25_p4+  pop_sqkm*pop_sqkm_p4+  elev_m*elev_m_p4+  NOXsum*NOXsum_p4+  PM10sum*PM10sum_p4+ SO2sum*SO2sum_p4

merged2003$p5fin<-dist_PE*dist_PE_p5 +  pcturb_1km*pcturb_1km_p5+  logroad*logroad_p5+  nei05nonpntcntypm25*nei05nonpntcntypm25_p5+  pop_sqkm*pop_sqkm_p5+  elev_m*elev_m_p5+  NOXsum*NOXsum_p5+  PM10sum*PM10sum_p5+ SO2sum*SO2sum_p5

merged2003$p6fin<-dist_PE*dist_PE_p6 +  pcturb_1km*pcturb_1km_p6+  logroad*logroad_p6+  nei05nonpntcntypm25*nei05nonpntcntypm25_p6+  pop_sqkm*pop_sqkm_p6+  elev_m*elev_m_p6+  NOXsum*NOXsum_p6+  PM10sum*PM10sum_p6+ SO2sum*SO2sum_p6

merged2003$p7fin<-dist_PE*dist_PE_p7 +  pcturb_1km*pcturb_1km_p7+  logroad*logroad_p7+  nei05nonpntcntypm25*nei05nonpntcntypm25_p7+  pop_sqkm*pop_sqkm_p7+  elev_m*elev_m_p7+  NOXsum*NOXsum_p7+  PM10sum*PM10sum_p7+ SO2sum*SO2sum_p7

detach(merged2003)


###model run

#USING PLS model
out.m1_2003 = lmer(PM25 ~ aod+tempc+WDSP+NDVI+ah_gm3+visib+aod*pbl+pbl+p1fin+p2fin+p3fin+p4fin+p5fin+p6fin+p7fin+(1+aod+tempc|date),data = merged2003)


merged2003$predicted <- predict(out.m1_2003)
mod1d_reg <- lm(merged2003$PM25~merged2003$predicted)
mod1table$r2003[1] <-summary(mod1d_reg)$r.squared
mod1table$r2003[2] <-summary(mod1d_reg)$coef[1,1]
mod1table$r2003[3] <-summary(mod1d_reg)$coef[1,2]
mod1table$r2003[4] <-summary(mod1d_reg)$coef[2,1]
mod1table$r2003[5] <-summary(mod1d_reg)$coef[2,2]
#rmspe
mod1table$r2003[6]<- sqrt(mean(mod1d_reg$residual^2))
aggf<- ddply(merged2003, c("SiteCode"), function(df) return(c(barpm=mean(df$PM25),barpred=mean(df$predicted))))
#spatial
mod_spatial <- lm(barpm ~ barpred, data=aggf)
summary(mod_spatial)
mod1table$r2003[7] <-summary(mod_spatial)$r.squared
aggfdt<-data.table(aggf)


#temporal
setkey(aggfdt,SiteCode)
setkey(merged2003,SiteCode)
dat <- merge(merged2003,aggf, all.x = T)
dat$delpm <-dat$PM25-dat$barpm
dat$delpred <-dat$predicted-dat$barpred
mod_temporal <- lm(delpm ~ delpred, data=dat)
mod1table$r2003[8] <-summary(mod_temporal)$r.squared

#rmspe_spatial (RMSPE of spatial predictions)
dat$spatresid<-dat$barpm-dat$barpred
mod1table$r2003[9]<- sqrt(mean(dat$spatresid^2))

















