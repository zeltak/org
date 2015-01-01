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


#create base dataset

###functions

#cross validation
#create CV table
mod1table <- data.frame(type=character(9), r2003=numeric(9),r2004=numeric(9),r2005=numeric(9),r2006=numeric(9),r2007=numeric(9),r2008=numeric(9),r2009=numeric(9),r2010=numeric(9),r2011=numeric(9),r2012=numeric(9),mean=numeric(9))

#name columns
mod1table$type <- c("N_R2","N_int","N_int_se","N_slope","N_slope_se","RMSPE","spatial","temporal","RMSPE_spatial")


splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}


m1_2003 <-  fread("f:/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN009_ALL_mods_base/mod1_2003n.csv") 
m1_2003[, date := as.Date(strptime(DATE, "%d%b%Y"))]
#clean data
#exclude bad values
m1_2003 <- m1_2003[aod < 1.4 & NDVI < 1 , ]
m1_2003$elevminuspbl<-m1_2003$elev_m-m1_2003$pbl
m1_2003$logroad<-log(m1_2003$Mjrrdden_1 +.1)
# remove old regions
m1_2003[, c("region", "reg") := NULL]

#add regions
reg<-fread("f:/Uni/Projects/p031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/region_guid.csv")
table(reg$region)
setkey(m1_2003,guid)
setkey(reg,guid)
m1_2003_st <- merge(m1_2003, reg, all.x = T)
table(m1_2003_st$region)

#add stacks
stack<-fread("y:/EAST_USA_MAIAC/NEI05/midatlneweng_nei05stacks.csv")
names(stack)
stack <- rename(stack, c(guid_="guid")) 
setkey(m1_2003_st,guid)
setkey(stack,guid)
m1_2003_st <- merge(m1_2003_st, stack, all.x = T)
names(m1_2003_st)


#transfer all to Z-scores
#spatial
#LU
m1_2003_st$S1<-(m1_2003_st$elev_m-mean(m1_2003_st$elev_m))/sd(m1_2003_st$elev_m) 
m1_2003_st$S2<-(m1_2003_st$pop_sqkm-mean(m1_2003_st$pop_sqkm))/sd(m1_2003_st$pop_sqkm)
m1_2003_st$S3<-(m1_2003_st$pcturb_1km-mean(m1_2003_st$pcturb_1km))/sd(m1_2003_st$pcturb_1km)
m1_2003_st$S4<- (m1_2003_st$Mjrrdden_1-mean(m1_2003_st$Mjrrdden_1))/sd(m1_2003_st$Mjrrdden_1)
m1_2003_st$S5<-(m1_2003_st$dist_PE-mean(m1_2003_st$dist_PE))/sd(m1_2003_st$dist_PE)
#NEI
m1_2003_st$S6<-(m1_2003_st$NOXsum-mean(m1_2003_st$NOXsum))/sd(m1_2003_st$NOXsum)
m1_2003_st$S7<-(m1_2003_st$PM10sum-mean(m1_2003_st$PM10sum))/sd(m1_2003_st$PM10sum)
m1_2003_st$S8<-(m1_2003_st$SO2sum-mean(m1_2003_st$SO2sum))/sd(m1_2003_st$SO2sum)
m1_2003_st$S9<-(m1_2003_st$nei05nonpntcntypm25-mean(m1_2003_st$nei05nonpntcntypm25))/sd(m1_2003_st$nei05nonpntcntypm25)

m1_2003_st$S10<-(m1_2003_st$so2stge30_15k-mean(m1_2003_st$so2stge30_15k))/sd(m1_2003_st$so2stge30_15k)
m1_2003_st$S11<-(m1_2003_st$pm10stge30_15k-mean(m1_2003_st$pm10stge30_15k))/sd(m1_2003_st$pm10stge30_15k)
m1_2003_st$S12<-(m1_2003_st$noxstge30_15k-mean(m1_2003_st$noxstge30_15k))/sd(m1_2003_st$noxstge30_15k)
m1_2003_st$S13<-(m1_2003_st$pm25stge30_15k-mean(m1_2003_st$pm25stge30_15k))/sd(m1_2003_st$pm25stge30_15k)

m1_2003_st$S14<-(m1_2003_st$so2stlt30_3k-mean(m1_2003_st$so2stlt30_3k))/sd(m1_2003_st$so2stlt30_3k)
m1_2003_st$S15<-(m1_2003_st$pm10stlt30_3k-mean(m1_2003_st$pm10stlt30_3k))/sd(m1_2003_st$pm10stlt30_3k)
m1_2003_st$S16<-(m1_2003_st$noxstlt30_3k-mean(m1_2003_st$noxstlt30_3k))/sd(m1_2003_st$noxstlt30_3k)
m1_2003_st$S17<-(m1_2003_st$pm25stlt30_3k-mean(m1_2003_st$pm25stlt30_3k))/sd(m1_2003_st$pm25stlt30_3k)

#temporal
#met
m1_2003_st$T1<- (m1_2003_st$visib-mean(m1_2003_st$visib))/sd(m1_2003_st$visib)
m1_2003_st$T2<-(m1_2003_st$WDSP-mean(m1_2003_st$WDSP))/sd(m1_2003_st$WDSP)
m1_2003_st$T3<-(m1_2003_st$ah_gm3-mean(m1_2003_st$ah_gm3))/sd(m1_2003_st$ah_gm3)
m1_2003_st$T4<-(m1_2003_st$NDVI-mean(m1_2003_st$NDVI))/sd(m1_2003_st$NDVI)
m1_2003_st$T5<-(m1_2003_st$pbl-mean(m1_2003_st$pbl))/sd(m1_2003_st$pbl)

m1_2003_st$zid<-1 #create id variable

#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
m1_2003_st$S1 <-m1_2003_st$S1*-1
m1_2003_st$S5 <-m1_2003_st$S5*-1

m1_2003_st$T1 <- m1_2003_st$T1*-1
m1_2003_st$T2 <- m1_2003_st$T2*-1
m1_2003_st$T4 <- m1_2003_st$T4*-1
m1_2003_st$T5 <- m1_2003_st$T5*-1






#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables

attach(m1_2003_st)
s.mat <- cbind(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,S11,S12,S13,S14,S15,S16,S17)
t.mat <- cbind(T1,T2,T3,T4,T5)
detach(m1_2003_st)


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

m1_2003_st <- data.frame(m1_2003_st,st.mat,t.avgs,s.avgs,st.avgs)
names(m1_2003_st) 

#PLS regression
p=plsr(PM25 ~ S1+S2+S3+S4+S5+S6+S7+S8+S9+S10+S11+S12+S13+S14+S15+S16+S17+T1+T2+T3+T4+T5+X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30+X31+X32+X33+X34+X35+X36+X37+X38+X39+X40+X41+X42+X43+X44+X45+X46+X47+X48+X49+X50+X51+X52+X53+X54+X55+X56+X57+X58+X59+X60+X61+X62+X63+X64+X65+X66+X67+X68+X69+X70+X71+X72+X73+X74+X75+X76+X77+X78+X79+X80+X81+X82+X83+X84+X85,ncomp=5,data = m1_2003_st)

#to decide number of components
#plot(RMSEP(p), legendpos = "topright")
plot(R2(p))
#R2(p)



mydat <- data.frame(p$coef)
mydat<- rename(mydat, c(PM25.1.comps="p1",PM25.2.comps="p2",PM25.3.comps="p3",PM25.4.comps="p4",PM25.5.comps="p5")) 
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
attach(merged2003)
names(merged2003)


merged2003$p1fin<-t.avgs*t.avgs_p1 +s.avgs*s.avgs_p1+st.avgs*st.avgs_p1

  
 



detach(merged2003)


###model run
#non PLS model
#out.m1_2003 = lmer(PM25 ~ aod+tempc+WDSP+NDVI+ah_gm3+visib+aod*pbl+pbl+dist_PE+pcturb_1km+logroad+nei05nonpntcntypm25+pop_sqkm+elev_m+NOXsum+PM10sum+SO2sum+(1+aod+tempc|date),data = merged2003) 

#USING PPLS model
#out.m1_2003 = lmer(PM25 ~ aod+tempc+WDSP+NDVI+ah_gm3+visib+aod*pbl+pbl+p1fin+p2fin+p3fin+p4fin+p5fin+(1+aod+tempc|date),data = merged2003)

out.m1_2003 = lmer(PM25 ~ aod+tempc+p1fin+p2fin+p3fin+(1+aod+tempc|date),data = merged2003)




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


















