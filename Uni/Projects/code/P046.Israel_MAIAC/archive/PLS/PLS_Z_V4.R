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
#sourcing
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/CV_splits.r")
source("/media/NAS/Uni/org/files/Uni/Projects/code/$Rsnips/rmspe.r")

#add data
m1.2003 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2003.rds")
m1.2004 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2004.rds")
m1.2005 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2005.rds")
m1.2006 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2006.rds")
m1.2007 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2007.rds")
m1.2008 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2008.rds")
m1.2009 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2009.rds")
m1.2010 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2010.rds")
m1.2011 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2011.rds")
m1.2012 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2012.rds")
m1.2013 <-readRDS("/media/NAS/Uni/Projects/P046_Israel_MAIAC/3.Work/2.Gather_data/FN000_RWORKDIR/mod1.AQ.2013.rds")
m1.all <- rbindlist(list(m1.2003,m1.2004,m1.2005,m1.2006,m1.2007,m1.2008,m1.2009,m1.2010,m1.2011,m1.2012,m1.2013), fill=TRUE)
#zscore
m1.all[,elev.s:= scale(elev)]
m1.all[,tden.s:= scale(tden)]
m1.all[,pden.s:= scale(pden)]
m1.all[,dist2A1.s:= scale(dist2A1)]
m1.all[,dist2water.s:= scale(dist2water)]
m1.all[,dist2rail.s:= scale(dist2rail)]
m1.all[,Dist2road.s:= scale(Dist2road)]
m1.all[,ndvi.s:= scale(ndvi)]
m1.all[,MeanPbl.s:= scale(MeanPbl)]
m1.all[,p_ind.s:= scale(p_ind)]
m1.all[,p_for.s:= scale(p_for)]
m1.all[,p_farm.s:= scale(p_farm)]
m1.all[,p_dos.s:= scale(p_dos)]
m1.all[,p_dev.s:= scale(p_dev)]
m1.all[,p_os.s:= scale(p_os)]
m1.all[,tempa.s:= scale(tempa)]
m1.all[,WDa.s:= scale(WDa)]
m1.all[,WSa.s:= scale(WSa)]
m1.all[,RHa.s:= scale(RHa)]
m1.all[,Raina.s:= scale(Raina)]
m1.all[,NO2a.s:= scale(NO2a)]


#transfer all to Z-scores
m1.all$zid<-1 #create id variable
#set direction right so that all variables are positvly associated with PM (IE make pctopenspace positvy 'associated' with pm by multiplying by -1)
m1.all[,elev.s :=  elev.s*-1]
m1.all[,p_os.s :=  p_os.s*-1]
m1.all[,WSa.s :=  WSa.s*-1]
m1.all[,p_for.s :=  p_for.s*-1]
m1.all[,p_farm.s :=  p_farm.s*-1]
m1.all[,ndvi.s :=  ndvi.s*-1]
m1.all[,dist2A1.s :=  dist2A1.s*-1]
m1.all[,dist2water.s :=  dist2water.s*-1]
m1.all[,dist2rail.s :=  dist2rail.s*-1]
m1.all[,Dist2road.s :=  Dist2road.s*-1]


l=seq(names(m1.all));names(l)=names(m1.all);l
#define spatial and temporal variables and create 2 matrixes: s.mat for spatial variables and t.mat for temporal variables
attach(m1.all)
s.mat <- cbind(elev.s,tden.s ,pden.s ,dist2A1.s,dist2water.s,dist2rail.s,Dist2road.s,p_ind.s,p_for.s,p_farm.s,p_dos.s,p_dev.s,p_os.s )
t.mat <- cbind(ndvi.s ,tempa.s,WDa.s,WSa.s,RHa.s,Raina.s)
detach(m1.all)



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

m1.all <- data.frame(m1.all,s.mat,t.mat,st.mat,t.avgs,s.avgs,st.avgs)
names(m1.all) 

#PLS regression
#p=plsr(PM25 ~ elev.s+tden.s +pden.s +dist2A1.s+dist2water.s+dist2rail.s+Dist2road.s+p_ind.s+p_for.s+p_farm.s+p_dos.s+p_dev.s+p_os.s+ndvi.s +tempa.s+WDa.s+WSa.s+RHa.s+Raina.s+X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30+X31+X32+X33+X34+X35+X36+X37+X38+X39+X40+X41+X42+X43+X44+X45+X46+X47+X48+X49+X50+X51+X52+X53+X54+X55+X56+X57+X58+X59+X60+X61+X62+X63+X64+X65+X66+X67+X68+X69+X70+X71+X72+X73+X74+X75+X76+X77+X78,ncomp=5,data = m1.all)

p=plsr(PM25 ~ t.avgs+s.avgs+st.avgs+X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30+X31+X32+X33+X34+X35+X36+X37+X38+X39+X40+X41+X42+X43+X44+X45+X46+X47+X48+X49+X50+X51+X52+X53+X54+X55+X56+X57+X58+X59+X60+X61+X62+X63+X64+X65+X66+X67+X68+X69+X70+X71+X72+X73+X74+X75+X76+X77+X78,ncomp=3,data = m1.all)


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
m1.all<-as.data.table(m1.all)
m1.all$fake<-1
cast1<-as.data.table(cast1)
setkey(m1.all,fake)
setkey(cast1,fake)
merged.all<- merge(m1.all, cast1, all.x = T)

attach(merged.all)
names(merged.all)
merged.all$p1fin<-t.avgs*t.avgs_p1 +s.avgs*s.avgs_p1+st.avgs*st.avgs_p1
detach(merged.all)
merged.all[,p1fin := t.avgs*t.avgs_p1 +s.avgs*s.avgs_p1+st.avgs*st.avgs_p1]
merged.all[,p2fin := t.avgs*t.avgs_p2 +s.avgs*s.avgs_p2+st.avgs*st.avgs_p2]
merged.all[,p3fin := t.avgs*t.avgs_p3 +s.avgs*s.avgs_p3+st.avgs*st.avgs_p3]
#merged.all[,p4fin := t.avgs*t.avgs_p4 +s.avgs*s.avgs_p4+st.avgs*st.avgs_p4]
#merged.all[,p5fin := t.avgs*t.avgs_p5 +s.avgs*s.avgs_p5+st.avgs*st.avgs_p5]
summary(merged.all)
xmerged.all<-na.omit(merged.all)
xmerged.all <- merged.all[p1fin  != 'NA']
xmerged.all <- merged.all[p2fin  != 'NA']
xmerged.all <- merged.all[p3fin  != 'NA']



#USING PPLS model
m1.formula <- as.formula(PM25~ aod
                        +tempa.s+p1fin+p2fin+p3fin
                        +(1+aod|day/reg_num)) #+(1|stn) !!! stn screws up mod3 


m1.fit.all <-  lmer(m1.formula,data=xmerged.all,weights=normwt)
xmerged.all$pred.m1 <- predict(m1.fit.all)
print(summary(lm(PM25~pred.m1,data=xmerged.all))$r.squared)
#RMSPE
print(rmse(residuals(m1.fit.all)))

#X78 casues errors..
m1.fit.all <- lmer(PM25 ~ aod+t.avgs+s.avgs+st.avgs+ (1+aod|day/reg_num)+(1+X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21+X22+X23+X24+X25+X26+X27+X28+X29+X30+X31+X32+X33+X34+X35+X36+X37+X38+X39+X40+X41+X42+X43+X44+X45+X46+X47+X48+X49+X50+X51+X52+X53+X54+X55+X56+X57+X58+X59+X60+X61+X62+X63+X64+X65+X66+X67+X68+X69+X70+X71+X72+X73+X74+X75+X76+X77),data = xmerged.all)

write.dbf(xmerged.all, "~/ZH_tmp/sast.dbf")


#---------------->>>> CV
#s1
splits_s1 <- splitdf(xmerged.all)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(xmerged.all)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(xmerged.all)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(xmerged.all)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(xmerged.all)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"
#s6
splits_s6 <- splitdf(xmerged.all)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(xmerged.all)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(xmerged.all)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(xmerged.all)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(xmerged.all)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#BIND 1 dataset
xmerged.all.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10))
#table updates
m1.fit.all.cv<-lm(PM25~pred.m1.cv,data=xmerged.all.cv)
print(summary(lm(PM25~pred.m1.cv,data=xmerged.all.cv))$r.squared)











