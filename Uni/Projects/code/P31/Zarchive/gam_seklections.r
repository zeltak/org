names(mod1d_all_st)

bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),data=mod1d_all_st)
summary(bp.model.ps)#0.0347


bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr')+pbl*tden,data=mod1d_all_st)
summary(bp.model.ps)#0.0383 


bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr')+s(tden,pbl),data=mod1d_all_st)
summary(bp.model.ps)#0.0383 

bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr')+s(tden,pbl)+pbl*WDSP+s(tden,WDSP),data=mod1d_all_st)
summary(bp.model.ps)#0.056 


bp.model.ps<-gam(resm3~s(tden)+s(popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr')+s(tden,pbl)+pbl*WDSP+s(tden,WDSP),data=mod1d_all_st)
summary(bp.model.ps)#0.0537


bp.model.ps<-gam(resm3~s(tden)+s(popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr')+s(tden,pbl)+pbl*WDSP+s(tden,WDSP)+tempc+ah_gm3+visib+NDVI,data=mod1d_all_st)
summary(bp.model.ps)#0.0657

#mixed, not that great
mmbp.model.ps<-gamm(resm3~s(tden)+s(popden)+s(pcturban,fx=FALSE,k=4,bs='cr')+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr')+s(tden,pbl)+s(tden,WDSP)+tempc+ah_gm3+visib+NDVI,random=list(day=~1),data=mod1d_all_st)
summary(mmbp.model.ps$gam)


bp.model.ps<-gam(resm3~s(tden)+s(popden)+s(pcturban)+s(elev)+s(dist_pemis)+s(dist_A1)+s(tden,pbl)+pbl*WDSP+s(tden,WDSP)+tempc+ah_gm3+visib+NDVI,data=mod1d_all_st)
summary(bp.model.ps)#0.079


bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban)+s(elev)+s(dist_pemis)+s(dist_A1)+s(tden,pbl)+pbl*WDSP+s(tden,WDSP)+tempc+ah_gm3+visib+NDVI,data=mod1d_all_st)
summary(bp.model.ps)#0.0877


bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban)+s(elev)+s(dist_pemis)+s(dist_A1)+s(tden,pbl)+s(pbl*WDSP)+s(tden,WDSP)+tempc+ah_gm3+visib,data=mod1d_all_st)
summary(bp.model.ps)#0.0916

#$$$$$
bp.model.ps<-gam(resm3~s(tden,popden)+s(pcturban)+s(elev)+s(dist_pemis)+s(dist_A1)+s(tden,pbl)+s(pbl*WDSP)+s(tden,WDSP)+s(tden,tempc)+ah_gm3+s(tden,visib),data=mod1d_all_st)
summary(bp.model.ps)#0.118



