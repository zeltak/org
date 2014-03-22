

#############Bos pred
mod1d_all_st<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod4_2003_st.rds")
bp.model.ps<-gam(resm3~s(tden,popden)+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),data=mod1d_all_st)
summary(bp.model.ps)

boslu<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/boslu.rds")
setnames(boslu,"popdens","popden")

boslu$lpm <-predict(bp.model.ps,boslu)

write.csv(boslu,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/boslulpm.csv")



#############NT pred

bp.model.ps<-gam(resm3~s(tden,popden)+s(elev,fx=FALSE,k=4,bs='cr')+s(dist_pemis,fx=FALSE,k=4,bs='cr')+s(dist_A1,fx=FALSE,k=4,bs='cr'),data=mod1d_all_st)
summary(bp.model.ps)

boslu<-readRDS("/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/boslu.rds")
setnames(boslu,"popdens","popden")

boslu$lpm <-predict(bp.model.ps,boslu)

write.csv(boslu,"/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN000_RWORKDIR/boslulpm.csv")