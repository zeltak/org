bt<-fread("c:/Users/ekloog/Documents/tmp/bdata_test.csv")
names(bt)
na.omit(bt)
#bwmod<-lm(birthw~ges_calc,data=bt)
bwmod <- lmer(birthw ~ ges_calc+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+ (1 |FIPS),data =  bt)
bt$res<-resid(bwmod)
bwmod2<-lm(res~IQRfintempmabirth,data=bt)
summary(bwmod2)

fbwmod<-lm(res~IQRfintempmabirth,data=bt,subset=(gender==1))
summary(fbwmod)

mbwmod<-lm(res~IQRfintempmabirth,data=bt,subset=(gender==2))
summary(mbwmod)
