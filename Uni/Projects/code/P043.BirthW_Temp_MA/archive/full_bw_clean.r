bda<-fread("/media/NAS/Uni/Projects/P043_BirthW_Temp_MA/3.1.11.4.Work/3.Analysis/2.R_analysis/bw_all.csv")



#rename key variables
#ta model
setnames(bda,"IQRfintempmabirth","ta270")
setnames(bda,"IQRfintempma3month","ta90")
setnames(bda,"IQRfintempmamonth","ta30")
setnames(bda,"IQRfintempma2week","ta14")
setnames(bda,"IQRfintempmaweek","ta7")
setnames(bda,"IQRfintempma3","ta3")
setnames(bda,"IQRfintempma1","ta1")
#tdnc
setnames(bda,"IQRtncdcmabirth","ncdc270")
setnames(bda,"IQRtncdcma3month","ncdc90")
setnames(bda,"IQRtncdcmamonth","ncdc30")
setnames(bda,"IQRtncdcma2week","ncdc14")
setnames(bda,"IQRtncdcmaweek","ncdc7")
setnames(bda,"IQRtncdcma3","ncdc3")
setnames(bda,"IQRtncdcma1","ncdc1")
#PM
setnames(bda,"pmnewmabirth","pm270")
setnames(bda,"pmnewma3month","pm90")
setnames(bda,"pmnewmamonth","pm30")
setnames(bda,"pmnewma2week","pm14")
setnames(bda,"pmnewmaweek","pm7")
setnames(bda,"pmnewma3","pm3")
setnames(bda,"pmnewma1","pm1")


## dates
bda[, day:=as.Date(strptime(date, "%d/%m/%y"))]
bda$rfpisga[bda$rfpisga== 9] <- NA
bda$rfpisga[bda$rfpisga== 2] <- 0

#delete bad data
bda<- bda[ges_calc  >= 20] #delete less then 20 weeks
bda<- bda[ges_calc  <= 45] #delete more then 45 weeks



###recoding
#finer race
bda<- bda[mother_race  == 1 , mrn.n  := 1] #white
bda<- bda[mother_race  == 5 , mrn.n  := 2] #hispanic
bda<- bda[mother_race  == 2 , mrn.n  := 3] #black
bda<- bda[mother_race  == 3 , mrn.n  := 4] #asian
bda<- bda[mother_race  == 4 , mrn.n  := 5] #other
bda<- bda[mother_race  == 8 , mrn.n  := 5] #other
bda<- bda[mother_race  == 9 , mrn.n  := 5] #other


# bda<- bda[plural == 1] #delete plural births
bda<- bda[csect != 1] #delete c-section births


#create preterm
bda<- bda[ges_calc  >= 37 , pterm  := 0] #full term
bda<- bda[ges_calc  < 37 , pterm  := 1] #preterm
## calcute SGA
sgamodel <- lm(birthw ~ges_calc+gender+as.factor(mrn.n),data =  bda)
bda$sga <- resid(sgamodel)
describe(bda$sga)
bda <- bda[sga  >= -584.3  , sgaq  := 0] # NON sga
bda <- bda[sga  < -584.3  , sgaq  := 1] # sga group
## calcute LBW
bda<- bda[birthw  >= 37 , pterm  := 0] #full term
bda<- bda[ges_calc  < 37 , pterm  := 1] #preterm


pt270.modx<-(glmmPQL(pterm ~ ta270+pm270+ +sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity+as.factor(FRN), random = ~ 1 | FIPS ,family=binomial, data =  bda))
round(summary(pt270.modx)$tTable,4)





##works
n1 <- glm(NSGA ~ ta270+pm270+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity,data=bd,family=binomial)
summary(n1)


##works
n1 <- glm(lowbw ~ ta270+pm270+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre++med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+parity,data=bd,family=binomial)
summary(n1)




n2<-(glmmPQL(lowbw ~ ta270+pm270 +sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(mrn.n)+as.factor(edu_group)+as.factor(byob)+parity, random = ~ 1 | FIPS ,family=binomial, data =  bd))
round(summary(n2)$tTable,4)

