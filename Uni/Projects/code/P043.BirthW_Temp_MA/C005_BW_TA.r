library(data.table)
bd<-fread("c:/Users/ekloog/Documents/tmp/bdata.csv")
list<-names(bd)

#interactions
library(lme4)
xx <- lmer(birthw ~ IQRfintempmabirth+urb_rur*IQRfintempmabirth+urb_rur+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+ (1 |FIPS),data =  bd)
summary(xx)

yy <- lmer(birthw ~ IQRfintempmabirth+furb_rur*IQRfintempmabirth+furb_rur+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob)+ (1 |FIPS),data =  bd)
summary(yy)

rur <-subset(bd, urb_rur == 0)
rur1<-lm(birthw ~ IQRfintempmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob),data =  rur)
summary(rur1)

urb <-subset(bd, urb_rur == 1)
urb1<-lm(birthw ~ IQRfintempmabirth+sinetime+costime+age_centered+age_centered_sq+cig_preg+cig_pre+med_income+p_ospace+gender+prev_400+ diab+hyper+lungd+diab_other+prevpret+as.factor(kess)+as.factor(MRN)+as.factor(edu_group)+as.factor(byob),data =  urb)
summary(urb1)

#calculate diffrances between models

se1 <-summary(rur1)$coef[2,2]
se2 <-summary(urb1)$coef[2,2]
se_diff=sqrt((se1)^2 +(se2)^2) 

be1 <-summary(rur1)$coef[2,1]
be2 <-summary(urb1)$coef[2,1]
be_diff<-be1-be2
bese_diff<-be_diff/se_diff
df_diff<- -40000
t.test(y1,y2)

anova(rur1)
