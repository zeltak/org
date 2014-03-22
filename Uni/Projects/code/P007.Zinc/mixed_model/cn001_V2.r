library(foreign) 
library(pastecs)
library(nlme)
library(mgcv)
library(MASS)
library(lme4)
library(car) 


#import and EDA
zinc <- read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.7.Zinc/3.1.7.4.Work/2.Gather_data/export to R/all_orig.csv", header=T) 

names(zinc)
hist(zinc$Znppm)
summary(zinc)
zinc$crop <- as.factor(zinc$crop_type)

#Final model Zinc

zinc5 <- lmer( Znppm ~  CO+iregg+nitro+(1|Paircount)+(1+CO|crop)+(1|crop/Cultivar),na.action=na.omit,
    data =  zinc) 
summary(zinc5)
#remeber that crop 4 is missing cultivars and therefor won't have a random intercept
ranz_5 <- ranef(zinc5)

VarCorr(zinc5)

(VC <- VarCorr(zinc5))
VC$crop

stddevs <- function(u1)
    unlist(lapply(VarCorr(zinc5), function(VC$crop) sqrt(diag(VC$crop))))


stddevs <- function(obj)
    unlist(lapply(VarCorr(obj), function(m) sqrt(diag(m))))





# log

zinc5l <- lmer( log(Znppm) ~  CO+iregg+nitro+(1|Paircount)+(1+CO|crop)+(1|crop/Cultivar),na.action=na.omit,
    data =  zinc) 
summary(zinc5l)
#remeber that crop 4 is missing cultivars and therefor won't have a random intercept
ranz_5l <- ranef(zinc5l)

names(zinc)

#by crops (crop 4 has multiple missing and thus dropped out)
# 
# table(zinc$Cultivar[zinc$crop==1])
# table(zinc$cultiv[zinc$crop==1])
# table(zinc$iregg[zinc$crop==1])
# table(zinc$nitro[zinc$crop==1])
# head(zinc$Cultivar)

zinc_crop1 <- lmer( log(Znppm) ~  CO+(1|Paircount)+as.factor(Cultivar),na.action=na.omit, subset=crop==1, data =  zinc) 
summary(zinc_crop1)

zinc_crop2 <- lmer( log(Znppm) ~  CO+(1|Paircount)+as.factor(Cultivar),na.action=na.omit, subset=crop==2, data =  zinc) 
summary(zinc_crop2)

zinc_crop3 <- lmer( log(Znppm) ~  CO+(1|Paircount)+as.factor(Cultivar),na.action=na.omit, subset=crop==3, data =  zinc) 
summary(zinc_crop3)

zinc_crop4 <- lmer( log(Znppm) ~  CO+(1|Paircount)+as.factor(cultiv),na.action=na.omit, subset=crop==4, data =  zinc) 
summary(zinc_crop4)

zinc_crop5 <- lmer( log(Znppm) ~  CO+(1|Paircount)+as.factor(Cultivar),na.action=na.omit, subset=crop==5, data =  zinc) 
summary(zinc_crop5)

zinc_crop6 <- lmer( log(Znppm) ~  CO+(1|Paircount)+as.factor(Cultivar),na.action=na.omit, subset=crop==6, data =  zinc) 
summary(zinc_crop6)



zinc_crop6 <- lmer( log(Znppm) ~  CO+(1|Paircount)+as.factor(Cultivar)*CO,na.action=na.omit, subset=crop==6, data =  zinc) 


ranef.lme4 <- attributes(zinc_crop6)$fixef #extracting the fixed coef


#for each clutivar compute the effect of CO for each cultivar
ranef.lme4[2]+ranef.lme4[10] 
sqrt(vcov(zinc_crop6)[2,2]+vcov(zinc_crop6)[10,10]+2*vcov(zinc_crop6)[2,10])
ranef.lme4[2]+ranef.lme4[11]
sqrt(vcov(zinc_crop6)[2,2]+vcov(zinc_crop6)[11,11]+2*vcov(zinc_crop6)[2,11])
ranef.lme4[2]+ranef.lme4[12]
sqrt(vcov(zinc_crop6)[2,2]+vcov(zinc_crop6)[12,12]+2*vcov(zinc_crop6)[2,12])
ranef.lme4[2]+ranef.lme4[13]
sqrt(vcov(zinc_crop6)[2,2]+vcov(zinc_crop6)[13,13]+2*vcov(zinc_crop6)[2,13])
ranef.lme4[2]+ranef.lme4[14]
sqrt(vcov(zinc_crop6)[2,2]+vcov(zinc_crop6)[14,14]+2*vcov(zinc_crop6)[2,14])
ranef.lme4[2]+ranef.lme4[15]
sqrt(vcov(zinc_crop6)[2,2]+vcov(zinc_crop6)[15,15]+2*vcov(zinc_crop6)[2,15])
ranef.lme4[2]+ranef.lme4[16]
sqrt(vcov(zinc_crop6)[2,2]+vcov(zinc_crop6)[16,16]+2*vcov(zinc_crop6)[2,16])


     
     
     
     
     
     
     
     
     
     
     


####IRON#############
#with cultivar

iron5 <- lmer( Feppm ~  CO+iregg+nitro+(1|Paircount)+(1+CO|crop)+(1|crop/Cultivar),na.action=na.omit,
    data =  zinc) 
summary(iron5)
#remeber that crop 4 is missing cultivars and therefor won't have a random intercept
rani_5 <- ranef(iron5)


# log

iron5l <- lmer( log(Feppm) ~  CO+iregg+nitro+(1|Paircount)+(1+CO|crop)+(1|crop/Cultivar),na.action=na.omit,
    data =  zinc) 
summary(iron5l)
#remeber that crop 4 is missing cultivars and therefor won't have a random intercept
rani_5l <- ranef(iron5l)

#crops

iron_crop1 <- lmer( log(Feppm) ~  CO+(1|Paircount)+as.factor(Cultivar),na.action=na.omit, subset=crop==1, data =  zinc) 
summary(iron_crop1)


iron_crop2 <- lmer( log(Feppm) ~  CO+(1|Paircount)+as.factor(Cultivar),na.action=na.omit, subset=crop==2, data =  zinc) 
summary(iron_crop2)

iron_crop3 <- lmer( log(Feppm) ~  CO+(1|Paircount)+as.factor(Cultivar),na.action=na.omit, subset=crop==3, data =  zinc) 
summary(iron_crop3)

iron_crop4 <- lmer( log(Feppm) ~  CO+(1|Paircount)+as.factor(cultiv),na.action=na.omit, subset=crop==4, data =  zinc) 
summary(iron_crop4)

iron_crop5 <- lmer( log(Feppm) ~  CO+(1|Paircount)+as.factor(Cultivar),na.action=na.omit, subset=crop==5, data =  zinc) 
summary(iron_crop5)

iron_crop6 <- lmer( log(Feppm) ~  CO+(1|Paircount)+as.factor(Cultivar),na.action=na.omit, subset=crop==6, data =  zinc) 
summary(iron_crop6)



names(zinc)
####phy#############
#with cultivar

phy5 <- lmer( PhMeanmgg ~  CO+iregg+nitro+(1|Paircount)+(1+CO|crop)+(1|crop/Cultivar),na.action=na.omit,
    data =  zinc) 
summary(phy5)
#remeber that crop 4 is missing cultivars and therefor won't have a random intercept
ranp_5 <- ranef(phy5)


# log

phy5l <- lmer( log(PhMeanmgg) ~  CO+iregg+nitro+(1|Paircount)+(1+CO|crop)+(1|crop/Cultivar),na.action=na.omit,
    data =  zinc) 
summary(phy5l)
#remeber that crop 4 is missing cultivars and therefor won't have a random intercept
ranp_5l <- ranef(phy5l)


#crops

phy_crop1 <- lmer( log(PhMeanmgg) ~  CO+(1|Paircount)+as.factor(Cultivar),na.action=na.omit, subset=crop==1, data =  zinc) 
summary(phy_crop1)


phy_crop2 <- lmer( log(PhMeanmgg) ~  CO+(1|Paircount)+as.factor(Cultivar),na.action=na.omit, subset=crop==2, data =  zinc) 
summary(phy_crop2)

phy_crop3 <- lmer( log(PhMeanmgg) ~  CO+(1|Paircount)+as.factor(Cultivar),na.action=na.omit, subset=crop==3, data =  zinc) 
summary(phy_crop3)

phy_crop4 <- lmer( log(PhMeanmgg) ~  CO+(1|Paircount)+as.factor(cultiv),na.action=na.omit, subset=crop==4, data =  zinc) 
summary(phy_crop4)

phy_crop5 <- lmer( log(PhMeanmgg) ~  CO+(1|Paircount)+as.factor(Cultivar),na.action=na.omit, subset=crop==5, data =  zinc) 
summary(phy_crop5)

phy_crop6 <- lmer( log(PhMeanmgg) ~  CO+(1|Paircount)+as.factor(Cultivar),na.action=na.omit, subset=crop==6, data =  zinc) 
summary(phy_crop6)
