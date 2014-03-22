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



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Ready datasets
#create a croptype factor variable
zinc$crop <- as.factor(zinc$crop_type)

table((zinc$crop_type))

#zinc only CO
zinc1 <- lme( Znppm ~ CO , na.action=na.omit,
   random = ~1  | Paircount, 
  data =  zinc) 

summary(zinc1)


# #zinc only CO (subset wheat)
# zincrop <- lme( Znppm ~ CO , na.action=na.omit,
#    random = ~1  | Paircount, subset=crop_type==4,
#   data =  zinc) 
# 
# summary(zincrop)



#has a random effect (intercept) for paircound and a random effect for CO+random slope for crop type
# zinc3 <- lmer( Znppm ~  CO+as.factor(year)+(1|Paircount)+(1+CO|crop),na.action=na.omit,
#     data =  zinc) 
# summary(zinc3)
# ##extract SE extract the random coeefiecnt of random slope and add to fixed effect ##NEED TO EXTRACT EACH RANDOM CO COEF AND ADD TO FIXED CO (-2.33)
# ran <- ranef(zinc3)




#Extended model

zinc5 <- lmer( Znppm ~  CO+as.factor(year)+iregg+nitro+(1|Paircount)+(1+CO|crop)+(1|crop/Cultivar),na.action=na.omit,
    data =  zinc) 
summary(zinc5)


#remeber that crop 4 is missing cultivars and therefor won't have a random intercept
ran <- ranef(zinc5)

table(zinc$crop,zinc$Cultivar)

summary(zinc$crop)



# #wheat
# zinc5 <- lmer( Znppm ~  CO+as.factor(year)+as.factor(cultiv)+iregg+nitro+(1|Paircount),na.action=na.omit, subset=crop_type==4,
#   data =  zinc) 
# summary(zinc5)
# 
# 
# #wheat (log)
# zinc6 <- lmer( log(Znppm) ~  CO+as.factor(year)+as.factor(cultiv)+iregg+nitro+(1|Paircount),na.action=na.omit, subset=crop_type==4,
#   data =  zinc) 
# summary(zinc6)







####IRON#############


#wheat
# iron2 <- lmer( Feppm ~  CO+as.factor(year)+as.factor(cultiv)+iregg+nitro+(1|Paircount),na.action=na.omit, subset=crop_type==4,
#   data =  zinc) 
# summary(iron2)
# 
# iron3 <- lmer( Feppm ~ CO+as.factor(year)+as.factor(cultiv)+iregg+nitro+(1|Paircount)+(1+CO|crop),na.action=na.omit,
#     data =  zinc) 
# summary(iron3)



#with cultivar
iron5 <- lmer( Feppm ~  CO+as.factor(year)+iregg+nitro+(1|Paircount)+(1+CO|crop)+(1|crop/Cultivar),na.action=na.omit,
    data =  zinc) 
summary(iron5)

ran <- ranef(iron5)
