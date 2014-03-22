library(foreign) 
library(pastecs)
library(nlme)
library(mgcv)
library(MASS)
library(lme4)
library(car) 


#import and EDA
zinc_sum <- read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.7.Zinc/3.1.7.4.Work/2.Gather_data/export to R/all_summary.csv", header=T) 
names(zinc_sum)
summary(zinc_sum$Znreplicates_lag_1)


#create a croptype factor variable
zinc_sum$crop <- as.factor(zinc_sum$crop_type)
summary(zinc_sum$crop_type)
table(zinc_sum$crop_type)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> SUMMARY DATASET

#zinc only CO
zinc1 <- lme( zinc_ppm ~ CO , na.action=na.omit,
   random = ~1  | pairnumber, 
  data =  zinc_sum) 
summary(zinc1)


              
# All crops no wight
# zinc3 <- lmer( zinc_ppm ~  CO+as.factor(year)+(1|pairnumber)+(1+CO|crop),na.action=na.omit,data =  zinc_sum) 
# summary(zinc3)   
# ran <- ranef(zinc3)
#                
#               
# #with weights               
# zinc4 <- lmer( zinc_ppm ~  CO+as.factor(year)+(1|pairnumber)+(1+CO|crop),na.action=na.omit,weights=Znreplicates_lag_1,data =  zinc_sum) 
# summary(zinc4)
# ran <- ranef(zinc4)

#with weights and nitro cultiv irreg
zinc5 <- lmer( zinc_ppm ~  CO+as.factor(year)+iregg+nitro+(1|pairnumber)+(1+CO|crop)+(1|crop/Cultivar),weights=Znreplicates_lag_1,na.action=na.omit,data =  zinc_sum) 
summary(zinc5)   
ran <- ranef(zinc5)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> separate crops with weights
table (zinc_sum$Crop)               

#wheat (8)         
zinc7 <- lmer( zinc_ppm ~  CO+as.factor(year)+(1|pairnumber)+(1|Cultivar),subset=crop_type==8,weights=Znreplicates_lag_1,na.action=na.omit,data =  zinc_sum) 
summary(zinc7)   


               
#soy (7)               
zinc8 <- lmer( zinc_ppm ~ CO+as.factor(year)+(1|pairnumber)+(1|Cultivar),subset=crop_type==7,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc8)   



#barley (1)               
zinc9 <- lmer( zinc_ppm ~ CO+as.factor(year)+(1|pairnumber)+(1|Cultivar),subset=crop_type==1,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc9)   


#corn (2)               
zinc10 <- lmer( zinc_ppm ~ CO+as.factor(year)+(1|pairnumber)+(1|Cultivar),subset=crop_type==2,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc10)   


#peas (3)               
zinc9 <- lmer( zinc_ppm ~ CO+as.factor(year)+(1|pairnumber)+(1|Cultivar),subset=crop_type==3,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc11)   


#potato (4)               
zinc12 <- lmer( zinc_ppm ~ CO+as.factor(year)+(1|pairnumber)+(1|Cultivar),subset=crop_type==4,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc12)   


#rice (5)               
zinc13 <- lmer( zinc_ppm ~ CO+as.factor(year)+(1|pairnumber)+(1|Cultivar),subset=crop_type==5,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc13)   


#soghrum (6)               
zinc14 <- lmer( zinc_ppm ~ CO+as.factor(year)+(1|pairnumber)+(1|Cultivar),subset=crop_type==6,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc14)   











#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> iron




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> SUMMARY DATASET

#iron only CO
iron1 <- lme( iron_ppm ~ CO , na.action=na.omit,
   random = ~1  | pairnumber, 
  data =  zinc_sum) 
summary(iron1)


              
# All crops no wight
# iron3 <- lmer( iron_ppm ~  CO+as.factor(year)+(1|pairnumber)+(1+CO|crop),na.action=na.omit,data =  zinc_sum) 
# summary(iron3)   
# ran <- ranef(iron3)
#                
#               
# #with weights               
# iron4 <- lmer( iron_ppm ~  CO+as.factor(year)+(1|pairnumber)+(1+CO|crop),na.action=na.omit,weights=Znreplicates_lag_1,data =  zinc_sum) 
# summary(iron4)
# ran <- ranef(iron4)

#with weights and nitro cultiv irreg
iron5 <- lmer( iron_ppm ~  CO+as.factor(year)+iregg+nitro+(1|pairnumber)+(1+CO|crop)+(1|crop/Cultivar),weights=Znreplicates_lag_1,na.action=na.omit,data =  zinc_sum) 
summary(iron5)   
ran <- ranef(iron5)




















               