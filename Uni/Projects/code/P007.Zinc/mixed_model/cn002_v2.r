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

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> zinc

#with weights and nitro cultiv irreg
zinc5 <- lmer( zinc_ppm ~  CO+iregg+nitro+(1|pairnumber)+(1+CO|crop)+(1|crop/Cultivar),weights=Znreplicates_lag_1,na.action=na.omit,data =  zinc_sum) 
summary(zinc5)   
ran <- ranef(zinc5)


zinc5l <- lmer( log(zinc_ppm) ~  CO+iregg+nitro+(1|pairnumber)+(1+CO|crop)+(1|crop/Cultivar),weights=Znreplicates_lag_1,na.action=na.omit,data =  zinc_sum) 
summary(zinc5l)   
ranl <- ranef(zinc5l)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> separate crops with weights
table (zinc_sum$crop_type)               




     
zinc7 <- lmer( log(zinc_ppm) ~  CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==1,weights=Znreplicates_lag_1,na.action=na.omit,data =  zinc_sum) 
summary(zinc7)   
               
             
zinc8 <- lmer( log(zinc_ppm) ~ CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==2,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc8)   



            
zinc9 <- lmer( log(zinc_ppm) ~ CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==3,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc9)   


           
zinc10 <- lmer( log(zinc_ppm) ~ CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==4,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc10)   


            
zinc9 <- lmer( log(zinc_ppm) ~ CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==5,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc9)   


          
zinc12 <- lmer( log(zinc_ppm) ~ CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==6,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc12)   


             
zinc13 <- lmer( log(zinc_ppm) ~ CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==7,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc13)   
       
zinc14 <- lmer( log(zinc_ppm) ~ CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==8,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc14)   




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> IRON

#with weights and nitro cultiv irreg
zinc5 <- lmer( iron_ppm ~  CO+iregg+nitro+(1|pairnumber)+(1+CO|crop)+(1|crop/Cultivar),weights=Znreplicates_lag_1,na.action=na.omit,data =  zinc_sum) 
summary(zinc5)   
ran <- ranef(zinc5)


zinc5l <- lmer( log(iron_ppm) ~  CO+iregg+nitro+(1|pairnumber)+(1+CO|crop)+(1|crop/Cultivar),weights=Znreplicates_lag_1,na.action=na.omit,data =  zinc_sum) 
summary(zinc5l)   
ranl <- ranef(zinc5l)




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> separate crops with weights
table (zinc_sum$crop_type)               




     
zinc7 <- lmer( log(iron_ppm) ~  CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==1,weights=Znreplicates_lag_1,na.action=na.omit,data =  zinc_sum) 
summary(zinc7)   
               
             
zinc8 <- lmer( log(iron_ppm) ~ CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==2,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc8)   



            
zinc9 <- lmer( log(iron_ppm) ~ CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==3,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc9)   


           
zinc10 <- lmer( log(iron_ppm) ~ CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==4,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc10)   


            
zinc9 <- lmer( log(iron_ppm) ~ CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==5,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc9)   


          
zinc12 <- lmer( log(iron_ppm) ~ CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==6,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc12)   


             
zinc13 <- lmer( log(iron_ppm) ~ CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==7,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc13)   
       
zinc14 <- lmer( log(iron_ppm) ~ CO+(1|pairnumber)+(1|Cultivar),subset=crop_type==8,weights=Znreplicates_lag_1,na.action=na.omit,data = zinc_sum) 
summary(zinc14)   









