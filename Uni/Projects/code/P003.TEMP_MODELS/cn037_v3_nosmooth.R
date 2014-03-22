library (mgcv)
library(nlme)
library(foreign)
library(reshape)

load("f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/mod3_workspace.RData")

summary(GAM_T2003)
#omit missing predictions
GAM_T2003 = na.omit(GAM_T2003)



library(nlme)

smooth_T2003_yearly = lme(predicted ~ mtmp  ,
                          random = list(guid= ~1 + mtmp ),
                          data= GAM_T2003 )


####import all xy points across new-england
grid_2003 = read.csv("f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3/T2003_grid_reg.csv", header=T) 


grid_pred$prens <- predict(smooth_T2003_yearly,grid_2003)
summary(grid_2003)







#export grid and predictions

library(foreign) 
write.dbf(grid_2003,"f:/Uni/Projects/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/mod3_pred/grid_2003_nosmooth.dbf") 



