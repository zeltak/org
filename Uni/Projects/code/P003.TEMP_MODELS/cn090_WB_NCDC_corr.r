cor<-read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/ncdc_wb_corr/cordata.dbf") 

names(cor)

cor(cor$TEMP,cor$temp_mean)^2


#TEMP is for ncdc temp_mean for wb and wu
library(ggplot2)

trend<-qplot(TEMP,temp_mean,data=cor, xlab="Weather bug and weather underground", ylab="NCDC", )

trend<-trend+stat_smooth(method="lm")
trend



ggsave("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.3.TEMP_MODELS/3.1.1.4.Work/3.Analysis/ncdc_wb_corr/cordata.svg")