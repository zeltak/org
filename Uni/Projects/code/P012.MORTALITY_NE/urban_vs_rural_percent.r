library(foreign)


test <-  read.dbf("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.12.MORTALITY_NE/3.1.10.4.Work/2.Gather_data/FN003_LU/lu_mointorprox.dbf") 
summary(test)

sum(test$pop00)#to get total poulation


library(doBy)
summaryBy(pop00~mon20, data=test, FUN=sum) #to get the total population of each area

#then manually calculate the percent in each area IE:

popurban<-(215646.4/sum(test$pop00))*100
popurban
