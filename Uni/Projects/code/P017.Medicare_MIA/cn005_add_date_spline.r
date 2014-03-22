
library (MASS)
library (splines)
library(nlme) 

###########################################################################
#                                allmort                                 #
###########################################################################
setwd("/n/home12/ekloog/work/")

sink("rout.txt",append=TRUE,split=TRUE)


ts0004lag = read.csv("short_0006.csv", header=T) 

summary(ts0004lag)



###########

##only acute

test<- data.frame(ns(ts0004lag$date,df=35))
test2<-cbind(ts0004lag,test)

write.csv(OBJECT NAME, "/n/home12/ekloog/work/spline.csv") 
 
