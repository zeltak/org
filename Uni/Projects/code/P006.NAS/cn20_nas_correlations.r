library(ggplot2)
library(foreign)
library(car)
library(stats) 
library(mgcv)
library(splines)
library(MASS)
library(nlme)


mb1 <-  read.csv("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.6.NAS/3.1.6.4.Work/3.Analysis/MB_analysis/mb_expo.csv", header=T) 
names(mb1)

y<-lm(pmnewmayear~cigaryrs,data=mb1)
summary(y)


y1<-lm(pmnewmayear~mets,data=mb1)
summary(y1)

y1<-lm(pmnewmayear~omega,data=mb1)
summary(y1)


y1<-lm(pmnewmayear~darkfish,data=mb1)
summary(y1)


y1<-lm(pmnewmayear~race,data=mb1)
summary(y1)

y1<-lm(pmnewmayear~afat,data=mb1)
summary(y1)

y1<-lm(pmnewmayear~calfat,data=mb1)
summary(y1)


y1<-lm(pmnewmayear~vitc,data=mb1)
summary(y1)


y1<-lm(pmnewmayear~neduc,data=mb1)
summary(y1)

y1<-lm(pmnewmayear~educmax,data=mb1)
summary(y1)

y1<-lm(pmnewmayear~pctbelowpo,data=mb1)
summary(y1)





# animal fat and vitc were correlated 

pack years ,diet (omega, dark fish) activity indicators

