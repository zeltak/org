library (MASS)
library (splines)
library(nlme) 
library(psych)
library(reshape)
library(car)

setwd("c:/Users/ekloog/Documents/$Doc/3.PostDoc/3.1.Projetcs/3.1.12.MORTALITY_NE/3.1.10.4.Work/3.Analysis/AN001_R_files_bycase")

sink("rout.txt",append=TRUE,split=TRUE)


ts0004lag = read.csv("allmort.csv", header=T) 

ts0004lag<-na.omit(ts0004lag)

# /*home death=0  /////// outside home=1*/

