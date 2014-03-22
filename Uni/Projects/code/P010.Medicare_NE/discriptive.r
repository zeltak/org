library (MASS)
library (splines)
library(nlme) 
library(foreign) 
library(gmodels)

st1 <-  read.dbf("c:/Users/ekloog/Documents/Postdoc/~work/H.Hospital Admitance/1.Data/i.cases by type/stroke.dbf") 
st1 <-  read.dbf("c:/Users/ekloog/Documents/Postdoc/~work/H.Hospital Admitance/1.Data/i.cases by type/crv.dbf") 
st1 <-  read.dbf("c:/Users/ekloog/Documents/Postdoc/~work/H.Hospital Admitance/1.Data/i.cases by type/ihd.dbf") 
st1 <-  read.dbf("c:/Users/ekloog/Documents/Postdoc/~work/H.Hospital Admitance/1.Data/i.cases by type/diab.dbf") 


CrossTable(st1$SEX)
CrossTable(st1$RACE2)
summary(st1$AGE)

pm1 = read.csv("c:/Users/ekloog/Documents/Postdoc/~work/H.Hospital Admitance/1.Data/j.export to r for ts/ts_crv.csv", header=T) 
names(pm1)
pm2 = read.csv("c:/Users/ekloog/Documents/Postdoc/~work/H.Hospital Admitance/1.Data/j.export to r for ts/ts_stroke.csv", header=T) 



summary(pm1$deltapm)
summary(pm1$mpmguid)


summary(pm2$deltapm)
summary(pm2$mpmguid)


pmd <-  read.csv("c:/Users/ekloog/Documents/Postdoc/~work/H.Hospital Admitance/2.statistic analysis/d.discriptive/ts_00_06.csv", header=T) 

summary(pmd$mpmguid)
summary(pmd$deltapm)
summary(pmd$temp2d)


library(psych)

describe(pmd$deltapm)
describe(pmd$mpmguid)
describe(pmd$temp2d)

