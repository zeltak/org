library(nlme) 
library(foreign) 
library(nlme) 
library (splines)
library (MASS)
library(ggplot2)
library(plyr)
library(Hmisc)
library (mgcv)
library (splines)

#simple model

res.mod1 <- lme(daymean ~ AOD+tden_1, random = ~1 + AOD| DATE,data =  mod1, na.action = na.omit) 
res.mod1 <- lm(daymean ~ AOD+tden_1,data =  mod1, na.action = na.omit) 
#---
mod1$predicted <- predict(object=res.mod1)
summary(lm(daymean ~ predicted, data=mod1))
plot(mod1$predicted,mod1$daymean)


#yearly


#create CV table
mod1table <- data.frame(type=character(9), r2000=numeric(9),r2001=numeric(9),r2002=numeric(9),r2003=numeric(9),r2004=numeric(9),r2005=numeric(9),r2006=numeric(9),r2007=numeric(9),r2008=numeric(9),r2009=numeric(9),r2010=numeric(9),r2011=numeric(9),mean=numeric(9))


names(mod1)
str(mod1$year)
mod1_2003 <-subset(mod1, year == 2003)
mod1_2004 <-subset(mod1, year == 2004)
mod1_2005 <-subset(mod1, year == 2005)
mod1_2006 <-subset(mod1, year == 2006)
mod1_2007 <-subset(mod1, year == 2007)
mod1_2008 <-subset(mod1, year == 2008)
mod1_2009 <-subset(mod1, year == 2009)
mod1_2010 <-subset(mod1, year == 2010)
mod1_2011 <-subset(mod1, year == 2011)

#2003
res.mod1 <- lme(daymean ~ AOD+tden_1, random = ~1 + AOD| DATE,data =  mod1_2003, na.action = na.omit) 
#---
mod1_2003$predicted <- predict(object=res.mod1)
mod1d_reg<-(lm(daymean ~ predicted, data=mod1_2003))
plot(mod1$predicted,mod1$daymean)
mod1table$r2003[1] <-summary(mod1d_reg)$r.squared

# one model per day

#2004
res.mod1 <- lme(daymean ~ AOD, random = ~1 + AOD| DATE,data =  mod1_2004, na.action = na.omit) 
#---
mod1_2004$predicted <- predict(object=res.mod1)
mod1d_reg<-(lm(daymean ~ predicted, data=mod1_2004))
plot(mod1$predicted,mod1$daymean)
mod1table$r2004[1] <-summary(mod1d_reg)$r.squared

#2005
res.mod1 <- lme(daymean ~ AOD+tden_1, random = ~1 + AOD| DATE,data =  mod1_2005, na.action = na.omit) 
#---
mod1_2005$predicted <- predict(object=res.mod1)
mod1d_reg<-(lm(daymean ~ predicted, data=mod1_2005))
plot(mod1$predicted,mod1$daymean)
mod1table$r2005[1] <-summary(mod1d_reg)$r.squared


#2006
res.mod1 <- lme(daymean ~ AOD+tden_1, random = ~1 + AOD| DATE,data =  mod1_2006, na.action = na.omit) 
#---
mod1_2006$predicted <- predict(object=res.mod1)
mod1d_reg<-(lm(daymean ~ predicted, data=mod1_2006))
plot(mod1$predicted,mod1$daymean)
mod1table$r2006[1] <-summary(mod1d_reg)$r.squared

#2007
res.mod1 <- lme(daymean ~ AOD+tden_1, random = ~1 + AOD| DATE,data =  mod1_2007, na.action = na.omit) 
#---
mod1_2007$predicted <- predict(object=res.mod1)
mod1d_reg<-(lm(daymean ~ predicted, data=mod1_2007))
plot(mod1$predicted,mod1$daymean)
mod1table$r2007[1] <-summary(mod1d_reg)$r.squared


#2008
res.mod1 <- lme(daymean ~ AOD+tden_1, random = ~1 + AOD| DATE,data =  mod1_2008, na.action = na.omit) 
#---
mod1_2008$predicted <- predict(object=res.mod1)
mod1d_reg<-(lm(daymean ~ predicted, data=mod1_2008))
plot(mod1$predicted,mod1$daymean)
mod1table$r2008[1] <-summary(mod1d_reg)$r.squared

#2009
res.mod1 <- lme(daymean ~ AOD+tden_1, random = ~1 + AOD| DATE,data =  mod1_2009, na.action = na.omit) 
#---
mod1_2009$predicted <- predict(object=res.mod1)
mod1d_reg<-(lm(daymean ~ predicted, data=mod1_2009))
plot(mod1$predicted,mod1$daymean)
mod1table$r2009[1] <-summary(mod1d_reg)$r.squared

#2010
res.mod1 <- lme(daymean ~ AOD+tden_1, random = ~1 + AOD| DATE,data =  mod1_2010, na.action = na.omit) 
#---
mod1_2010$predicted <- predict(object=res.mod1)
mod1d_reg<-(lm(daymean ~ predicted, data=mod1_2010))
plot(mod1$predicted,mod1$daymean)
mod1table$r2010[1] <-summary(mod1d_reg)$r.squared

#2011
res.mod1 <- lme(daymean ~ AOD+tden_1, random = ~1 + AOD| DATE,data =  mod1_2011, na.action = na.omit) 
#---
mod1_2011$predicted <- predict(object=res.mod1)
mod1d_reg<-(lm(daymean ~ predicted, data=mod1_2011))
plot(mod1$predicted,mod1$daymean)
mod1table$r2011[1] <-summary(mod1d_reg)$r.squared
