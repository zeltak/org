library(nlme) 
library(foreign) 
library(nlme) 
library (splines)
library (MASS)
library(ggplot2)
library(plyr)
library(Hmisc)

raw1 <-  read.csv("f:/Uni/Projects/P031_MIAC_MEXICO/3.Work/2.Gather_data/FN009_mod1_files/mod1.csv", header=T)  
names(raw1)
summary(raw1)

hist(raw1$daymean)
hist(raw1$meanpbl)

# drop everything that is missing
mod1 <- raw1[!is.na(raw1$meanpbl+raw1$avewsp) & !is.na(raw1$rhmean), ]



# drop Jan 1 2005, Jan 1 2006
mod1$day <- as.Date(strptime(mod1$DATE, "%m/%d/%y"))
mod1$dayofyr <- as.numeric(format(mod1$day, "%j"))
mod1 <- mod1[mod1$dayofyr > 1, ]

model.formula <- as.formula(daymean ~ AOD+tden_1+elev_1+avewsp+meanampbl*elev_1+meanpmpbl*elev_1+tempmean+season)
res.mod1 <- lme(model.formula, random = ~1 + AOD| DATE,data =  mod1, na.action = na.omit) 

summary(res.mod1) 
mod1$predicted <- predict(object=res.mod1)
summary(lm(daymean ~ predicted, data=mod1))

plot(mod1$predicted,mod1$daymean)

mod1$resid<-residuals(res.mod1)
submod1 <- subset(mod1, resid < 2 * sd(mod1$resid) & resid > -2 * sd(mod1$resid))
res.mod1 <- lme(model.formula,
                 random = ~1 + AOD| DATE,data =  submod1, na.action = na.omit) 


summary(res.mod1) 
submod1$predicted <- predict(object=res.mod1)
summary(lm(daymean ~ predicted, data=submod1))
plot(submod1$predicted,submod1$daymean)

# identify outliers
ggplot(mod1[,], aes(predicted, daymean)) + geom_abline(linetype = "dotted") + 
  geom_point(alpha = 0.2) + 
  geom_smooth() +
  coord_equal() + 
  facet_wrap(~mon) + theme_bw(16)

ggplot(mod1[mod1$dayofyr <= 3 | mod1$dayofyr >= 364,], aes(dayofyr, daymean, color = mon)) + geom_point() + 
  facet_wrap(~yr)

describe(mod1[mod1$daymean > 100, ])

# run one model per day - summarise betas
model.formula <- as.formula(daymean ~ AOD+tden_1+elev_1+avewsp+meanampbl*elev_1+meanpmpbl*elev_1+tempmean)
model.formula <- as.formula(daymean ~ AOD)

moddays <- sort(unique(mod1$day))
moddayslm <- data.frame(beta = rep(NA, length(moddays), nrows = rep(NA, length(moddays)), 
                                   ith = rep(NA, length(moddays))))
for(i in 1:moddays){
  subdat <- mod1[mod1$day == moddays[i], ]
    day.mod1 <- try(lm(model.formula, data =  subdat, na.action = na.omit) )
    moddayslm$ith[i] <- i
    moddayslm$beta[i] <- coef(day.mod1)[2]
    moddayslm$nrows[i] <- nrow(subdat)
}
describe(moddayslm)
which.max(moddayslm$beta)
moddayslm[which.max(moddayslm$beta), ]


splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}




splits_s1 <- splitdf(submod1)
mod1d_10_s1 <- splits_s1$trainset
mod1d_90_s1 <- splits_s1$testset
out_90_s1 = lme(model.formula,
                random = ~1 + AOD| DATE,data =mod1d_90_s1  , na.action = na.omit) 




mod1d_10_s1$predicted <- predict(object=out_90_s1,newdata=mod1d_10_s1 )

mod1d_all <- rbind(mod1d_10_s1,mod1d_10_s2,mod1d_10_s3,mod1d_10_s4,mod1d_10_s5,mod1d_10_s6,mod1d_10_s7,mod1d_10_s8,mod1d_10_s9, mod1d_10_s10)


