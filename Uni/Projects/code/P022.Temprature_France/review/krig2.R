m1.all.cv<- read_rds("/media/NAS/Uni/Projects/P022_Temprature_France/3.work/cn099.R/Xmod1cv.ALL.rds")


m1.formula <- as.formula(tm ~ elev_m+pcturb+(1|date)) 
s1 <- lmer(m1.formula,data =  m1.all.cv)
m1.all.cv$pred.m1 <- predict(object=s1 ,allow.new.levels=TRUE,re.form=NULL )
#overall
summary(lm(tm~pred.m1,data=m1.all.cv))




### krig analysis
library(mgcv)
# met<- as.data.table(read.dbf("/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/2.Gather_data/FN003_WUNCDC yearly/met2000.dbf") )
# met<-met[,c(1:5),with=FALSE]
# met<-na.omit(met)
# met$month <- as.numeric(format(met$Date, "%m"))

#run "kriging" analysis
#need to convert dat to numeric
m1.all.cv$dd <- as.numeric(m1.all.cv$day)
fi1_gam <- gam( tm ~ s(Longitude,Latitude+elev_m+pcturb,by=dd), data=m1.all.cv)
#fi1_gam <- gam( tm ~ s(Longitude,Latitude)+elev_m+pcturb, data=m1.all.cv)
summary(fi1_gam)
m1.all.cv$pred.gam <- predict(fi1_gam)

summary(lm(tm~pred.gam, data=m1.all.cv))

#calc long term means
#spatial
spatialall.cv<-m1.all.cv %>%
    group_by(num_insee) %>%
    summarise(barpm = mean(tm, na.rm=TRUE), barpred = mean(pred.gam, na.rm=TRUE)) 
m1.fit.all.cv.s <- lm(barpm ~ barpred, data=spatialall.cv)
print(summary(lm(barpm ~ barpred, data=spatialall.cv))$r.squared)
print(rmse(residuals(m1.fit.all.cv.s)))


summary(lm(tm~pred,data=m1.all.cv))

add5 <- met[Date == "2000-01-01"]