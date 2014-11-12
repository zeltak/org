test<-fread("/home/zeltak/ZH_tmp/DBPM25.csv")
test<-na.omit(test)


#per year
m1.formula<-PM25~DB550
modelList <- dlply(test, "yr", function(x) lm(m1.formula, data=x))
pm25.year<-t(as.data.table(lapply(modelList, function(x) summary(x)$r.squared)))
pm25.year

#per station
modelList <- dlply(test[yr==2013], c("stn"), function(x) lm(m1.formula, data=x))
aquaSTN<-t(as.data.table(lapply(modelList, function(x) summary(x)$r.squared)))
aquaSTN




rawdf <- ddply(test, c("yr","stn"), 
      function(x) {
        mod1 <- lm(PM25 ~ DB550, data=x)
        data.frame(R2 = round(summary(mod1)$r.squared, 5), 
                   nsamps = length(summary(mod1)$resid))
})
rawdf
rawdf<-as.data.table(rawdf)
bad <- rawdf[nsamps < 10]
bad[,badid := paste(stn,yr,sep="-")]
#################BAD STN
test[,badid := paste(stn,yr,sep="-")]
####Take out bad stations
test <- test[!(test$badid %in% bad$badid), ] 


#per year
m1.formula<-PM25~DB550
modelList <- dlply(test, "yr", function(x) lm(m1.formula, data=x))
pm25.year.good<-t(as.data.table(lapply(modelList, function(x) summary(x)$r.squared)))
pm25.year.good



