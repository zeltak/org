###############
#LIBS
###############
library(lme4)
library(reshape)
library(foreign) 
library(ggplot2)
library(plyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(mgcv)
library(gdata)


######## import pollution sets
pm2005<-readRDS("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod3best_2005.rds")
pm2005<-pm2005[,c(1,2,9),with=FALSE]


#subset to buffer5
buf5<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/4.Results/1km buffer/buffer1km_p5.csv")
buf5<-buf5[,c(11),with=FALSE]
pm2005.buf5 <- pm2005 [pm2005 $guid %in% buf5[,unique(guid)], ] 
summary(pm2005.buf5$bestpred)
pm2005.buf5$bid <-5

#subset to buffer4
buf4<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/4.Results/1km buffer/buffer_p4.csv")
buf4<-buf4[,c(11),with=FALSE]
pm2005.buf4 <- pm2005 [pm2005 $guid %in% buf4[,unique(guid)], ] 
summary(pm2005.buf4$bestpred)
pm2005.buf4$bid <-4



#subset to buffer3
buf3<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/4.Results/1km buffer/buffer_p3.csv")
buf3<-buf3[,c(11),with=FALSE]
pm2005.buf3 <- pm2005 [pm2005 $guid %in% buf3[,unique(guid)], ] 
summary(pm2005.buf3$bestpred)
pm2005.buf3$bid <-3


#subset to buffer2
buf2<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/4.Results/1km buffer/buffer_p2.csv")
buf2<-buf2[,c(11),with=FALSE]
pm2005.buf2 <- pm2005 [pm2005 $guid %in% buf2[,unique(guid)], ] 
summary(pm2005.buf2$bestpred)
pm2005.buf2$bid <-2

#subset to buffer 1
buf1<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/4.Results/1km buffer/buffer5km_p1.csv")
buf1<-buf1[,c(11),with=FALSE]
pm2005.buf1 <- pm2005 [pm2005 $guid %in% buf1[,unique(guid)], ] 
summary(pm2005.buf1$bestpred)
pm2005.buf1$bid <-1



fin<-rbind(pm2005.buf1,pm2005.buf2,pm2005.buf3,pm2005.buf4,pm2005.buf5)
fin$bid <-as.factor(fin$bid)


#all points and dist
#subset to buffer 1
ap<-fread("/media/NAS/Uni/Projects/P031_MIAC_PM/4.Results/1km buffer/buffer_points_alldist.csv")
pm2005.ap <- pm2005 [pm2005 $guid %in% ap[,unique(guid)], ] 
summary(pm2005.buf1$bestpred)

setkey(pm2005.ap, guid)
setkey(ap, guid)
find <- merge(pm2005.ap, ap[,list(guid,Distance_1)], all.x = T)
#+end_src




#map the predictions
#aggregate by guid
m3d_agg <- (find[, list(bestpred =mean(bestpred, na.rm = TRUE), 
                        dist = Distance_1[1], #use the first long and lat (by guid)
                        dis2 = Distance_1[1]),by = guid])


qplot(bid, bestpred, data = fin, geom="boxplot")+geom_boxplot(outlier.shape = NA)
p + geom_boxplot(outlier.colour = "green", outlier.size = 3)

#ignore outliers
p2 <- ggplot(fin, aes(bid, bestpred)) + geom_boxplot(outlier.shape = NA) 
p2<-p2+ scale_y_continuous(limits=c(10.3, 10.6)) 
p2  # no outliers plotted, range shifted

#fill with colors
p + geom_boxplot(aes(fill = factor(bid)))


ggplot(m3d_agg, aes(x=dist, y=bestpred)) + geom_point()

summary(lm(dist~bestpred,data=m3d_agg))


