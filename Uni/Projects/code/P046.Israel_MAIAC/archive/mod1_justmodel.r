m1.formula <- as.formula(PM25~ aod+(1+aod|day))
m1_sc <- lmer(m1.formula,data=m1.all,weights=normwt)
m1.all[,pred.m1 := NULL]
m1.all$pred.m1 <- predict(m1_sc)
print(summary(lm(PM25~pred.m1,data=m1.all))$r.squared)


m1.formula <- as.formula(PM25~ aod
                        +tempa.s+WSa.s
                        +pbldag
                        +RHa.s+O3a.s+Raina.s+NOa.s 
                        +elev.s+tden.s
                        +pden.s
                        +ndvi.s 
                        +dist2rail.s +dist2water.s +dist2A1.s+Dist2road.s
                        +p_os.s+p_dev.s+p_dos.s+p_farm.s+p_for.s+p_ind.s  
                        #+as.factor(metreg)+as.factor(reg_num)
                        +as.factor(season)
                        +as.factor(season)*aod
                         #                        +closest5kmean
                         #      +aodpre #+aodpost
                         #+meanPM10
                         # + aod:Dust 
                        +aod*lat_aod.x
                        +Dust*lat_aod.x
                        +pbldag*lat_aod.x
                        +(1+aod|day/reg_num)) 

m1_sc <- lmer(m1.formula,data=m1.all,weights=normwt)
m1.all[,pred.m1 := NULL]
m1.all$pred.m1 <- predict(m1_sc)
print(summary(lm(PM25~pred.m1,data=m1.all))$r.squared)
#CV 0.7350478







#---------------->>>> CV
#s1
splits_s1 <- splitdf(m1.all)
test_s1 <- splits_s1$testset
train_s1 <- splits_s1$trainset
out_train_s1 <- lmer(m1.formula,data =  train_s1,weights=normwt)
test_s1$pred.m1.cv <- predict(object=out_train_s1 ,newdata=test_s1,allow.new.levels=TRUE,re.form=NULL )
test_s1$iter<-"s1"
#s2
splits_s2 <- splitdf(m1.all)
test_s2 <- splits_s2$testset
train_s2 <- splits_s2$trainset
oasut_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
test_s2$pred.m1.cv <- predict(object=out_train_s2 ,newdata=test_s2,allow.new.levels=TRUE,re.form=NULL )
test_s2$iter<-"s2"
#s3
splits_s3 <- splitdf(m1.all)
test_s3 <- splits_s3$testset
train_s3 <- splits_s3$trainset
out_train_s3 <- lmer(m1.formula,data =  train_s3,weights=normwt)
test_s3$pred.m1.cv <- predict(object=out_train_s3 ,newdata=test_s3,allow.new.levels=TRUE,re.form=NULL )
test_s3$iter<-"s3"
#s4
splits_s4 <- splitdf(m1.all)
test_s4 <- splits_s4$testset
train_s4 <- splits_s4$trainset
out_train_s4 <- lmer(m1.formula,data =  train_s4,weights=normwt)
test_s4$pred.m1.cv <- predict(object=out_train_s4 ,newdata=test_s4,allow.new.levels=TRUE,re.form=NULL )
test_s4$iter<-"s4"
#s5
splits_s5 <- splitdf(m1.all)
test_s5 <- splits_s5$testset
train_s5 <- splits_s5$trainset
out_train_s5 <- lmer(m1.formula,data =  train_s5,weights=normwt)
test_s5$pred.m1.cv <- predict(object=out_train_s5 ,newdata=test_s5,allow.new.levels=TRUE,re.form=NULL )
test_s5$iter<-"s5"

#BIND 1 dataset
m1.all.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5))
m1.fit.all.cv<-lm(PM25~pred.m1.cv,data=m1.all.cv)
print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$r.squared)

















