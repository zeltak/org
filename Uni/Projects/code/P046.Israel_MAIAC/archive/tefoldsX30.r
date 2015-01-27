
#---------------->>>> CV

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
out_train_s2 <- lmer(m1.formula,data =  train_s2,weights=normwt)
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
#s6
splits_s6 <- splitdf(m1.all)
test_s6 <- splits_s6$testset
train_s6 <- splits_s6$trainset
out_train_s6 <- lmer(m1.formula,data =  train_s6,weights=normwt)
test_s6$pred.m1.cv <- predict(object=out_train_s6 ,newdata=test_s6,allow.new.levels=TRUE,re.form=NULL )
test_s6$iter<-"s6"
#s7
splits_s7 <- splitdf(m1.all)
test_s7 <- splits_s7$testset
train_s7 <- splits_s7$trainset
out_train_s7 <- lmer(m1.formula,data =  train_s7,weights=normwt)
test_s7$pred.m1.cv <- predict(object=out_train_s7 ,newdata=test_s7,allow.new.levels=TRUE,re.form=NULL )
test_s7$iter<-"s7"
#s8
splits_s8 <- splitdf(m1.all)
test_s8 <- splits_s8$testset
train_s8 <- splits_s8$trainset
out_train_s8 <- lmer(m1.formula,data =  train_s8,weights=normwt)
test_s8$pred.m1.cv <- predict(object=out_train_s8 ,newdata=test_s8,allow.new.levels=TRUE,re.form=NULL )
test_s8$iter<-"s8"
#s9
splits_s9 <- splitdf(m1.all)
test_s9 <- splits_s9$testset
train_s9 <- splits_s9$trainset
out_train_s9 <- lmer(m1.formula,data =  train_s9,weights=normwt)
test_s9$pred.m1.cv <- predict(object=out_train_s9 ,newdata=test_s9,allow.new.levels=TRUE,re.form=NULL )
test_s9$iter<-"s9"
#s10
splits_s10 <- splitdf(m1.all)
test_s10 <- splits_s10$testset
train_s10 <- splits_s10$trainset
out_train_s10 <- lmer(m1.formula,data =  train_s10,weights=normwt)
test_s10$pred.m1.cv <- predict(object=out_train_s10 ,newdata=test_s10,allow.new.levels=TRUE,re.form=NULL )
test_s10$iter<-"s10"

#s1
splits_s11 <- splitdf(m1.all)
test_s11 <- splits_s11$testset
train_s11 <- splits_s11$trainset
out_train_s11 <- lmer(m1.formula,data =  train_s11,weights=normwt)
test_s11$pred.m1.cv <- predict(object=out_train_s11 ,newdata=test_s11,allow.new.levels=TRUE,re.form=NULL )
test_s11$iter<-"s11"
#s2
splits_s12 <- splitdf(m1.all)
test_s12 <- splits_s12$testset
train_s12 <- splits_s12$trainset
out_train_s12 <- lmer(m1.formula,data =  train_s12,weights=normwt)
test_s12$pred.m1.cv <- predict(object=out_train_s12 ,newdata=test_s12,allow.new.levels=TRUE,re.form=NULL )
test_s12$iter<-"s12"
#s3
splits_s13 <- splitdf(m1.all)
test_s13 <- splits_s13$testset
train_s13 <- splits_s13$trainset
out_train_s13 <- lmer(m1.formula,data =  train_s13,weights=normwt)
test_s13$pred.m1.cv <- predict(object=out_train_s13 ,newdata=test_s13,allow.new.levels=TRUE,re.form=NULL )
test_s13$iter<-"s13"
#s4
splits_s14 <- splitdf(m1.all)
test_s14 <- splits_s14$testset
train_s14 <- splits_s14$trainset
out_train_s14 <- lmer(m1.formula,data =  train_s14,weights=normwt)
test_s14$pred.m1.cv <- predict(object=out_train_s14 ,newdata=test_s14,allow.new.levels=TRUE,re.form=NULL )
test_s14$iter<-"s14"
#s5
splits_s15 <- splitdf(m1.all)
test_s15 <- splits_s15$testset
train_s15 <- splits_s15$trainset
out_train_s15 <- lmer(m1.formula,data =  train_s15,weights=normwt)
test_s15$pred.m1.cv <- predict(object=out_train_s15 ,newdata=test_s15,allow.new.levels=TRUE,re.form=NULL )
test_s15$iter<-"s15"
#s6
splits_s16 <- splitdf(m1.all)
test_s16 <- splits_s16$testset
train_s16 <- splits_s16$trainset
out_train_s16 <- lmer(m1.formula,data =  train_s16,weights=normwt)
test_s16$pred.m1.cv <- predict(object=out_train_s16 ,newdata=test_s16,allow.new.levels=TRUE,re.form=NULL )
test_s16$iter<-"s16"
#s7
splits_s17 <- splitdf(m1.all)
test_s17 <- splits_s17$testset
train_s17 <- splits_s17$trainset
out_train_s17 <- lmer(m1.formula,data =  train_s17,weights=normwt)
test_s17$pred.m1.cv <- predict(object=out_train_s17 ,newdata=test_s17,allow.new.levels=TRUE,re.form=NULL )
test_s17$iter<-"s17"
#s8
splits_s18 <- splitdf(m1.all)
test_s18 <- splits_s18$testset
train_s18 <- splits_s18$trainset
out_train_s18 <- lmer(m1.formula,data =  train_s18,weights=normwt)
test_s18$pred.m1.cv <- predict(object=out_train_s18 ,newdata=test_s18,allow.new.levels=TRUE,re.form=NULL )
test_s18$iter<-"s18"
#s9
splits_s19 <- splitdf(m1.all)
test_s19 <- splits_s19$testset
train_s19 <- splits_s19$trainset
out_train_s19 <- lmer(m1.formula,data =  train_s19,weights=normwt)
test_s19$pred.m1.cv <- predict(object=out_train_s19 ,newdata=test_s19,allow.new.levels=TRUE,re.form=NULL )
test_s19$iter<-"s19"
#s10
splits_s20 <- splitdf(m1.all)
test_s20 <- splits_s20$testset
train_s20 <- splits_s20$trainset
out_train_s20 <- lmer(m1.formula,data =  train_s20,weights=normwt)
test_s20$pred.m1.cv <- predict(object=out_train_s20 ,newdata=test_s20,allow.new.levels=TRUE,re.form=NULL )
test_s20$iter<-"s20"



#BIND 1 dataset
m1.all.cv<- data.table(rbind(test_s1,test_s2,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9,test_s11,test_s13,test_s14,test_s15,test_s18,test_s19))


#BIND 1 dataset
#m1.all.cv<- data.table(rbind(test_s1,test_s3,test_s4,test_s5,test_s6,test_s7,test_s8,test_s9, test_s10,test_s11,test_s12,test_s13,test_s14,test_s15,test_s16,test_s17,test_s18,test_s19, test_s20))


#table updates
m1.fit.all.cv<-lm(PM25~pred.m1.cv,data=m1.all.cv)
res[res$type=="PM25", 'm1cv.R2'] <- print(summary(lm(PM25~pred.m1.cv,data=m1.all.cv))$r.squared)
