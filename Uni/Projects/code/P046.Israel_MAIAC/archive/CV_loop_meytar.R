x<-pm25.m1[!is.na(Temp) & !is.na(WS) & !is.na(NO2)]

n=dim(x)[1]
kk=floor(n*0.9) #create train and test datasets.
Table=data.frame(type=character(10), it1=numeric(10),
               it2=numeric(10),it3=numeric(10),it4=numeric(10),it5=numeric(10),
               it6=numeric(10),it7=numeric(10),
               it8=numeric(10),it9=numeric(10),
               it10=numeric(10),mean=numeric(10),median=numeric(10))

#name columns

Table$type<- c("CV_R2","CV_int","CV_int_SE",
             "CV_Slope","CV_Slope SE","CV_RMSPE",
             "CV_RMSE","CV_NRMSE","NA","NA")

for (II in 1:10) {
  ran_row=sample(1:n,kk ) #random sample of 90% of the data (NAN's excluded)
  train=x[ran_row,]
  test=(x[-ran_row,])
  
  m1.formula <- as.formula(PM25~aod+Dust+elev+tden+dist2rail+tden+dist2water+ndvi+season+MeanPbl+p_os+p_dev+p_dos+p_farm+p_for+p_ind+(1+aod|day/reg_num))
  MM1=lmer(m1.formula,data=train)
  #summary(MM)
  
  test$pred=predict(MM1,test,allow.new.levels=TRUE,re.form=NULL)
  #Mpred2=predict(MM2,test,allow.new.levels=TRUE)
  r1=lm(test$PM25~test$pred)
  Table[1,II] <-summary(r1)$r.squared #R2
  Table[2,II] <-summary(r1)$coef[1,1] #intercept
  Table[3,II] <-summary(r1)$coef[1,2] #intercept SE
  Table[4,II] <-summary(r1)$coef[2,1] #Slope
  Table[5,II] <-summary(r1)$coef[2,2] #Slope SE
  Table[6,II]<- sqrt(mean(r1$residual^2)) #rmspe
  Table[7,II]<-sqrt(sum((test$PM25-test$pred)^2)/length(test$pred)) #RMSE 
}
X=rowMeans(Table[1:10,2:11])
Table[,12]=X
Y=rowMedians(as.matrix(Table[1:10,2:11]))
Table[,13]=Y
View(Table)