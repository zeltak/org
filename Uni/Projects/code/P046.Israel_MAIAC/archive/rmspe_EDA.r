#for data
m1.formula <- as.formula(PM10~ aod
                        +(1+aod|day/reg_num))
sim<-m1.all$pred.m1
obs<-m1.all$PM10
rmse(sim, obs)
library(Metrics)
Metrics::rmse(m1.all$pred.m1, m1.all$PM10)

PM10~pred.m1

Metrics::rmse(m1.all$pred.m1,m1.all$PM10)
m1.all<-as.data.frame(m1.all)
sqrt(mean((m1.all$pred.m1-m1.all$PM10)^2,na.rm=TRUE))
sqrt(sum((m1.all$pred.m1-m1.all$PM10)^2,na.rm = TRUE )/ nrow(m1.all) )
RMSE <- sqrt(mean((y-y_pred)^2))


tst<-m1.all %>%
group_by(stn) %>%
summarise(barpm = mean(PM10, na.rm=TRUE), barpred = mean(pred.m1, na.rm=TRUE)) %>%
mutate(rmspe = sqrt(mean((barpm-barpred)^2)))

sqrt(mean((tst$barpm-tst$barpred)^2))



################# clean BAD STN PM10 and check if improved model?
rmse.p10 <- ddply(m1.all, c("stn"), 
      function(x) {
        sqrt(mean((x$PM10-x$pred.m1)^2))
                      })
rmse.p10<-as.data.table(rmse.p10)
setnames(rmse.p10,"V1","RMSPE")
setkey(rmse.p10,RMSPE)
rmse.p10


rmse.p10 <- ddply(m1.all, c("c"), 
      function(x) {
        sqrt(mean((x$PM10-x$pred.m1)^2))
                      })
rmse.p10<-as.data.table(rmse.p10)
setnames(rmse.p10,"V1","RMSPE")
rmse.p10



out<-m1.all %>%
  group_by(stn) %>%
  summarise (n = n()) 
setkey(out,stn)
head(out,n=50)
