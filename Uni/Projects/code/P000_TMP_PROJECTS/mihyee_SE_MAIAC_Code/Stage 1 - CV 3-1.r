#remove.packages('lme4'); install.packages('c:/test/lme4_1.0-6.tar.gz', repos=NULL, type='source')
#http://cran.r-project.org/src/contrib/Archive/lme4/
#If error, install R developer tool (Rtools31.exe default installation at http://cran.r-project.org/bin/windows/Rtools/)

#After lme4_1.1-5, produces error like random coeff >= obs. :options(lmerControl=list(check.nobs.vs.rankZ = "ignore"))
#https://github.com/lme4/lme4/issues/175

library(data.table); library(plyr); library(lme4); library(mgcv)

#Making a grand report table
mod1table <- matrix(nrow=45, ncol=21); mod1table <- data.frame(mod1table)
colnames(mod1table) <- c('Year', 'Group', 'OA_R2', 'CV_R2', 'CV_int', 'CV_int_se', 'CV_slope', 'CV_slope_se', 'RMSPE', 'spatial', 'temporal', 'RMSPE_spatial', 'LPM_CV_R2', 'LPM_CV_int', 'LPM_CV_int_se', 'LPM_CV_slope', 'LPM_CV_slope_se', 'LPM_RMSPE', 'LPM_spatial', 'LPM_temporal', 'LPM_RMSPE_spatial')
mod1table$Year <- rep(2003:2011, each=5); mod1table$Group <- rep(1:5, 9)

lu <- read.csv('C:/Data/Thesis 2/Data/Local PM/pm_stations_lpmvariables_2_7_14.csv')
lu$dist_pemis[is.na(lu$dist_pemis)] <- 15; lu$dist_A1[is.na(lu$dist_A1)] <- 50

splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/10))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset, testset=testset)
}

m1.formula <- as.formula(PM25 ~ aod + TEMP + DEWP + SLP + WDSP + VISIB + ah_gm3 + NDVI + elev_m + pbl  
                         + pcturb_1km + Emsn_Pt + PM10_Pt + NOX + Mjrrdden_1km + Pop_10km + nei05nonpntcntypm25    
                         + Canopy01 + Dist_A1 + Dist_Air + Dist_Port + Dist_Rail + Dist_Rd + Dist_Bldg + aod*pbl 
                         + (1 + aod + TEMP|Date/Region2))
ctrl <- lmerControl(optCtrl=list(maxfun=50000))

lpm.formula <- as.formula(Resid ~ s(tden_hpms11, pbl) + s(tden_hpms11, WDSP) + s(pcturban_90, fx=FALSE,k=4, bs='cr') 
                          + s(elev_m, fx=FALSE, k=4, bs='cr') + s(dist_pemis, fx=FALSE, k=4, bs='cr') 
                          + s(dist_A1, fx=FALSE,k=4, bs='cr'))


options(warn=1) #Produce warnings right away where it occurs
for (i in 2003:2011)  {
  
  for (j in 1:5)  {
    
    print (paste(i, j))
    
    m1 <- read.csv(paste('C:/Data/Thesis 2/Results/Stage 1/Pred/Pred1 CSV/stage1_', i, '_', j, '.csv', sep=""), colClasses=c("SiteCode"="character")) #To keep leading zeros in sitecode
    
    out.m1 <- lmer(m1.formula, weights=IPW_N, data=m1, control=ctrl)
    m1$predicted <- predict(out.m1)
    mod1d_reg <- lm(m1$PM25 ~ m1$predicted)
    
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'OA_R2'] <- summary(mod1d_reg)$r.squared", sep="")))
    
    cv.results <- list()
    for (k in 1:10)  {
      
      splits <- splitdf(m1)
      mod1d_10 <- splits$trainset
      mod1d_90 <- splits$testset
      out_90 <- lmer(m1.formula, data=mod1d_90, control=ctrl)
      mod1d_10$predicted <- predict(object=out_90, newdata=mod1d_10, allow.new.levels=TRUE, REform=NULL )
      
      cv.results[[k]] <- mod1d_10
      
    }
    
    mod1d_all <- do.call(rbind, cv.results)
    
    mod1d_reg <- lm(mod1d_all$PM25 ~ mod1d_all$predicted)
    
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'CV_R2'] <- summary(mod1d_reg)$r.squared", sep="")))
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'CV_int'] <- summary(mod1d_reg)$coef[1,1]", sep="")))
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'CV_int_se'] <- summary(mod1d_reg)$coef[1,2]", sep="")))
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'CV_slope'] <- summary(mod1d_reg)$coef[2,1]", sep="")))
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'CV_slope_se'] <- summary(mod1d_reg)$coef[2,2]", sep="")))
    #rmspe
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'RMSPE'] <- sqrt(mean(mod1d_reg$residual^2))", sep="")))
    
    #spatial
    aggf<- ddply(mod1d_all, c("SiteCode"), function(df) return(c(barpm=mean(df$PM25),barpred=mean(df$predicted))))
    mod_spatial <- lm(barpm ~ barpred, data=aggf)
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'spatial'] <- summary(mod_spatial)$r.squared", sep="")))
    aggfdt <- data.table(aggf)
    
    #temporal
    dat <- merge(mod1d_all, aggf, by='SiteCode', all.x=T)
    dat$delpm <-dat$PM25-dat$barpm
    dat$delpred <-dat$predicted-dat$barpred
    mod_temporal <- lm(delpm ~ delpred, data=dat)
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'temporal'] <- summary(mod_temporal)$r.squared", sep="")))
    
    #rmspe_spatial (RMSPE of spatial predictions)
    dat$spatresid <- dat$barpm - dat$barpred
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'RMSPE_spatial'] <- sqrt(mean(dat$spatresid^2))", sep="")))
    
    #Local PM: Start
    lu$elev_m <- NULL
    mod1d_all_st <- merge(mod1d_all, lu, by="SiteCode", all.x=T)
    mod1d_all_st <- na.omit(mod1d_all_st)
    mod1d_all_st$resm1 <- mod1d_all_st$PM25 - mod1d_all_st$predicted
    
    cv.lpm <- list()
    for (k in 1:10)  {
      
      #Splitting data
      splits <- splitdf(mod1d_all_st)
      mod1d_10 <- splits$trainset
      mod1d_90 <- splits$testset
      
      #Main model
      out_90 <- lmer(m1.formula, data=mod1d_90, control=ctrl)
      
      #Local PM model
      mod1d_90$Resid <- resid(out_90)
      lpm_90 <- gam(lpm.formula, data=mod1d_90)
      pred_10 <- predict(object=out_90, newdata=mod1d_10, allow.new.levels=TRUE, REform=NULL)
      pred_lpm_10 <- predict(object=lpm_90, newdata=mod1d_10, allow.new.levels=TRUE, REform=NULL)
      mod1d_10$pred.lpm <- pred_10 + pred_lpm_10
      
      cv.lpm[[k]] <- mod1d_10
      
    }
    cv.lpm <- do.call(rbind, cv.lpm)
    
    ####################reg
    reg.cv.lpm <- lm(cv.lpm$PM25 ~ cv.lpm$pred.lpm)
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'LPM_CV_R2'] <- summary(reg.cv.lpm)$r.squared", sep="")))
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'LPM_CV_int'] <- summary(reg.cv.lpm)$coef[1,1]", sep="")))
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'LPM_CV_int_se'] <- summary(reg.cv.lpm)$coef[1,2]", sep="")))
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'LPM_CV_slope'] <- summary(reg.cv.lpm)$coef[2,1]", sep="")))
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'LPM_CV_slope_se'] <- summary(reg.cv.lpm)$coef[2,2]", sep="")))
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'LPM_RMSPE'] <- sqrt(mean(reg.cv.lpm$residual^2))", sep="")))
    
    #spatial
    aggf <- ddply(cv.lpm, c("SiteCode"), function(df) return(c(barpm=mean(df$PM25), barpred=mean(df$predicted))))
    mod_spatial <- lm(barpm ~ barpred, data=aggf)
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'LPM_spatial'] <- summary(mod_spatial)$r.squared", sep="")))
    aggfdt <- data.table(aggf)
    
    #temporal
    dat <- merge(cv.lpm, aggf, by='SiteCode', all.x = T)
    dat$delpm <- dat$PM25 - dat$barpm
    dat$delpred <- dat$predicted - dat$barpred
    mod_temporal <- lm(delpm ~ delpred, data=dat)
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'LPM_temporal'] <- summary(mod_temporal)$r.squared", sep="")))
    
    #rmspe_spatial (RMSPE of spatial predictions)
    dat$spatresid <- dat$barpm - dat$barpred
    eval(parse(text=paste("mod1table[mod1table$Year==", i, " & mod1table$Group==", j, ", 'LPM_RMSPE_spatial'] <- sqrt(mean(dat$spatresid^2))", sep="")))
    
    #Just round to 2 decimal places
    #eval(parse(text=paste('mod1table$', i, '_', j, ' <- round(mod1table$', i, '_', j, ', 2)', sep='')))
    
  }
}

saveRDS(mod1table, 'C:/Data/Thesis 2/Results/Stage 1/Results_Stage1_CV2_old2.rds')
write.csv(mod1table, 'C:/Data/Thesis 2/Results/Stage 1/Results_Stage1_CV2_old2.csv', row.names=F)
