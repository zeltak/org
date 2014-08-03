library(mgcv); library(gdata); library(data.table)

for (i in 2003)  {
  
  for (j in 1)  {
    
    print(paste('Year of', i, 'Group', j, 'at', Sys.time()))
    
    m2mpm <- read.csv(paste('/n/holyscratch/jschwrtz_lab/mhlee/Stage2_mpm_', i, "_", j, ".csv", sep=""), header=T)
    
    m2.smooth <- lme(Pred2 ~ mpm*as.factor(bimon) + mpm*pbl + elev_m + ah_gm3, random=list(GID= ~1 + mpm), weights=varFixed(~IPW_N), data=m2mpm)
        
    m2mpm$tpred <- predict(m2.smooth)
    mod3a_reg <- lm(m2mpm$Pred2 ~ m2mpm$tpred)
    eval(parse(text=paste('mod3table$r', i, '_', j, '[1] <- summary(mod3a_reg)$r.squared', sep='')))
    m2mpm$resid <- residuals(m2.smooth)
  
    for (k in 1:6)  {                      
      bimon <- m2mpm[m2mpm$bimon==k, ]
      fit = gam(resid ~ s(long_aod, lat_aod),  data=bimon)
      Xpred=(bimon$Pred2 - fit$fitted)
      assign(paste("fit_", k, sep=""), fit)
      assign(paste("Xpred_", k, sep=""), Xpred)
                    }
    m2mpm$newpred <- c(Xpred_1, Xpred_2, Xpred_3, Xpred_4, Xpred_5, Xpred_6)
  
    #rerun the lme on the predictions including the spatial spline (smooth)
    Final_pred = lme(newpred ~ mpm, random=list(GID= ~1 + mpm), control=lmeControl(opt = "optim"),data= m2mpm  )
  
    m2mpm$tpred2 <- predict(Final_pred)
    mod3b_reg <- lm(m2mpm$Pred2 ~ m2mpm$tpred2)
    eval(parse(text=paste('mod3table$r', i, '_', j, '[2] <- summary(mod3b_reg)$r.squared', sep='')))
  
    #saveRDS(m2.smooth, paste('/data/Mihye/Stage 3/Results/m2.smooth_', i, '_', j, '.rds'))
    #saveRDS(Final_pred, paste('/data/Mihye/Stage 3/Results/Final_pred_', i, '_', j, '.rds'))
    
    mod2 <-m2mpm[ , c('GID', 'DATE', 'Pred2')]
    colnames(mod2) <- c('GID', 'Date', 'Pred2')
  
    keep(i, j, mod2, Final_pred, mod3table, fit_1, fit_2, fit_3, fit_4, fit_5, fit_6, sure=TRUE)
    gc()

    mpmg <- read.csv(paste('/n/holyscratch/jschwrtz_lab/mhlee/Stage3_', i, "_", j, ".csv", sep=""), header=T)
    
    mpmg$grid_pred2 <- predict(Final_pred, mpmg, level=0)
    #Interpolation: Start
    random.coef.GID <- data.frame(Final_pred$coeff$random$GID)
    colnames(random.coef.GID) <- c('Intercept', 'Ran_Cof_mpm')
    random.coef.GID$GID <- rownames(random.coef.GID)
    mpmg <- merge(mpmg, random.coef.GID, by='GID')
    #mpmg <- merge(mpmg, random.coef.GID, by='GID', all.x=T)
    #Interpolation: End
    mpmg$zb <- mpmg$Intercept + mpmg$mpm*mpmg$Ran_Cof_mpm
    mpmg$mixpred <- mpmg$grid_pred2 + mpmg$zb
    
    gc()
  
    m2_agg <- read.csv('/n/holyscratch/jschwrtz_lab/mhlee/Unique_Grids.csv', header=T)
    
    for (m in 1:6)  {                      
      #split back into bimons to include the gam prediction in final prediction        
      mpmg_bimon <- mpmg[mpmg$bimon==m, ]
      fit <- eval(parse(text=paste('fit_', m, sep='')))
      uniq_gid_bimon <- m2_agg #addin unique grid to each bimon
      #get predictions for Bimon residuals
      uniq_gid_bimon$gpred <- predict.gam(fit, uniq_gid_bimon)
      mpmg_bimon <- merge(mpmg_bimon, uniq_gid_bimon, all.x=T)
      assign(paste('mpmg_bimon', m, sep=''), mpmg_bimon)
                    }
    mod3 <- rbind(mpmg_bimon1,mpmg_bimon2,mpmg_bimon3,mpmg_bimon4,mpmg_bimon5,mpmg_bimon6)
  
    #Create Pred3
    mod3$Pred3 <- mod3$mixpred + mod3$gpred
    mod3 <- mod3[mod3$Pred3>=0, ]
    #hist(mod3$Pred3)
    #saveRDS(mod3, paste("/data/Mihye/Stage 3/Results/pred3_", i, "_", j, ".rds", sep=""))
    #clean
    keep(i, j, mod2, mod3table, mod3, sure=TRUE)
    gc()
  
    #load mod1
    mod1 <- read.csv(paste("/n/holyscratch/jschwrtz_lab/mhlee/Stage1_", i, "_", j, ".csv", sep=""), header=T)
    mod1 <- mod1[ , c("GID", "Date", "PM25", "Pred", "SiteCode")]
    colnames(mod1)[colnames(mod1)=='Pred'] <- 'Pred1'
  
    #R2.m3
    mod1 <- merge(mod1, mod3, by=c('GID', 'Date'), all.x = T)        
    mod3d_reg <- lm(PM25 ~ Pred3, data=mod1)
    eval(parse(text=paste('mod3table$r', i, '_', j, '[3] <- summary(mod3d_reg)$r.squared', sep='')))
  
    #saveRDS(mod1, paste('/data/Mihye/Stage 3/Results/Mod3 and 1 Merge/mod1n3_', i, '_', j, '.rds', sep=''))
    #saveRDS(mod3table, '/data/Mihye/Stage 3/Results/mod3table.rds')
    write.csv(mod3table, '/n/holyscratch/jschwrtz_lab/mhlee/mod3table.csv', row.names=F)

    # store the best available
    mod3best <- mod3[ , c('GID', 'Date', 'STATE_ABBR', 'long_aod', 'lat_aod', 'Pred3')]
    mod3best <- merge(mod3best, mod2, by=c('Date', 'GID'), all.x=T)
    mod1 <- mod1[ , c("GID","Date","PM25", "Pred1")]
    mod3best <- merge(mod3best, mod1, by=c('Date', 'GID'), all.x=T)

    mod3best <- data.table(mod3best)
    mod3best[ , bestpred := Pred3]
    mod3best[!is.na(Pred2), bestpred := Pred2]
    mod3best[!is.na(Pred1), bestpred := Pred1]

    #saveRDS(mod3best, paste('/data/Mihye/Stage 3/Results/Final Preds/mod3best_', i, '_', j, '.rds', sep=''))
    #mod3best <- readRDS(paste('/data/Mihye/Stage 3/Results/Final Preds/mod3best_', i, '_', j, '.rds', sep=''))
    mod3best[ , c('PM25', 'Pred1', 'Pred2', 'Pred3'):=NULL]
      
    write.csv(mod3best, paste("/n/holyscratch/jschwrtz_lab/mhlee/final_pred_", i, "_", j, ".csv", sep=""), row.names=F)
    annual.mean <- aggregate(bestpred~GID, mod3best, mean)
    write.csv(annual.mean, paste('/n/holyscratch/jschwrtz_lab/mhlee/Annual_Mean_', i, '_', j, '.csv', sep=''), row.names=F)

      }
  }

print(paste('End at', Sys.time()))