# Cross-validation
# for mod1
library(doRNG)
library(doParallel)


# cross-validation and model building
# repeated leave x monitors out CV
#neveruse <- c("PER")
neveruse <- c("")
mons <- unique(m1.all[!stn %in% neveruse, stn]); length(mons)
xout <- 1 # number of monitors to hold out
# how many combinations if we pull out xout mons
ncol(combn(mons, xout))
n.iter <- 81
# we will compute mean of the other monitors using all monitoring data
setkey(m1.all, stn)

# list to store scheme
cvscheme <- list()
cvout <- list()
# set seed for reproducibility
set.seed(20150112)

# cross-validation in parallel

registerDoParallel(14)
# use a proper reproducible backend RNG
registerDoRNG(1234)
system.time({
  iter.out <- foreach(i=1:n.iter, .combine = rbind, .packages = c("data.table", "lme4") ) %dorng% {
  #system.time(for(i in 1:n.iter){
  #mons.test <- mons[sample(length(mons), xout)]
  mons.test <- combn(mons, xout)[,i]
  cvscheme[[i]] <- mons.test
  test <- m1.all[stn %in% mons.test, ]
  train<- m1.all[!stn %in% mons.test, ]
  # fit the model
  print(paste("iteration #", i, "testing set is monitor", paste(unique(test$stn), collapse = ","), ",", nrow(test), "records from", paste(format(range(test$day), "%Y-%m-%d"), collapse = " to ")))
  print(paste("training on", nrow(train), "records"))
  trainmod <-  lmer(m1.formula, data =  train)
  test$predcv <- predict(object=trainmod,newdata=test,allow.new.levels=TRUE,re.form=NULL )
  test$itercv <- i  
  # export these results
  test[, list(day, stn, PM25, predcv, itercv)]
}# end of cross-validation loop
})
summary(lm(PM25 ~ predcv, data = iter.out))
# compute root mean squared error
iter.out[, sqrt(mean((PM25 - predcv)^2))]
