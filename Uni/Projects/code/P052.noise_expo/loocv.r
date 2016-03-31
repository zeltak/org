
m1.formula<-as.formula(dB_Avg_M~ P_industry+oneway+(1|Stat) )

# cross-validation and model building
# repeated leave x monitors out CV
TA<-as.data.table(TA)
neveruse <- c("")
mons <- unique(TA[!Address %in% neveruse, Address]); length(mons)
xout <- 1 # number of monitors to hold out
# how many combinations if we pull out xout mons
ncol(combn(mons, xout))
n.iter <- 97 #how many monitors we have -1 to decide how many times this runs
# we will compute mean of the other monitors using all monitoring data
setkey(TA, Address)

# list to store scheme
cvscheme <- list()
cvout <- list()
# set seed for reproducibility
set.seed(20150112)

# cross-validation in parallel
library(doParallel)
library(doRNG)

registerDoParallel(14)
# use a proper reproducible backend RNG
registerDoRNG(1234)
system.time({
  iter.out <- foreach(i=1:n.iter, .combine = rbind, .packages = c("data.table", "lme4") ) %dorng% {
  #system.time(for(i in 1:n.iter){
  #mons.test <- mons[sample(length(mons), xout)]
  mons.test <- combn(mons, xout)[,i]
  cvscheme[[i]] <- mons.test
  test <- TA[Address %in% mons.test, ]
  train<- TA[!Address %in% mons.test, ]
  # fit the model
  print(paste("iteration #", i, "testing set is monitor", paste(unique(test$Address), collapse = ","), ",", nrow(test), "records from", paste(format(range(test$day), "%Y-%m-%d"), collapse = " to ")))
  print(paste("training on", nrow(train), "records"))
  trainmod <-  lmer(m1.formula, data =  train)
  test$predcv <- predict(object=trainmod,newdata=test,allow.new.levels=TRUE,re.form=NULL )
  test$itercv <- i  
  # export these results
  test[, list(Address, dB_Avg_M, predcv, itercv)]
}# end of cross-validation loop
})
summary(lm(dB_Avg_M ~ predcv, data = iter.out))


# compute root mean squared error
iter.out[, sqrt(mean((tempcmin - predcv)^2))]
