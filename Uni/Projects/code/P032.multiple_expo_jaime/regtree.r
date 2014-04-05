library(tree)

data1 <-  read.csv("f:/Uni/Projects/3.1.0.TMP_PROJECTS/Jamie_regtree/data/bwfinal4regtree.csv", header=T)  
names(data1)
data2<-data1[1:200,]
data3<-na.omit(data1$pmnewma2)
data4<-data1[]

xx<- names(data1) %in% c("pmnewma2","fintempma2","med_income","p_ospace", "birthw")
yy <- data1[xx]
yy<-na.omit(yy)
hist(yy$birthw)
hist(yy$pmnewma2)
str(yy)
summary(yy)
cpus.ltr <- tree( birthw ~ pmnewma2+fintempma2, data=data2,na.action = na.omit )
plot(cpus.ltr)
str(mtcars)

fit <- rpart(mpg  ~ cyl+hp,method="anova", data=mtcars)



library(rpart)
fit <- rpart(birthw ~ med_income+pmnewma2,method="anova", data=data2)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for bw")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "c:/tree.ps", 
     title = "Classification Tree for Kyphosis")









library(randomForest)
fit <- randomForest( birthw ~ pmnewma2+fintempma2, data=data2,,na.action = na.omit)

print(fit) # view results 
importance(fit) # importance of each predictor