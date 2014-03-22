
allbestpredlist <- list()
path.data<-"/media/NAS/Uni/Projects/P020_Temprature_NE_MIA/3.Work/3.Analysis/AN_001_mods_CV/"

for(i in 2000:2011){
  allbestpredlist[[paste0("Mod1_", i)]] <- read.dbf(paste0(path.data, "Mod1_", i, ".dbf"))
  print(i)
} 
allbestpred <- rbindlist(allbestpredlist)
rm(allbestpredlist)

mod1<-allbestpred

DELLIST <-  names(mod1) %in% c("DTckin", "humidity")
mod1d <- mod1[!DELLIST]
mod1d<-na.omit(mod1d)
mod1d$predicted<-NA
mod1d<-as.data.table(mod1d)
describe(mod1d$STATE_ABBR)

outd = lme(tempc ~ NTckin+elev+purban+NDVI+ws, random = ~1 + NTckin| date,  data =  mod1d) 
mod1d$predicted <- predict(object=outd,newdata=mod1d )
summary(lm(tempc~predicted,data=mod1d))


library(car)

mod1d$month <- as.numeric(format(mod1d$date, "%m"))
#1-winter, 2-spring,3-summer,4-autum
mod1d$season<-recode(mod1d$month,"1=1;2=1;3=2;4=2;5=2;6=3;7=3;8=3;9=4;10=4;11=4;12=1")
#1-winter, 2-summer
mod1d$seasonSW<-as.character(recode(mod1d$month,"1=1;2=1;3=1;4=2;5=2;6=2;7=2;8=2;9=2;10=1;11=1;12=1"))


mod1d_winter <- mod1d[seasonSW %in% c("1"), ]
mod1d_summer <- mod1d[seasonSW %in% c("2"), ]
summary(lm(tempc~predicted,data=mod1d_winter))
0.92
summary(lm(tempc~predicted,data=mod1d_summer))
0.87



mod1d_s1 <- mod1d[season %in% c("1"), ]
mod1d_s2 <- mod1d[season %in% c("2"), ]
mod1d_s3 <- mod1d[season %in% c("3"), ]
mod1d_s4 <- mod1d[season %in% c("4"), ]

summary(lm(tempc~predicted,data=mod1d_s1))
summary(lm(tempc~predicted,data=mod1d_s2))
summary(lm(tempc~predicted,data=mod1d_s3))
summary(lm(tempc~predicted,data=mod1d_s4))









#boston analysis
bos2005<- mod1d[glong < -70.5 & glong > -71.5 & glat > 42.00 & glat < 43, ]
summary(lm(tempc~predicted,data=bos2005))
0.95

###3 cities

#NYC analysis
ny2005<- mod1d[glong < -73 & glong > -75 & glat > 40 & glat < 42, ]
summary(lm(tempc~predicted,data=ny2005))
0.96




#########paper
#########paper
#########paper
#urban
mod1d_urb <- mod1d[purban > 75, ]
mod1d_rural <- mod1d[purban <=75, ]

summary(lm(tempc~predicted,data=mod1d_urb))
0.9693
summary(lm(tempc~predicted,data=mod1d_rural))
0.8851 
#########paper
#########paper
#########paper






