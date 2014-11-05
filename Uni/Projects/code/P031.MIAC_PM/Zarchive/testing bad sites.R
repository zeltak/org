

#import and clip data
mod1<-readRDS ("/media/NAS/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN008_model_prep/mod1_2011.rds")

mod1C <- mod1[long_aod > -76 & long_aod < -66.7 & lat_aod < 47 & lat_aod > 38.8, ]


###############
#MOD1
###############

###############
#switch to choose all area or cliped area for paper
m1_2011<-mod1C
#m1_2011<-mod1
###############


#per year
m1.formula<-PM25~aod
modelList <- dlply(m1_2011, "c", function(x) lm(m1.formula, data=x))
aquaPY<-t(as.data.table(lapply(modelList, function(x) summary(x)$r.squared)))
aquaPY



#per station
modelList <- dlply(m1_2011, "EPACode", function(x) lm(m1.formula, data=x))
aquaSTN<-t(as.data.table(lapply(modelList, function(x) summary(x)$r.squared)))
aquaSTN




###EXAMINE BAD STATIONS

#MAL
x<-ggplot(m1_2011[EPACode == "90113002"], aes(aod, PM25)) + geom_line() + stat_smooth(method=lm)+
theme_bw(13)
x+ annotate("text", label="STN == MAL", parse = TRUE, x=0.5, y=400) 


