library(data.table)
library(Hmisc)

T2003p1<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2003/TileName-00v00-part1.txt")
T2003p2<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2003/TileName-00v00-part2.txt")
T2004p1<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2004/TileName-00v00-part1.txt")
T2004p2<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2004/TileName-00v00-part2.txt")
T2005p1<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2005/TileName-00v00-part1.txt")
T2005p2<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2005/TileName-00v00-part2.txt")
T2006p1<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2006/TileName-00v00-part1.txt")
T2006p2<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2006/TileName-00v00-part2.txt")
T2007p1<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2007/TileName-00v00-part1.txt")
T2007p2<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2007/TileName-00v00-part2.txt")
T2008p1<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2008/TileName-00v00-part1.txt")
T2008p2<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2008/TileName-00v00-part2.txt")
T2009p1<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2009/TileName-00v00-part1.txt")
T2009p2<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2009/TileName-00v00-part2.txt")
T2010p1<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2010/TileName-00v00-part1.txt")
T2010p2<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2010/TileName-00v00-part2.txt")
T2011p1<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2011/TileName-00v00-part1.txt")
T2011p2<- fread("f:/Uni/Projects/P031_MIAC_MEXICO/1_Raw_data/modis/Per year/T2011/TileName-00v00-part2.txt")

alleyars <- rbind(T2003p1,
                  T2003p2,
                  T2004p1,
                  T2004p2,
                  T2005p1,
                  T2005p2,
                  T2006p1,
                  T2006p2,
                  T2007p1,
                  T2007p2,
                  T2008p1,
                  T2008p2,
                  T2009p1,
                  T2009p2,
                  T2010p1,
                  T2010p2,
                  T2011p1,
                  T2011p2)

write.csv(alleyars, "f:/Uni/Projects/P031_MIAC_MEXICO/3.Work/2.Gather_data/FN005_MODIS_yearly/") 
 

