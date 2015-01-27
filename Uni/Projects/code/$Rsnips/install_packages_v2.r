# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
 
ipak <- function(pkg){
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]

if (length(new.pkg))
install.packages(new.pkg, dependencies = TRUE)

sapply(pkg, require, character.only = TRUE)
}
 
# usage
packages <- c("ggplot2", "plyr","lme4", "FNN","data.table","reshape","reshape2","sqldf","sp","gtools","doBy","Hmisc","gdata","car","dplyr", "RgoogleMaps","ggmap","broom","DataCombine")

ipak(packages)


