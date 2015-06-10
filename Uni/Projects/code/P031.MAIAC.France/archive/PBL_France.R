#France met data
library(foreign)
library(data.table)
library(celestial)
library(zoo)

###function: repeat by rows 
rep.row<-function(x,n){
  matrix(rep(x,each=n), nrow=n, byrow=TRUE)
}
#x is the vector to be repeated and n is the number of replication
###

setwd("/media/NAS/Uni/Data/Europe/france/pbl/")
#read all dbf filenames - stewd directory
filenames <- list.files( pattern="*.dbf", full.names=TRUE)

#create a sequence of dates based on data from Kees: n_1=31/12/1999; n_5479=31/12/2014
Date=seq(as.Date("1999-12-31"), as.Date("2014-12-31"), by="days")
Date=as.Date(Date, format = "%Y/%m/%d")

#create null variables
table <- as.data.frame(setNames(replicate(4,numeric(0), simplify = F), c("X","Y","PBL","Date")))
ldf.sub <- as.data.frame(setNames(replicate(4,numeric(0), simplify = F), c("X","Y","PBL","Date")))

#loop on files
for (I in 1:length(filenames)) {
  #read file
  ldf <- read.dbf(filenames[I])
  #num=number of observations (Dates) in file
  num=dim(ldf)[2]

  #loop on Dates within a file
  for (J in 4:num) {
    #insert X,Y columns
    ldf.n=ldf[,c(2:3,J)]
  #read the index number in the column name
    str=as.numeric(substr(colnames(ldf)[J],3:6,6))
  #using the index 'str' within the Date seq 'Date' will give the date of observations Date[str]
  #replicate date for all rows
  ldf.n$Date=rep.row(Date[str],dim(ldf.n)[1])
  #give colnames to ldf.n
  colnames(ldf.n)=c("X","Y","PBL","Date")
  #merge previous and current ldf.sub (e.g if ldf[1] has 257 columns and 5385 rows, final ldf.sub should be 4 columns and 257*5385 rows)
  ldf.sub=rbind(ldf.sub,ldf.n)
  }
 #merge previous and current 'table' while table is per filename[I] 
  table=rbind(table,ldf.sub)
} 
#convert Date column to date format
table$Date=as.Date(table$Date)

#write to a csv file
write.csv(table,"C:\\Users\\MEYTAR\\Documents\\POSTDOC\\DATA\\France\\PBL\\PBL_France.csv")
