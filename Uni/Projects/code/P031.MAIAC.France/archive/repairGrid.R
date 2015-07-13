#find lat/lon point in a certain distance from a known lat/lon point
#clear workspace
rm(list = ls())
library(data.table)
library(geosphere)
#dist = (arccos(sin(lat1) · sin(lat2)) + (cos(lat1) · cos(lat2)) · cos(lon1 - lon2)) · R

#read old grid
grid=fread('C:\\Users\\MEYTAR\\Documents\\R\\France\\OLDfullgrid.csv')

lat_old=grid[,'Latitude',with=FALSE] #insert vector of lat
setnames(lat_old,"Latitude","lat_old")
lon_old=grid[,'Longitude',with=FALSE] #insert vector of long
setnames(lon_old,"Longitude","lon_old")

r=6378137 #assuming a spherical approximationa of the figure of the Earth.  R is Radius of the Earth in m.

b = 135 #Bearing is 315 degrees between points (center to upper left) (upper left to center is 135)
d = sqrt(0.5)*1000 #Distance in m (diagonal of 1km square)
L1=cbind(lon_old,lat_old)#insert 2 column matrix with all lat-long pairs
L1=as.data.table(L1)

grid.n=round(destPoint(L1, b, d,r),6)
grid.n=as.data.table(grid.n)

#create aodid
grid.n[,aodid_new:=paste(lon,lat , sep="-")]
L1$aodid=L1[,aodid:=paste(lon_old,lat_old , sep="-")]


bothgrids=cbind(L1,grid.n)
write.csv(bothgrids,'C:\\Users\\MEYTAR\\Documents\\R\\France\\aodid_grid.csv')
