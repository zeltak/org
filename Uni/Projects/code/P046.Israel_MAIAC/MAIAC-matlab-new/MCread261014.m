%The function name that is called by MCrun to extract hdf files into a table with output of: day,month,year,hour,lat,lon,aod.

function [DataArray]=MCread261014(PathName,lat,lon)
addpath (genpath('C:\Users\MEYTAR\Documents\MATLAB1'))
clc
DataArray=zeros(10000,24);
DataArray(1,:)=1;

% define M (length of the rows from pathnames)
[M, N] =size(PathName);
% define x,y from lat and lon file, its from LON since lat and long are
% same size
[X, Y] = size(lon);
tempLon = reshape(lon,X*Y,1);
tempLat = reshape(lat,X*Y,1);
%tr and tc are the indexes that are clipped to the study area
[tr,tc]=find(tempLon<=36 & tempLon>=34.1); %exclude data with Lon above 36 and below 34.1
%final lat and lon
LatN=tempLat(tr);
LonN=tempLon(tr);


%define the length of i 1 to..M. for testing purpose we can change M to
%numbner IE 1:3
for I = 2:2
    
    % Import AOD,Lat,Lon Data from hdf files
    
    %read AOD from PATHNAME (the 1:M)
    %you can get the variable name and order from importing data (hdfimport
    %tool. then add the names to the next line at the end in qoutes
    temp1 = hdfread( PathName{I}, 'grid1km', 'Fields', 'Optical_Depth_Land' );
    temp2 = hdfread( PathName{I}, 'grid1km', 'Fields', 'AOT_QA' );
    temp3 = hdfread( PathName{I}, 'grid1km', 'Fields', 'AOT_Uncertainty' );
    temp4 = hdfread( PathName{I}, 'grid1km', 'Fields', 'Column_WV' );
    
    % AOD data matrix reshape, reshape 600x600 matrix into clipped area
    % matrix. the new matrix
    %size of temp which is the new size    % tempXn is the reshape of aod from matrix to vector
    %based on LatN(clipped lat vector)
    temp1n = reshape(temp1,X*Y,1);
    %this is where its clipped
    temp1n=temp1n(tr);
    %%ar and ac are the indexes that are clipped to AOD not fill value (-9.9990)
    AOD=double(temp1n);
    AOD=AOD*0.001;
    %Lat,Lon (all NaN's excluded from AOD)
    Lat=LatN;
    Lon=LonN;
    
    %AOD QA
  temp2n = reshape(temp2,X*Y,1);
    temp2n=temp2n(tr);
     QA=de2bi(temp2n); 
    
    %AOD Uncertainty
    temp3n = reshape(temp3,X*Y,1);
    temp3n=temp3n(tr);
    UN=double(temp3n);
    UN=UN*0.0001;
    
   %water vapor column 
   temp4n = reshape(temp4,X*Y,1);
    temp4n=temp4n(tr);
    WV=double(temp4n);
    WV=WV*0.001;
    
    %Create table with date, lat,lon,aod and then add other variables.
    [r, c] = size(Lat);
    [r1,c1]=find(DataArray(:,1),1,'last');
    maxr1=max(r1);
    DataArray(maxr1+1,1)=maxr1;
    DataArray(maxr1+1:maxr1+r,5) = Lat;
    DataArray(maxr1+1:maxr1+r,6) = Lon;
    DataArray(maxr1+1:maxr1+r,7) = AOD;
    DataArray(maxr1+1:maxr1+r,8) = UN;
    DataArray(maxr1+1:maxr1+r,9) = WV;
    DataArray(maxr1+1:maxr1+r,10:24) = QA;

    
    %Convert Julian date to normal date DD-MM-YYY
    %extract the string from filename based on text position in filename
    %temp5 is the year variable
    temp5 = str2double( PathName{I}(69:72) );
    %temp6 is the julian day of year
    temp6 = str2double( PathName{I}(73:75) );
    [year,month, day]=julian2date(temp6,temp5);
    
    % Insert Date and Time data to Array
    
    DataArray(maxr1+1:maxr1+r,1) = day;
    DataArray(maxr1+1:maxr1+r,2) = month;
    DataArray(maxr1+1:maxr1+r,3) = year;
    %extract hour from filename as in above
    hour = str2double( PathName{I}(76:79) ); DataArray(maxr1+1:maxr1+r,4) = hour;
    
 
    
end

 [row,col]=find(DataArray(:,7)~= -28.672); %exclude all days with AOD = NAN
    DataArray=DataArray(row,:);





