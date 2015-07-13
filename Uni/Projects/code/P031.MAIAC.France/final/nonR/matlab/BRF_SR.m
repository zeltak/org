%The function name that is called by BRFrun to extract hdf files into a table with output of: 
%day,month,year,hour,lat,lon,surface reflectance.

function [DataArray]=BRF_SR(Path,latlon,index)
clc
addpath('/media/NAS/Uni/org/files/Uni/Projects/code/C00Matlab/');

DataArray=zeros(10000,8);
DataArray(1,:)=1;

% define M (length of the rows from pathnames)
[M, N] =size(Path);
%define Tile

%define the length of i 1 to..M. for testing purpose we can change M to
%numbner IE 1:3
for K = 1:2
    
    % Import SR,Lat,Lon Data from hdf files
    
    %read AOD from PATHNAME (the 1:M)
    %you can get the variable name and order from importing data (hdfimport
    %tool. then add the names to the next line at the end in qoutes
    temp1 = hdfread( Path{K}, 'grid1km', 'Fields', 'sur_refl' ); 

    % BRF data matrix reshape, reshape 12x1200x1200 matrix into clipped area
    % matrix. the new matrix
    %size of temp which is the new size    
    % tempXn is the reshape of aod from matrix to vector
    %based on LatN(clipped lat vector)
    temp1n=0;SR=[];SuRef=[];
    [X,Y,Z]=size(temp1);
    for KK = 1:X
   temp1n=temp1(KK,:,:);
    temp1n = reshape(temp1n,Z*Y,1);
    %this is where its clipped
    temp1n=temp1n(index(:,1));
% Solar and Viewing Geometry Parameters (FROM MATLAB WEBSITE)
% Solar_Zenith
% Description: Solar Zenith Angle, Cell to Sun 
% Dimensions: (Cell_Along_Swath, Cell_Across_Swath) 
% Valid Range: 0 to +180 degrees
% Solar_Azimuth
% Description: Solar Azimuth Angle, Cell to Sun 
% Dimensions: (Cell_Along_Swath, Cell_Across_Swath) 
% Valid Range: -180 to +180 degrees
% Sensor_Zenith
% Description: Sensor Zenith Angle, Cell to Sensor 
% Dimensions: (Cell_Along_Swath, Cell_Across_Swath) 
% Valid Range: 0 to 180 degrees
% Sensor_Azimuth
% Description: Sensor Azimuth Angle, Cell to Sensor 
% Dimensions: (Cell_Along_Swath, Cell_Across_Swath) 
% Valid Range: -180 to 180 degrees
    
    SuRef=double(temp1n);
    SuRef=SuRef*0.0001; %range -1 to +1 as reflectance is a ratio
   SuRef(:,2)=KK;
    
    %Create table with date, lat,lon,aod and then add other variables.
    [r, c] = size(latlon);
    [r1,c1]=find(DataArray(:,7),1,'last');
    maxr1=max(r1);
    DataArray(maxr1+1,1)=maxr1;
    DataArray(maxr1+1:maxr1+r,[5 6]) = latlon;
    DataArray(maxr1+1:maxr1+r,[7 8]) = SuRef;
        
    %Convert Julian date to normal date DD-MM-YYY
    %extract the string from filename based on text position in filename
    str=char(regexp(Path{K},'[.]\d+[.]','match'));
    %temp4 is the year variable
    temp4 = str2double(str(2:5)) ;
    %temp5 is the julian day of year
    temp5 =str2double( str(6:8 ));
    [year,month, day]=julian2date(temp5,temp4);
    
    % Insert Date and Time data to Array
    
    DataArray(maxr1+1:maxr1+r,1) = day;
    DataArray(maxr1+1:maxr1+r,2) = month;
    DataArray(maxr1+1:maxr1+r,3) = year;
    %extract hour from filename as in above
    hour = str2double(str(9:12)); DataArray(maxr1+1:maxr1+r,4) = hour;
    end  
 
    
end

 [row,col]=find(DataArray(:,7)~= -2.8672); %exclude all days with NAN
    DataArray=DataArray(row,:);
 
  
    
    





