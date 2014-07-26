
%The function name that is called by MCrun to extract hdf files into a table with output of: day,month,year,hour,lat,lon,aod.

function [Data]=MCreadv2(PathName,Lat,Lon)
clc
DataArray=zeros(100000,7);
DataArray(1,:)=1;
k=1;
% define M (length of the rows from pathnames)
[M, N] =size(PathName);
% define x,y from lat and lon file, its from LON since lat and long are
% same size
[X, Y] = size(Lon);
    tempLon = reshape(Lon,X*Y,1);
    tempLat = reshape(Lat,X*Y,1);
%tr and tc are the indexes that are clipped to the study area
[tr,tc]=find(tempLon<=36 & tempLon>=34.1); %exclude data with Lon above 36 and below 34.1
%final lat and lon
LatN=tempLat(tr);
LonN=tempLon(tr);


%define the length of i 1 to..M. for testing purpose we can change M to
%numbner IE 1:3
for I = 1:2;
    
    % Import AOD,Lat,Lon Data from hdf files
   
    %read AOD from PATHNAME (the 1:M) 
    %you can get the variable name and order from importing data (hdfimport
    %tool. then add the names to the next line at the end in qoutes
    temp6 = hdfread( PathName{I}, 'grid1km', 'Fields', 'Optical_Depth_Land' ); 
    
    
    % AOD data matrix reshape, reshape 400x400 matrix into clipped area
    % matrix. the new matrix 
    %size of temp which is the new size based on LatN(clipped lat vector)
    [r, c] = size(LatN);
    % temp7 is the reshape of aod from matrix to vector
    temp7 = reshape(temp6,X*Y,1);
    %this is where its clipped
    temp7=temp7(tr);
    
    
    % insert data to final array for final data
    [r1,c1]=find(DataArray(:,1),1,'last');
    maxr1=max(r1);
    DataArray(maxr1+1,1)=maxr1;
    DataArray(maxr1+1:maxr1+r,5) = LatN;
    DataArray(maxr1+1:maxr1+r,6) = LonN;
    DataArray(maxr1+1:maxr1+r,7) = temp7;
    
    
    %Convert Julian date to normal date DD-MM-YYY
    %extract the string from filename based on text position in filename
    %temp1 is the year variable
    temp1 = str2double( PathName{I}(59:62) );
    %temp2 is the julian date
    temp2 = str2double( PathName{I}(63:65) );
    [year,month, day]=julian2date(temp2,temp1);
    
    % Insert Date and Time data to Array
    
    DataArray(maxr1+1:maxr1+r,1) = day;
    DataArray(maxr1+1:maxr1+r,2) = month;
    DataArray(maxr1+1:maxr1+r,3) = year;
    %extract hour from filename as in above
    hour = str2double( PathName{I}(67:69) ); DataArray(maxr1+1:maxr1+r,4) = hour;
    
    end

%apply AOD scale Factor
DataArray(:,7)=DataArray(:,7)/1000;
%dont touch
DataArray(1,:)=[];

%Exclude days with AOD values of '-9999'
k=1;Data=zeros(1,7);
for I=1:length(DataArray)
    
    if DataArray(I,7)~=-9.9990
        Data(k,:)=DataArray(I,:); k=k+1;
    end
end




