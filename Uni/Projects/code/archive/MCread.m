
function [DataArray]=MCread(PathName)
clc
DataArray=zeros(100000,7);
DataArray(1,:)=1;
[M, N] =size(PathName);
Lat=hdfread ('C:\Users\meytar\Documents\MAIAC\MAIAC_RAWDATA_25012014\MAIACLatlon.Israel.hdf','latlon','Fields','lat'); 
Lon=hdfread ('C:\Users\meytar\Documents\MAIAC\MAIAC_RAWDATA_25012014\MAIACLatlon.Israel.hdf','latlon','Fields','lon'); 

for I = 1:10
    
    % Import AOD,Lat,Lon Data from hdf files
    temp4 = Lat;
    temp5 = Lon;
    temp6 = hdfread( PathName{I}, 'grid1km', 'Fields', 'Optical_Depth_Land' );
   
    
    % data matrix reshape
    [X, Y] = size(temp4);
    temp7 = reshape(temp4,X*Y,1); [r, c] = size(temp7);
    temp8 = reshape(temp5,X*Y,1);
    temp9 = reshape(temp6,X*Y,1);
   
    
    % insert data to final array
    [r1,c1]=find(DataArray(:,1),1,'last');
    maxr1=max(r1);
    DataArray(maxr1+1,1)=maxr1;
    DataArray(maxr1+1:maxr1+r,5) = temp7;
    DataArray(maxr1+1:maxr1+r,6) = temp8;
    DataArray(maxr1+1:maxr1+r,7) = temp9;
  
    
    %Convert Julian date to normal date DD-MM-YYY
    temp1 = str2double( PathName{I}(77:80) ); 
    temp2 = str2double( PathName{I}(81:83) ); 
    [year,month, day]=julian2date(temp2,temp1);

    % Insert Date and Time data to Array
    
    DataArray(maxr1+1:maxr1+r,1) = day;
    DataArray(maxr1+1:maxr1+r,2) = month;
    DataArray(maxr1+1:maxr1+r,3) = year;
    hour = str2double( PathName{I}(84:87) ); DataArray(maxr1+1:maxr1+r,4) = hour;
 
    %exclude data out of Israel Polygon 35.7<Lon<34.1
      [r,c]=find(DataArray(maxr1+1:maxr1+r,6)>35.7 | DataArray(maxr1+1:maxr1+r,6)<34.1);
         DataArray(r,:)=[];
 
     
end

%AOD Factor
DataArray(1,:)=[];
DataArray(:,7)=DataArray(:,7)/1000;

% Change AOD values of '-9999' to NAN
k=1;
for I=1:length(DataArray)-1
    
    if DataArray(I,7)==-9.9990
        DataArray(I,7)=NaN;
    end
end