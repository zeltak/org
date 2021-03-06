%The function name that is called by MCrun to extract hdf files into a table with output of: day,month,year,hour,lat,lon,aod.

function [DataArray]=MCread261014_modified(Path_file)
clc
DataArray=zeros(10000,9);
DataArray(1,:)=1;

load LatLon_3

% define M (length of the rows from pathnames)
[M, N] =size(Path_file);
% define x,y from lat and lon file, its from LON since lat and long are
% same size
[X, Y] = size(lat_h20v05);
% tempLon = reshape(lat_h20v05,X*Y,1);
% tempLat = reshape(long_h20v05,X*Y,1);
% %tr and tc are the indexes that are clipped to the study area
% [tr,tc]=find(tempLon<=36 & tempLon>=34.1); %exclude data with Lon above 36 and below 34.1
% %final lat and lon
% LatN=tempLat(tr);
% LonN=tempLon(tr);


%define the length of i 1 to..M. for testing purpose we can change M to
%numbner IE 1:3
for I = 1:M
    
    % Import AOD,Lat,Lon Data from hdf files
    
    %read AOD from PATHNAME (the 1:M)
    %you can get the variable name and order from importing data (hdfimport
    %tool. then add the names to the next line at the end in qoutes
    temp1 = hdfread( Path_file{I}, 'MODIS_Grid_Daily_1km_LST','Fields','LST_Day_1km');
    temp2 = hdfread( Path_file{I}, 'MODIS_Grid_Daily_1km_LST','Fields','LST_Night_1km');
    temp3 = hdfread( Path_file{I}, 'MODIS_Grid_Daily_1km_LST','Fields','Emis_31');
    
    tile = Path_file{I}(58:63);
    if (tile == 'h20v05')
        Lat = lat_h20v05;
        Long = long_h20v05;
    end
    
    if (tile=='h21v05')
        Lat = lat_h21v05;
        Long = long_h21v05;
    end
    if (tile=='h21v06')
        Lat = lat_h21v06;
        Long = long_h21v06;
    end
   
    
    tempLon = reshape(Lat,X*Y,1);
    tempLat = reshape(Long,X*Y,1);
    %tr and tc are the indexes that are clipped to the study area
    [tr,tc]=find(tempLon<=36 & tempLon>=34.1); %exclude data with Lon above 36 and below 34.1
    %final lat and lon
    LatN=tempLat(tr);
    LonN=tempLon(tr);
    
    % AOD data matrix reshape, reshape 600x600 matrix into clipped area
    % matrix. the new matrix
    %size of temp which is the new size    % tempXn is the reshape of aod from matrix to vector
    %based on LatN(clipped lat vector)
    temp1n = reshape(temp1,X*Y,1);
    %this is where its clipped
    temp1n=temp1n(tr);
    %%ar and ac are the indexes that are clipped to AOD not fill value (-9.9990)
     DAY=double(temp1n);
%    AOD=AOD*0.001;
    %Lat,Lon (all NaN's excluded from AOD)
    Lat=LatN;
    Lon=LonN;
    
       %AOD QA
  temp2n = reshape(temp2,X*Y,1);
    temp2n=temp2n(tr);
     NIGHT=(temp2n);
    
    %AOD Uncertainty
    temp3n = reshape(temp3,X*Y,1);
    temp3n=temp3n(tr);
    EMIS=double(temp3n); 
%     UN=double(temp3n);
%     UN=UN*0.0001;
    
   %water vapor column 
%    temp4n = reshape(temp4,X*Y,1);
%     temp4n=temp4n(tr);
%     WV=double(temp4n);
%     WV=WV*0.001;
%     
    %Create table with date, lat,lon,aod and then add other variables.
    [r, c] = size(Lat);
    [r1,c1]=find(DataArray(:,1),1,'last');
    maxr1=max(r1);
    DataArray(maxr1+1,1)=maxr1;
    DataArray(maxr1+1:maxr1+r,5) = Lat;
    DataArray(maxr1+1:maxr1+r,6) = Lon;
    DataArray(maxr1+1:maxr1+r,7) = DAY;
    DataArray(maxr1+1:maxr1+r,8) = NIGHT;
    DataArray(maxr1+1:maxr1+r,9) = EMIS;
   % DataArray(maxr1+1:maxr1+r,10) = QA;

    
    %Convert Julian date to normal date DD-MM-YYY
    %extract the string from filename based on text position in filename
    %temp5 is the year variable
    temp5 = str2double( Path_file{I}(50:53) );
    %temp6 is the julian day of year
    temp6 = str2double( Path_file{I}(54:56) );
    [year,month, day]=julian2date(temp6,temp5);
    
    % Insert Date and Time data to Array
    
    DataArray(maxr1+1:maxr1+r,1) = day;
    DataArray(maxr1+1:maxr1+r,2) = month;
    DataArray(maxr1+1:maxr1+r,3) = year;
    %extract hour from filename as in above
    %hour = str2double( Path_file{I}(91:94) ); 
    DataArray(maxr1+1:maxr1+r,4) = 0;
    
    
end

 [row,col]=find(DataArray(:,7)~= -28.672); %exclude all days with AOD = NAN
    DataArray=DataArray(row,:);
    DataArray=DataArray(2:end,:);
    
    
%     QAbin=de2bi(DataArray(:,9),16); 
%     DataArray(:,10:25) = QAbin;  
