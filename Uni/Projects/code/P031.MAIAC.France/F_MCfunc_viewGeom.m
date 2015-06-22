%The function name that is called by MCrun to extract hdf files into a table with output of: day,month,year,hour,lat,lon,aod.

function [DataArray]=F_MCfunc_viewGeom(Path,latlon,Index)
clc
addpath('/media/NAS/Uni/org/files/Uni/Projects/code/C00Matlab/');


DataArray=zeros(10000,10);
DataArray(1,:)=1;

% define M (length of the rows from pathnames)
[M, N] =size(Path);
%define Tile

%define the length of i 1 to..M. for testing purpose we can change M to
%numbner IE 1:3
for K = 1:M
    
    % Import AOD,Lat,Lon Data from hdf files
    
    %read AOD from PATHNAME (the 1:M)
    %you can get the variable name and order from importing data (hdfimport
    %tool. then add the names to the next line at the end in qoutes
    %temp1 = hdfread( Path{K}, 'grid1km', 'Fields', 'Optical_Depth_Land' ); 
    temp1= hdfread( Path{K}, 'grid1km', 'Fields', 'Optical_Depth_Land' ); 
    temp2 = hdfread( Path{K}, 'grid5km', 'Fields', 'cosSZA' );[X,Y]=size(temp2);
    A=reshape(Index(temp2(:)), size(temp2)); temp1=temp1(A);
    temp3 = hdfread( Path{K}, 'grid5km', 'Fields', 'cosVZA' );
    temp4 = hdfread( Path{K}, 'grid5km', 'Fields', 'RelAZ' );
    temp5 = hdfread( Path{K}, 'grid5km', 'Fields', 'Scattering_Angle' );
    % AOD data matrix reshape, reshape 600x600 matrix into clipped area
    % matrix. the new matrix
    %size of temp which is the new size    % tempXn is the reshape of aod from matrix to vector
    %based on LatN(clipped lat vector)
    temp1n = reshape(temp1,X*Y,1);
    %this is where its clipped
    %temp1n=temp1n(index(:,1));
    %%ar and ac are the indexes that are clipped to AOD not fill value (-9.9990)
    AOD=double(temp1n);
    AOD=AOD*0.001;
    
    %View Geometry
    %long_name: cosine of Solar Zenith Angle 
    %scale_factor: 0.0001
    temp2n = reshape(temp2,X*Y,1);
    cosSZA=double(temp2n)*0.0001;
       
    %long_name: cosine of View Zenith Angle 
    %scale_factor: 0.0001
    temp3n = reshape(temp3,X*Y,1);
    cosVZA=double(temp3n)*0.0001;
    
    %long_name: Relative_azimuth (degrees)
    %scale_factor: 0.01
    temp4n = reshape(temp4,X*Y,1);
    RelAZ=double(temp4n)*0.01;
       
    %long_name: Scattering_Angle (degrees)
    %scale_factor: 0.01
    temp5n = reshape(temp5,X*Y,1);
    Scatt_Angl=double(temp5n)*0.01;

    
  
    
    %Create table with date, lat,lon,aod and then add other variables.
    
    
    %LATLON - 1km grid vs 5 km grid ???
    
    [r, c] = size(latlon);
    [r1,c1]=find(DataArray(:,1),1,'last');
    maxr1=max(r1);
    DataArray(maxr1+1,1)=maxr1;
    DataArray(maxr1+1:maxr1+r,[5 6]) = latlon;
    DataArray(maxr1+1:maxr1+r,7) = AOD;
    DataArray(maxr1+1:maxr1+r,8) = cosSZA;
    DataArray(maxr1+1:maxr1+r,9) = cosVZA;
    DataArray(maxr1+1:maxr1+r,10) = RelAZ;
    DataArray(maxr1+1:maxr1+r,11) = Scatt_Angl;
    
    %Convert Julian date to normal date DD-MM-YYY
    %extract the string from filename based on text position in filename
    str=char(regexp(Path{K},'[.]\d+[.]','match'));
    %temp4 is the year variable
    temp6= str2double(str(2:5)) ;
    %temp5 is the julian day of year
    temp7 =str2double( str(6:8 ));
    [year,month, day]=julian2date(temp7,temp6);
    
    % Insert Date and Time data to Array
    
    DataArray(maxr1+1:maxr1+r,1) = day;
    DataArray(maxr1+1:maxr1+r,2) = month;
    DataArray(maxr1+1:maxr1+r,3) = year;
    %extract hour from filename as in above
    hour = str2double(str(9:12)); DataArray(maxr1+1:maxr1+r,4) = hour;
    
 
    
end

 [row,col]=find(DataArray(:,7)~= -28.672); %exclude all days with AOD = NAN
    DataArray=DataArray(row,:);
    DataArray=DataArray(2:end,:);
    
    
    





