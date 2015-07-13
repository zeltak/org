%The function name that is called by MCrun to extract hdf files into a table with output of: day,month,year,hour,lat,lon,aod.

function [DataArray]=F_MCfunc_france(Path,latlon,index)
clc
addpath('/media/NAS/Uni/org/files/Uni/Projects/code/C00Matlab/');


DataArray=zeros(10000,9);
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
    temp1 = hdfread( Path{K}, 'grid1km', 'Fields', 'Optical_Depth_Land' ); [X,Y]=size(temp1);
    temp2 = hdfread( Path{K}, 'grid1km', 'Fields', 'AOT_Uncertainty' );
    temp3 = hdfread( Path{K}, 'grid1km', 'Fields', 'AOT_QA' );

    % AOD data matrix reshape, reshape 600x600 matrix into clipped area
    % matrix. the new matrix
    %size of temp which is the new size    % tempXn is the reshape of aod from matrix to vector
    %based on LatN(clipped lat vector)
    temp1n = reshape(temp1,X*Y,1);
    %this is where its clipped
    temp1n=temp1n(index(:,1));
    %%ar and ac are the indexes that are clipped to AOD not fill value (-9.9990)
    AOD=double(temp1n);
    AOD=AOD*0.001;
    
    %AOD Uncertainty
    temp2n = reshape(temp2,X*Y,1);
    temp2n=temp2n(index(:,1));
    UN=double(temp2n);
    UN=UN*0.0001;     
    
    %AOD QA
    temp3n = reshape(temp3,X*Y,1);
    temp3n=temp3n(index(:,1));
    QA=(temp3n);
    

    
  
    
    %Create table with date, lat,lon,aod and then add other variables.
    [r, c] = size(latlon);
    [r1,c1]=find(DataArray(:,1),1,'last');
    maxr1=max(r1);
    DataArray(maxr1+1,1)=maxr1;
    DataArray(maxr1+1:maxr1+r,[5 6]) = latlon;
%     DataArray(maxr1+1:maxr1+r,6) = Lon;
    DataArray(maxr1+1:maxr1+r,7) = AOD;
    DataArray(maxr1+1:maxr1+r,8) = UN;
    DataArray(maxr1+1:maxr1+r,9) = QA;
    
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

 [row,col]=find(DataArray(:,7)~= -28.672); %exclude all days with AOD = NAN
    DataArray=DataArray(row,:);
    DataArray=DataArray(2:end,:);
    
    
    





