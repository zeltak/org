% %run aqua and terra HDF imports
% %THIS CODE HAS BEEN UPDATED ON 30.10.2014. IT UPLOADS THE PATHNAMES, LAT AND LON DATA, AND CALLS THE FUNCTION FOR EXTRACTING MAIAC HDF FILES
% % load files: Pathname, lat,lon
% 
% 
% 
% 
% %change dir to path
%cd '/media/NAS/Uni/org/files/Uni/Projects/code/P46/MAIAC';
addpath (genpath('/media/NAS/Uni/org/files/Uni/Projects/code/P045.Israel_LST/'))

clear
% %import text file of hdf files
PathName=readtable('/media/NAS/Uni/org/files/Uni/Projects/code/P045.Israel_LST/path.txt','ReadVariableNames',false); 

PathName=table2cell(PathName);

% %lat long hdf file
% lat=hdfread ('/media/NAS/Uni/Data/Israel/MAIAC_new_10_2014/h06v07/MAIACLatlon.h06v07.hdf','latlon','Fields','lat');
% lon=hdfread ('/media/NAS/Uni/Data/Israel/MAIAC_new_10_2014/h06v07/MAIACLatlon.h06v07.hdf','latlon','Fields','lon');

% %Read function MCread with output variable 'Table' that includes the
% %following columns:
% day,month,year,hour,lat,lon,AOD,AOD_QA,AOD_uncertainty,Water Vapor
% seperate tables for Aqua and for Terra


count=2;
Path=PathName(1,1); % loop for running several years
yr = str2double(PathName{1}(50:53));
while (count<(length(PathName)+1))
    while (yr == str2double(PathName{count}(50:53)))
        if (isempty(Path))
            countPath = 1;
        else
            countPath = (length(Path)+1);
        end    
        Path{countPath,1} = PathName{count};
        count = count+1;
        if (count == (length(PathName)+1))
            break;
        end
    end
    
   
    TableAq=MCread261014_modified(Path);
        

%     %save 'Table' as matlab variable (*.m)
    Years(yr) = {num2str(yr)};
    save (['/media/NAS/Uni/Data/Israel/MODIS_LST_IL/out/MAIACAqIsr_' Years{yr} '.mat'],'TableAq');
    
%     %save table as csv
filename=['/media/NAS/Uni/Data/Israel/MODIS_LST_IL/out/MAIACAqIsr_' Years{yr} '.csv'];
headers={'Day','Month','Year','Hour','Lat','Lon','DAY','NIGHT','EMIS'};
m=TableAq;
csvwrite_with_headers(filename,m,headers)

    
% clears TableA
if (count == 25)
    break;
end
TableAq=[];
yr = str2double(PathName{count,1}(50:53));
Path = [];
    
end


