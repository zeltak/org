% %run aqua and terra HDF imports
% %THIS CODE HAS BEEN UPDATED ON 26.10.2014. IT UPLOADS THE PATHNAMES, LAT AND LON DATA, AND CALLS THE FUNCTION FOR EXTRACTING MAIAC HDF FILES
% % load files: Pathname, lat,lon
% 
% 
% 
% 
% %change dir to path
%cd '/media/NAS/Uni/org/files/Uni/Projects/code/P46/MAIAC';
clear
% %import text file of hdf files
PathName=readtable('C:\Users\MEYTAR\Documents\MATLAB1\MAIAC\readata\PathName.txt');
PathName=table2cell(PathName);

k=1;w=1;
for I=1:size(PathName,1)
    name = PathName{I}(57:60) ;
    if name=='AAOT'
        PAq{k,1}=PathName{I}; k=k+1; %Pathnames for Aqua AOT
    elseif name=='TAOT'
        PTr{w,1}=PathName{I}; w=w+1; %Pathnames for Terra AOT
        name=[];
    end
end

% %lat long hdf file
lat=hdfread ('C:\Users\MEYTAR\Documents\MATLAB1\MAIAC\readata\MAIACLatlon.h06v07.hdf','latlon','Fields','lat');
lon=hdfread ('C:\Users\MEYTAR\Documents\MATLAB1\MAIAC\readata\MAIACLatlon.h06v07.hdf','latlon','Fields','lon');

% %Read function MCread with output variable 'Table' that includes the
% %following columns:
% day,month,year,hour,lat,lon,AOD,AOD_QA,AOD_uncertainty,Water Vapor
% seperate tables for Aqua and for Terra
%% Aqua %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% f=1;Path=PathName(1,1); % loop for running several years
% for Y=2002:2014
%     for II=1:length(PAq)
%         yr=str2double( PAq{II}(69:72) );
%         if (yr~=Y || II==length(PAq))
%             if II==length(PAq)
%                 Path=PAq(f:II);
%             else Path=PAq(f:II-1);
%             end
%             Path=PAq(II:length(PAq));
%             f=1;
%             break
%         end
%         
%     end
%     
     Path=PAq; 
     TableAq=MCread261014(Path,lat,lon);
    
%     %save 'Table' as matlab variable (*.m)
    Years(Y) = {num2str(Y)};
    save (['C:\Users\MEYTAR\Documents\MATLAB1\MAIAC\readata\Data\MAIACAqIsr_' Years{Y}],'TableAq');
    
%     %save table as csv
filename=['C:\Users\MEYTAR\Documents\MATLAB1\MAIAC\readata\Data\MAIACAqIsr_' Years{Y} '.csv'];
headers={'Day','Month','Year','Hour','Lat','Lon','AOD','UN','WV','QA1','QA2','QA3','QA4','QA5','QA6','QA7','QA8','QA9','QA10','QA11','QA12','QA13','QA14','QA15'};
m=TableAq;
csvwrite_with_headers(filename,m,headers)

    
% clears TableA
TableA=[];
    
%end



%% 
%%%% Terra %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% f=1;Path=PathName(1,1); % loop for running several years
% for Y=2002:2014
%     for II=1:length(PAq)
%         yr=str2double( PAq{II}(69:72) );
%         if (yr~=Y || II==length(PAq))
%             if II==length(PAq)
%                 Path=PAq(f:II);
%             else Path=PAq(f:II-1);
%             end
%             Path=PAq(II:length(PAq));
%             f=1;
%             break
%         end
%         
%     end
%     
     Y=2012; %insert the year one is working on
     Path=PTr; 
     TableTr=MCread261014(Path,lat,lon);
    
%     %save 'Table' as matlab variable (*.m)
    Years(Y) = {num2str(Y)};
    save (['C:\Users\MEYTAR\Documents\MATLAB1\MAIAC\readata\Data\MAIACTrIsr_' Years{Y}],'TableTr');
    
%     %save table as csv
filename=['C:\Users\MEYTAR\Documents\MATLAB1\MAIAC\readata\Data\MAIACTrIsr_' Years{Y} '.csv'];
headers={'Day','Month','Year','Hour','Lat','Lon','AOD','UN','WV','QA1','QA2','QA3','QA4','QA5','QA6','QA7','QA8','QA9','QA10','QA11','QA12','QA13','QA14','QA15'};
m=TableTr;
csvwrite_with_headers(filename,m,headers)

    
% clears TableA
%TableTr=[];
    
%end

 




