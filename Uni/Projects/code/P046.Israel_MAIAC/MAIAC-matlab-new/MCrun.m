% %run aqua and terra HDF imports
% %THIS CODE HAS BEEN UPDATED ON 26.10.2014. IT UPLOADS THE PATHNAMES, LAT AND LON DATA, AND CALLS THE FUNCTION FOR EXTRACTING MAIAC HDF FILES
% % load files: Pathname, lat,lon

% %change dir to path
%cd '/media/NAS/Uni/org/files/Uni/Projects/code/P46/MAIAC';
clc
clear
addpath (genpath('/media/NAS/Uni/org/files/Uni/Projects/code/$Matlab/'))
addpath (genpath('/media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/MAIAC-matlab-new/'))


 %import text file of hdf files
PathName=readtable('/media/NAS/Uni/Data/Israel/MAIAC_new_10_2014/h06v07/PathName.txt','ReadVariableNames',false);
PathName=table2cell(PathName);

% define pathnames for Aqua and for Terra in seperate variables
k=1;w=1;
for I=1:size(PathName,1)
    if strcmp(regexp(PathName{I},'AAOT','match'),'AAOT')
        PAq{k,1}=PathName{I}; k=k+1; %Pathnames for Aqua AOT
    elseif strcmp(regexp(PathName{I},'TAOT','match'),'TAOT')
        PTr{w,1}=PathName{I}; w=w+1; %Pathnames for Terra AOT
        name=[];
    end
end

%lat long hdf file
lat=hdfread ('/media/NAS/Uni/Data/Israel/MAIAC_new_10_2014/h06v07/MAIACLatlon.h06v07.hdf','latlon','Fields','lat');
lon=hdfread ('/media/NAS/Uni/Data/Israel/MAIAC_new_10_2014/h06v07/MAIACLatlon.h06v07.hdf','latlon','Fields','lon');

% %Read function MCread with output variable 'Table' that includes the
% %following columns:
% day,month,year,hour,lat,lon,AOD,AOD_QA,AOD_uncertainty,Water Vapor
% seperate tables for Aqua and for Terra

%% Aqua %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f=1;Path=PAq(1,1); % loop for running several years
Y=num2cell(2002:2013);
for I=1:length(Y)
    for II=1:length(PAq)
        if strcmp(regexp(PAq{II},num2str(Y{I}),'match','once'),num2str(Y{I}))==0;
            if II==length(PAq)
                Path=PAq(f:II);
            else Path=PAq(f:II-1);
            end
            PAq=PAq(II:length(PAq));
            f=1;
            break
        end
        
    end
   

     TableA=MCfunc(Path,lat,lon);
    
  %save 'Table' as matlab variable (*.m)
    Years = num2str(Y{I});
    save (['/media/NAS/Uni/Data/Israel/MAIAC_new_10_2014/output/MAIACAqIsr_' Years ],'TableA','-v7.3');
    
    %save table as csv
    filename=['/media/NAS/Uni/Data/Israel/MAIAC_new_10_2014/output/MAIACAqIsr_' Years '.csv'];
headers={'Day','Month','Year','Hour','Lat','Lon','AOD','UN','WV','QA','QA0','QA1','QA2','QA3','QA4','QA5','QA6','QA7','QA8','QA9','QA10','QA11','QA12','QA13','QA14','QA15'};
m=TableA;
csvwrite_with_headers(filename,m,headers)

    
% clears TableA
TableA=[]; 
    
end



%% tera %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f=1;Path=PTr(1,1); % loop for running several years
Y=num2cell(2000:2013);
for I=1:length(Y)
    for II=1:length(PTr)
        if strcmp(regexp(PTr{II},num2str(Y{I}),'match','once'),num2str(Y{I}))==0;
            if II==length(PTr)
                Path=PTr(f:II);
            else Path=PTr(f:II-1);
            end
            PTr=PTr(II:length(PTr));
            f=1;
            break
        end
        
    end
   

     TableT=MCfunc(Path,lat,lon);
    
  %save 'Table' as matlab variable (*.m)
    Years = num2str(Y{I});
    save (['/media/NAS/Uni/Data/Israel/MAIAC_new_10_2014/output/MAIACTrIsr_' Years ],'TableT','-v7.3');
    
    %save table as csv
    filename=['/media/NAS/Uni/Data/Israel/MAIAC_new_10_2014/output/MAIACTrIsr_' Years '.csv'];
headers={'Day','Month','Year','Hour','Lat','Lon','AOD','UN','WV','QA','QA0','QA1','QA2','QA3','QA4','QA5','QA6','QA7','QA8','QA9','QA10','QA11','QA12','QA13','QA14','QA15'};
m=TableT;
csvwrite_with_headers(filename,m,headers)

    
% clears TableT
TableT=[]; 
    
end


