clear
clc
%addpath where all general matlab scripts are (e.g. julian2date) so that
%matlab loads to memory
addpath('/media/NAS/Uni/org/files/Uni/Projects/code/C00Matlab/');
% add where this code resides alongsode Function code which is needed
cd ('/media/NAS/Uni/org/files/Uni/Projects/code/P031.MAIAC.France/final/nonR/matlab');
%
% a loop that will create a list of all files recusivly under selected
% tiles
tiles={'h00v01'; 'h00v02'; 'h01v01'; 'h01v02'};

for T=1:size(tiles,1)
    %path to hdf files of BRF
    cd ('/media/NAS/Uni/Data/MV3/dataportal.nccs.nasa.gov/DataRelease/Europe/');
    %insert Tile names here manually per area required
    tiles={'h00v01'; 'h00v02'; 'h01v01'; 'h01v02'};
    %name of current tile in loop
    name=tiles{T};
    %name of folder to open for hdf files per tile)
    folderName=['/media/NAS/Uni/Data/MV3/dataportal.nccs.nasa.gov/DataRelease/Europe/',name];
    %file list is list of all files
    fileList = getAllFiles(folderName);                                                                                                                                                                                                           
    %numfiles is the number of files in file list
    numfiles = numel(fileList);
    %lat long hdf file by tile
    %define path to where relevant lat and long files reside
    XYFile=['/media/NAS/Uni/Data/MV3/MAIACLatlon.', name, '.hdf'];
    lat=hdfread(XYFile, 'latlon','Fields','lat');[X, Y] = size(lat);
    long=hdfread(XYFile,'latlon','Fields','lon');
    %reshape from (1200,1200) matrix to a (1200*1200,1) vector
    lat1 = reshape(lat,X*Y,1);long1 = reshape(long,X*Y,1);
    % 'cbind' the 2 vectors relevant to this tile
    cord=[lat1 long1];
    %
    % clip per tile based in lat and lon (currently its setup for France)
    if T==1
        [r,c]=find(cord(:,1)<50.1 & cord(:,2)>-2.25);
        index=[r c];
        latlon=cord(r,:);
    elseif T==2
        [r,c]=find(cord(:,1)>42 & cord(:,2)>-5.5);
        index=[r c];
        latlon=cord(r,:);
    elseif T==3
        [r,c]=find(cord(:,1)<51.5 & cord(:,2)<6.3);
        index=[r c];
        latlon=cord(r,:);
    elseif T==4
        [r,c]=find(cord(:,1)>42.8 & cord(:,2)<9);
        index=[r c];
        latlon=cord(r,:);
    end
    %
    %Read function MCread with output variable 'Table' that includes the following columns:
    %day,month,year,hour,lat,lon,SR,
    %seperate tables for Aqua and for Terra
    %
    %define fileLists for Aqua and for Terra in seperate variables
    k=1;w=1;
    for I=1:size(fileList,1) %size(obj,1) returns the number of rows.
        if strcmp(regexp(fileList{I},'ABRF','match'),'ABRF')
            PAq{k,1}=fileList{I}; k=k+1; %fileLists for Aqua AOT
        elseif strcmp(regexp(fileList{I},'TBRF','match'),'TBRF')
            PTr{w,1}=fileList{I}; w=w+1; %fileLists for Terra AOT
            %name=[];
        end
    end
    %
    %%%%AQUA%%%%%
    % loop for running several years
    % insert relevant years manualy
    f=1;Path=PAq(1,1);
    YY=num2cell(2003:2013);
    for W=1:length(YY)
        Path={0};kk=1;
        for II=1:length(PAq)
            if strcmp(regexp(PAq{II},['/',num2str(YY{W}),'/'],'match'),['/',num2str(YY{W}),'/'])==1;
                Path{kk,1}=PAq{II}; kk=kk+1;
            end
            
        end
        %
        Years=num2str(YY{W});
        %
        % add where this code resides alongsode Function code which is needed
        cd ('/media/NAS/Uni/org/files/Uni/Projects/code/P031.MAIAC.France/final/nonR/matlab');
        %
        % read the the BRF_SR function , we send the differnt indexes and it
        % returns a variable called TableAq (aqua) with lat,long,aod date
        TableAq=BRF_SR(Path,latlon,index);
        %
        %save table as csv: put the path of the code for exporting to csv
        %files- we currently disable this
        filename=['/media/NAS/Uni/Data/MV3/Out/MAIAC_SuRfl_Aq_',name,'.',Years, '.csv'];
        headers={'Day','Month','Year','Hour','Lat','Lon','SuRef','band'}; 
        m=TableAq;
        csvwrite_with_headers(filename, m, headers)
        %save to mat files
        
        %filename=['/media/NAS/Uni/Data/MV3/Out/MAIAC_Aq.', name, '.',Years, '.mat'];
        %headers={'Day','Month','Year','Hour','Lat','Lon','AOD','UN','QA'}; %In R we have a better code for extracting the QA flag
        %save (filename,'TableAq')
        
        %clears TableA
        TableAq=[];
    end
    %     %   Terra   %
    %     % loop for running several years
    %     % insert relevant years manualy
    %     f=1;Path=PTr(1,1);
    %
    %     Y=num2cell(2000:2013);
    %     for W=1:length(Y)
    %         Path={0};kk=1;
    %         for II=1:length(PTr)
    %             if strcmp(regexp(PTr{II},['/',num2str(Y{W}),'/'],'match'),['/',num2str(Y{W}),'/'])==1;
    %                 Path{kk,1}=PTr{II}; kk=kk+1;
    %             end
    %
    %         end
    %
    %         Years=num2str(Y{W});
    %
    %         % add where this code resides alongsode Function code which is needed
    %         cd ('/media/NAS/Uni/org/files/Uni/Projects/code/P031.MAIAC.France/');
    %
    %         % read the the MCfun function , we send the differnt indexes and it
    %         % returns a variable called TableTr (Trua) with lat,long,aod date
    %         TableTr=F_MCfunc_france(Path,latlon,index);
    %
    %         %save table as csv: put the path of the code for exporting to csv
    %         %files- we currently disable this
    %         %filename=['/media/NAS/Uni/Data/MV3/Out/MAIAC_Tr.', name, '.',Years, '.csv'];
    %         %headers={'Day','Month','Year','Hour','Lat','Lon','AOD','UN','QA'}; %In R we have a better code for extracting the QA flag
    %         % m=TableTr;
    %         %csvwrite_with_headers(filename, m, headers)
    %         %save to mat files
    %
    %         filename=['/media/NAS/Uni/Data/MV3/Out/MAIAC_Tr.', name, '.',Years, '.mat'];
    %         %headers={'Day','Month','Year','Hour','Lat','Lon','AOD','UN','QA'}; %In R we have a better code for extracting the QA flag
    %         save (filename,'TableTr')
    %
    %
    %         %clears TableA
    %         TableTr=[];
    %
    %     end
    clear
end
disp(' end')