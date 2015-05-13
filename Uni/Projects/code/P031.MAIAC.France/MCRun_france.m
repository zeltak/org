
%list of relevant tiles
tiles={'h00v01'; 'h00v02'; 'h01v01'; 'h01v02'};

%import text file of hdf files (List of the full paths of HDF files including year subdirectories)
for I=1:size(tiles,1)
name=tiles{I};

    TableName=['/media/NAS/Uni/Data/MV3/path.' , name , '.txt'];


PathName=readtable(TableName, 'ReadVariableNames', false);
PathName=table2cell(PathName);
PathName=sort(PathName); %Need to be sort by year, unless weird results

%define pathnames for Aqua and for Terra in seperate variables
k=1;w=1;
for I=1:size(PathName,1) %size(obj,1) returns the number of rows.
    if strcmp(regexp(PathName{I},'AAOT','match'),'AAOT')
        PAq{k,1}=PathName{I}; k=k+1; %Pathnames for Aqua AOT
    elseif strcmp(regexp(PathName{I},'TAOT','match'),'TAOT')
        PTr{w,1}=PathName{I}; w=w+1; %Pathnames for Terra AOT
        name=[];
    end
end


%lat long hdf file by tile

XYFile=['/media/NAS/Uni/Data/MV3/MAIACLatlon.', name, '.hdf'];


lat=hdfread(XYFile, 'latlon','Fields','lat');[X, Y] = size(lat);
long=hdfread(XYFile,'latlon','Fields','lon');
lat1 = reshape(lat,X*Y,1);long1 = reshape(long,X*Y,1);
cord=[lat1 long1];

if I==1
    [r,c]=find(cord(:,1)<50.1 & cord(:,2)<-2.25);
index=[r c];
latlon=cord(r,:);
elseif I==2
    [r,c]=find(cord(:,1)>42 & cord(:,2)>-5.5);
index=[r c];
latlon=cord(r,:);
elseif I==3
    [r,c]=find(cord(:,1)<51.5 & cord(:,2)<6.3);
   index=[r c];
latlon=cord(r,:);
elseif I==4
    [r,c]=find(cord(:,1)>42.8 & cord(:,2)<9);
index=[r c];
latlon=cord(r,:);
end

end
%Read function MCread with output variable 'Table' that includes the following columns:
%day,month,year,hour,lat,lon,AOD,AOD_QA,AOD_uncertainty,
%seperate tables for Aqua and for Terra

%   AQUA   %
f=1;Path=PAq(1,1); % loop for running several years
Y=num2cell(2004:2004);%check years!
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
    
    Years=num2str(Y{I});
    Sat='Aqua';
    
    TableAq=MCread_france(Path,latlon,index);
    
    %save table as csv: put the path of the code for exporting to csv files
  
    filename=['/media/NAS/Uni/Data/MV3/Out/', TileName, '/MAIAC_Aq_', Years, '.csv'];
    headers={'Day','Month','Year','Hour','Lat','Lon','AOD','UN','QA'}; %In R we have a better code for extracting the QA flag
   
    m=TableAq;
    csvwrite_with_headers(filename, m, headers)
    
    %clears TableA
    TableAq=[];
end
disp('Aqua end')
%%
for tile=1:length(tiles)
    %M    TileName=celldata(tile);
    TileName=tiles{tile};
    
    %import text file of hdf files (List of the full paths of HDF files including year subdirectories)
    %M    TableName=strcat('/home/microway/Desktop/Mihye/MATLAB Codes/PathName_', TileName, '.txt');
    
    TableName=['/home/microway/Desktop/Mihye/MATLAB Codes/PathName_' , TileName , '.txt'];
    
    % %    TableName=TableName{1};
    
    PathName=readtable(TableName, 'ReadVariableNames', false);
    PathName=table2cell(PathName);
    PathName=sort(PathName); %Need to be sort by year, unless weird results
    
    %define pathnames for Aqua and for Terra in seperate variables
    k=1;w=1;
    for I=1:size(PathName,1) %size(obj,1) returns the number of rows.
        if strcmp(regexp(PathName{I},'AAOT','match'),'AAOT')
            PAq{k,1}=PathName{I}; k=k+1; %Pathnames for Aqua AOT
        elseif strcmp(regexp(PathName{I},'TAOT','match'),'TAOT')
            PTr{w,1}=PathName{I}; w=w+1; %Pathnames for Terra AOT
            name=[];
        end
    end
    
    %lat long hdf file by tile
    %M XYFile=strcat('/data/Mihye/Drobo/MAIAC_NEW/MAIACLatlon.', TileName, '.hdf');
    XYFile=['/data/Mihye/Drobo/MAIAC_NEW/MAIACLatlon.', TileName, '.hdf'];
    
    %M XYFile=XYFile{1};
    lat=hdfread(XYFile, 'latlon','Fields','lat');
    lon=hdfread(XYFile,'latlon','Fields','lon');
    
    %Read function MCread with output variable 'Table' that includes the following columns:
    %day,month,year,hour,lat,lon,AOD,AOD_QA,AOD_uncertainty,Water Vapor
    %seperate tables for Aqua and for Terra
    
    
    %   TERAA   %
    f=1;Path=PTr(1,1); % loop for running several years
    Y=num2cell(2000:2014);
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
        
        %TableT=MCfunc(Path,lat,lon);
        Years=num2str(Y{I});
        Sat='Terra';
        TableT=MCfunc_Tile_Loop(Path, lat,lon); %changed as before
        
        %save table as csv
        filename=['/data/Mihye/Drobo/MAIAC_NEW/dataportal.nccs.nasa.gov/DataRelease/NorthAmerica/results_Terra_Aqua/', TileName, '/MAIAC_Tr_', Years, '.csv'];
        % filename=filename{1};
        headers={'Day','Month','Year','Hour','Lat','Lon','AOD','UN','WV','QA'};
        m=TableT;
        csvwrite_with_headers(filename,m,headers)
        
        %clears TableT
        TableT=[];
    end
end
disp('Terra end')
