


%change dir to path
cd '/media/NAS/Uni/Data/Europe/france/ndvi_france/';
addpath('/media/NAS/Uni/org/files/Uni/Projects/code/C00Matlab/');
clear
%import text file of Phath names of NDVI hdf files
pathList = dir('/media/NAS/Uni/Data/Europe/france/ndvi_france/');
pathList = pathList(~[pathList.isdir]); %remove directories
[junk, sortorder] = sort([pathList.datenum]);
pathList = pathList(sortorder); %list is now in ascending date order
numfiles = numel(pathList);
P=pathList;
%Create subset by tile
%Tiles
T={'h17v04','h18v03','h18v04'}; %france
%T={'h18v01','h18v02','h18v03','h19v02','h19v03'}; %sweden

[M,N]=size(P); yes=0;sub={1};f=0;k=1;
for I=1:length(T)
    for JJ=1:M
        f=char(regexp(P(JJ).name,'h.....[.]','match'));
        yes=strcmp(f,[T{I},'.']);
        if yes==1
            c=struct2cell(P(JJ));
            sub{k,1}=c{1,1}; k=k+1;%
        end
    end

[MM,N]=size(sub);
X=1200; Y=1200;

%Add lat and long to each tile (output from hdfdump)
%import text file of Lat Long for NDVI tiles
cd '/media/NAS/Uni/Data/Europe/france/ndvi_france/latlon/';

tileLat=[T{I},'.lat.txt'];
lat=readtable(tileLat, 'ReadVariableNames', false);
lat=table2array(lat);
tileLong=[T{I},'.lon.txt'];
long=readtable(tileLong, 'ReadVariableNames', false);
long=table2array(long);
cord=[lat long];
    
    % clip per tile based in lat and lon (currently its setup for France)
    if I==1
        [r,c]=find(cord(:,1)<50.1 & cord(:,3)>-2.25);
        index=[r c];
        latlon=cord(r,:);
    elseif I==2
        [r,c]=find(cord(:,1)>42 & cord(:,3)>-5.5);
        index=[r c];
        latlon=cord(r,:);
    elseif I==3
        [r,c]=find(cord(:,1)<51.5 & cord(:,3)<6.3);
        index=[r c];
        latlon=cord(r,:);
    elseif I==4
        [r,c]=find(cord(:,1)>42.8 & cord(:,3)<9);
        index=[r c];
        latlon=cord(r,:);
    end
%read hdf file
NDVI=cell(1,5);
    cd ('/media/NAS/Uni/Data/Europe/france/ndvi_france/');
for J=1:MM
    hdf=hdfread (sub{J,1},'MOD_Grid_monthly_1km_VI','Fields','1 km monthly NDVI');
    hdf=double(reshape(hdf,X*Y,1));
    NDVI{J,3}= num2cell(hdf(r));
    %extract the string from filename based on text position in filename
    str=cellstr(regexp(sub{J,1},'[.A].......[.]','match'));
    %temp1 is the year variable
    temp1 = str2double(str{1,1}(2:5)) ;
    %temp2 is the julian day of year
    temp2 =str2double( str{1,1}(6:8 ));
    [year,month, day]=julian2date(temp2,temp1);
    NDVI{J,1}=year;
    NDVI{J,2}=month;
end



        %data matrix reshape
        new=zeros(5);n=zeros(5);
        temp=(NDVI{1,3});
        [xx yy] = size(temp);
        for v=1:length(NDVI)
        new=n;n=zeros(5);
        n(1:xx,3)= cell2mat(NDVI{v,3});
        n(:,3)=n(:,3)/10000;
        n(:,4)=lat(r,1);
        n(:,5)=long(r,1);
        n(:,1)= repmat(year,1);
        n(:,2)=month;
        n=[new ;n];
        end
        y=num2str(year); m=num2str(month);
        name=['NDVI_',T{I}];
ms  = n;
fid = fopen( ['/media/NAS/Uni/Data/Europe/france/ndvi_france/out/',name,'.csv'], 'w' );
fprintf( fid, '%s.%s.%s.%s.%s\n', 'Year','Month','NDVI','Lat','Long' );
%fprintf( fid, '%d,%d,%f,%f,%f\n', ms );
fprintf(fid, [repmat('%4f\t', 1, size(ms, 2)) '\n'], ms');
fclose( fid );  
      
        n=[];
    end   
    

