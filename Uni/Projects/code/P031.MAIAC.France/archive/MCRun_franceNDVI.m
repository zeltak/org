% this script does not need a func file and its self contained!

%change dir to path
% general path to helper matlab codes (e.g. julian2date)
addpath ('/media/NAS/Uni/org/files/Uni/Projects/code/C00Matlab/');

clear

%import text files of Path names of NDVI hdf files
%lins 33   path to the directory to the ndvi.hdf files
pathList = dir('/media/NAS/Uni/Data/Europe/france/ndvi_france/');
pathList = pathList(~[pathList.isdir]);
[junk, sortorder] = sort([pathList.datenum]);
pathList = pathList(sortorder); %list of hdf filenames
numfiles = numel(pathList); %number of hdf files


%Tiles
T={'h17v03','h17v04','h18v03','h18v04'}; %france

NDVI={1,1,1,1,1,1}; %preallocate NDVI array

%loop on tiles

%for I=1:length(T)
    %loop on hdf files by pathlist
    %lins 33   path to the directory to the ndvi.hdf files
%Tile index
I=1;
tic
    for ii = 1:numfiles
        %read hdf file
        cd '/media/NAS/Uni/Data/Europe/france/ndvi_france/';
        hdf=hdfread (pathList(ii).name,'MOD_Grid_monthly_1km_VI','Fields','1 km monthly NDVI');%NDVI
        X=size(hdf,1); Y=size(hdf,2);
        %reshape Nvi data from (X,Y) matrix to (X*Y,1) vector
        NDVI{ii,3}= num2cell(double(reshape(hdf,X*Y,1)));
        
        %extract the string from filename based on text position in filename
        str=cellstr(regexp(pathList(ii).name,'[.A].......[.]','match'));
        %temp1 is the year variable
        temp1 = str2double(str{1,1}(2:5)) ;
        %temp2 is the julian day of year
        temp2 =str2double( str{1,1}(6:8 ));
        [year,month, day]=julian2date(temp2,temp1);
        NDVI{ii,1}=year;
        NDVI{ii,2}=month;
        
        %add tilename to each row
        ind=regexp(pathList(ii).name,'h.....','match');
        
        if strcmp(ind,T{I})==1
            NDVI{ii,4}=T{I};
        end
   
    %Add lat and long to each tile (output from hdfdump)
    cd '/media/NAS/Uni/Data/Europe/france/ndvi_france/latlon/';
    tileLat=[T{I},'.lat'];
    fid = fopen(tileLat);
    lat = textscan(fid, '%f', 'delimiter', ',');
    fclose(fid);
    
    tileLong=[T{I},'.lon'];
    fid = fopen(tileLong);
    lon = textscan(fid, '%f', 'delimiter', ',');
    fclose(fid);
    
     if strcmp(NDVI{ii,4},T{I})==1
    NDVI(ii,5)=lat(:,1);
    NDVI(ii,6)=lon(:,1);
     end
    end
    toc
    end
                                                                                                                                                                                                                                                                        





%save NDVI array table
save ('NDVI_n.mat','NDVI','-v7.3') ;


% clear
% load NDVI.mat
%data matrix reshape to one big table
ndvi=zeros(1,6);
for J=1:length(NDVI)
    n=zeros(1,6);temp=ndvi;
    n(1:(X*Y),3)= cell2mat(NDVI{J,3});
    n(:,3)=n(:,3)/10000;
    n(1:(X*Y),4)=cell2mat(NDVI{J,5});
    n(1:(X*Y),5)=cell2mat(NDVI{J,6});
    n(:,1)= repmat(year,1);
    n(:,2)=month;
    ndvi=[temp;n];
end                                                                                                                                                                                                                                                                                                                                                                                      





y=num2str(year); m=num2str(month);
name=['NDVI.',y,'.',m,'.',tile];
save -v7.3 ('NDVI_n.mat','ndvi');

ms  = ndvi;
fid = fopen( ['C:\Users\MEYTAR\Documents\MATLAB1\NDVI\sweden\',name,'.csv'], 'w' );
fprintf( fid, '%s.%s.%s.%s.%s\n', 'Year','Month','NDVI','Lat','Long' );
%fprintf( fid, '%d,%d,%f,%f,%f\n', ms );
fprintf(fid, [repmat('%4f\t', 1, size(ms, 2)) '\n'], ms');
fclose( fid );

n=[];
end
%                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 