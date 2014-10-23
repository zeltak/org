%% tera
%change dir to path,
cd '/media/NAS/Uni/org/files/Uni/Projects/code/P045.Israel_LST/';
clear

%import text file of Path names of hdf files and convert them to a cell
%array
PathNameA=textscan(fopen('path.txt'),'%s');; % create another txt file with path in each computer.make sure the path is specified correctly
PathName=PathNameA{:};
P=PathName;

clear PathNameA;


%read hdf file
[M,N]=size(P); 
LST=cell(M,3);
for I=1:M
    LST{I,1}=hdfread (P{I},'MODIS_Grid_Daily_1km_LST','Fields','LST_Day_1km');% P - WHERE TO READ THE DATA FROM, NAME OF FILE, fields, name of field
    LST{I,2}=hdfread (P{I},'MODIS_Grid_Daily_1km_LST','Fields','LST_Night_1km');
    LST{I,3}=hdfread (P{I},'MODIS_Grid_Daily_1km_LST','Fields','Emis_31');
end
%Add lat and long to each tile
load LatLon_3.mat

%tr and tc are the indexes that are clipped to the study area
LST2{M,1}=0;
for J=1:M
    tile= P{J}(37:42);% (19:24) count places in path name and change accordingly, prepare a text file for each machine.
    year=str2double( P{J}(48:51) ); % year (11:14)
    seqday=str2double( P{J}(52:54) ); % sequential day (15:17)
    [year,month,day]=julian2date(seqday,year);
    
    % Insert Date and Time data to Array
    n=zeros(size(lat_h20v05,1),8);
    if (tile=='h20v05')
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
    
    [tr]=find(Long<=36 & Long>=34.1); %exclude data with Lon above 36 and below 34.1
        
    temp=(LST{J,1});
    %data matrix reshape
    [X Y] = size(temp);
    n(:,4)= double(reshape(temp,X*Y,1));
    n(:,5)= double(reshape((LST{J,2}),X*Y,1));
    n(:,6)= double(reshape((LST{J,3}),X*Y,1));
    n(:,7)=Lat;
    n(:,8)=Long;
    n(:,1)= repmat(year,1); %year
    n(:,2)=month; %month
    n(:,3)=day; %day
    n=n(tr,1:8);
    [tc]=find(n(:,7)<34);%exclude data with Lat above 34
    n=n(tc,1:8);
    LST2{J,1}= n;
    
end

LST2 = cell2mat(LST2);

i=1;
while i<size(LST2,1)
    year = LST2(i,1);
    newMat = LST2(find(LST2==year),:);
    i = i + length(newMat);
    n=array2table(newMat);
    n.Properties.VariableNames = {'Year','Month','Day','LST_Day_1km','LST_Night_1km','Emis_31','Lat','Long'};
    save(['/media/NAS/Uni/Data/Israel/MODIS_LST_IL/out/' num2str(year)],'newMat');
    %writetable(n,['D:\itai\output_try\' num2str(year)],'Delimiter','tab');
end
    

