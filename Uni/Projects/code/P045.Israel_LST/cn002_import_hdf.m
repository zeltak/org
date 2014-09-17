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

%num=1:2:length(P); %exclude every second row wich includes tile '06'
%Pnew=P(num,1);
%P=Pnew;

%read hdf file
[M,N]=size(P); 
LST=cell(M,3);
for I=1:M
    LST{I,1}=hdfread (P{I},'MODIS_Grid_Daily_1km_LST','Fields','LST_Day_1km');% P - WHERE TO READ THE DATA FROM, NAME OF FILE, fields, name of field
    LST{I,2}=hdfread (P{I},'MODIS_Grid_Daily_1km_LST','Fields','LST_Night_1km');
    LST{I,3}=hdfread (P{I},'MODIS_Grid_Daily_1km_LST','Fields','Emis_31');
end
%Add lat and long to each tile
load /media/NAS/Uni/Data/code library/matlab/LatLon_3.mat

%tr and tc are the indexes that are clipped to the study area
for J=1:M
    tile= P{J}(58:63);% (19:24) count places in path name and change accordingly, prepare a text file for each machine.
    year=str2double( P{J}(50:53) ); % year (11:14)
    seqday=str2double( P{J}(54:56) ); % sequential day (15:17)
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
    n=array2table(n);
    n.Properties.VariableNames = {'Year','Month','Day','LST_Day_1km','LST_Night_1km','Emis_31','Lat','Long'};
    LST2{J,1}= n;
    n=[];
end

LST = [LST2{1,1}];
for i=2:size(LST2,1)
    if (LST2{i-1,1}{1,1}==LST2{i,1}{1,1})
        LST = [LST ; LST2{i,1}];
    else
        name = num2str(LST2{i-1,1}{1,1});
        writetable(LST,['/media/NAS/Uni/Data/Israel/MODIS_LST_IL/out/' name],'Delimiter','tab');
        save(['/media/NAS/Uni/Data/Israel/MODIS_LST_IL/out/' name],'LST');
        LST = [];
    end
end
name = num2str(LST2{i,1}{1,1});
writetable(LST,['/media/NAS/Uni/Data/Israel/MODIS_LST_IL/out/' name],'Delimiter','tab');
save(['/media/NAS/Uni/Data/Israel/MODIS_LST_IL/out/' name],'LST');

%save('LST.mat','LST2');