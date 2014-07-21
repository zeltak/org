%% tera
%change dir to path,
cd 'D:/TEMP_ISRAEL/MODIS_LST_IL';
clear

%import text file of Path names of hdf files and convert them to a cell
%array
PathNameA=readtable('PathLST2.txt'); % create another txt file with path in each computer.make sure the path is specified correctly
PathName=table2cell(PathNameA);
P=PathName;


%num=1:2:length(P); %exclude every second row wich includes tile '06'
%Pnew=P(num,1);
%P=Pnew;

%read hdf file
[M,N]=size(P); 
LST=cell(M,1);
for I=1:M
    LST{I}=hdfread (P{I},'MODIS_Grid_Daily_1km_LST','Fields','LST_Day_1km');% P - WHERE TO READ THE DATA FROM, NAME OF FILE, fields, name of field
end
%Add lat and long to each tile
load latlon.mat
Long=longh20v05(1:1440000,:); Long1=longh21v05(1:1440000,:); %1440000=1200*1200
Lat=lath20v05(1:1440000,:);Lat1=lath21v05(1:1440000,:);

%tr and tc are the indexes that are clipped to the study area
for J=1:M
    tile= P{J}(19:24);%count places in path name and change accordingly, prepare a text file for each machine.
    year=str2double( P{J}(11:14) );
    seqday=str2double( P{J}(15:17) );
    [year,month,day]=julian2date(seqday,year);
    
    % Insert Date and Time data to Array
    n=zeros(size(Lat,1),6);
    if (tile=='h20v05')
        
        [tr]=find(Long<=36 & Long>=34.1); %exclude data with Lon above 36 and below 34.1
        
        temp=(LST{J,1});
        %data matrix reshape
        [X Y] = size(temp);
        n(:,4)= double(reshape(temp,X*Y,1));
        n(:,5)=Lat;
        n(:,6)=Long;
        n(:,1)= repmat(year,1);
        n(:,2)=month;
        n(:,3)=day;
        n=n(tr,1:6);
        [tc]=find(n(:,5)<34);%exclude data with Lat above 34
        n=n(tc,1:6);
        n=array2table(n);
        n.Properties.VariableNames = {'Year','Month','Day','LST','Lat','Long'};
        LST{J,1}= n;
        n=[];
    end
    
    %from here on - need to find a solution so it will run on both tiles...
   % if (tile=='h21v05')
        
       % [tr]=find(Long1<=36 & Long1>=34.1); %exclude data with Lon above 36 and below 34.1
        
        %temp=(LST{J,1});
        %data matrix reshape
        %[X Y] = size(temp);
        %n (:,4)= double(reshape(temp,X*Y,1));
        %n(:,5)=Lat1;
        %n(:,6)=Long1;
        %n(:,1)= repmat(year,1);
        %n(:,2)=repmat(month,1);
        %n(:,3)=repmat(day,1);
        %n=n(tr,1:6);
        %[tc]=find(n(:,5)<34);%exclude data with Lat above 34
        %n=n(tc,1:6);
        %n=array2table(n);
        %n.Properties.VariableNames = {'Year','Month','Day','LST','Lat','Long'};
        %LST{J,1}= n;
        %n=[];
        
    %end
end
 
save('LST.mat','LST');