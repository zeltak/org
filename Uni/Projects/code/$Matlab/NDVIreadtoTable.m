%open NDVI HDF imports
%THIS CODE HAS BEEN UPDATED ON 27.4.2014. IT UPLOADS THE PATHNAMES, NDVI
%DATA, LAT AND LONG DATA, AND CREATES A TABLE OF: YEAR, MONTH, NDVI VALUE,
%LAT LONG


%%
%change dir to path
cd '/media/NAS/Uni/Data/Israel/NDVI/';
clear
%import text file of Phath names of NDVI hdf files
fileID=fopen('PathName.txt');
PathNameA=textscan(fileID,'%s');
fclose(fileID);
PathName=PathNameA{1,1};
P=PathName;

num=1:2:length(P); %exclude every second row wich includes tile '06'
Pnew=P(num,1);
P=Pnew;
    
    
    %read hdf file
[M,N]=size(P); NDVI=cell(M,1);
for I=1:M
    NDVI{I}=hdfread (PathName{I},'MOD_Grid_monthly_1km_VI','Fields','1 km monthly NDVI');
end

%Add lat and long to each tile
load latlonNDVI.mat
Long=longh20v05; Long1=longh21v05;
Lat=lath20v05;Lat1=lath21v05;

%tr and tc are the indexes that are clipped to the study area
for J=1:M
    tile= P{J}(50:55);
    y=str2double( P{J}(42:45) );
    m=str2double( P{J}(46:48) );
    [year,month,day]=julian2date(m,y);
    
    % Insert Date and Time data to Array
    if (tile=='h20v05')
        
        [tr]=find(Long<=36 & Long>=34.1); %exclude data with Lon above 36 and below 34.1
        
        temp=(NDVI{J,1});
        %data matrix reshape
        [X Y] = size(temp);
        n (:,3)= double(reshape(temp,X*Y,1));
        n(:,4)=Lat;
        n(:,5)=Long;
        n(:,1)= repmat(year,1);
        n(:,2)=month;
        n=n(tr,1:5);
        [tc]=find(n(:,4)<34);%exclude data with Lat above 34
        n=n(tc,1:5);
        n=array2table(n);
        n.Properties.VariableNames = {'Year','Month','NDVI','Lat','Long'};
        NDVI{J,1}= n;
        n=[];
    end
    
    if (tile=='h21v05')
        
        [tr]=find(Long1<=36 & Long1>=34.1); %exclude data with Lon above 36 and below 34.1
        
        temp=(NDVI{J,1});
        %data matrix reshape
        [X Y] = size(temp);
        n (:,3)= double(reshape(temp,X*Y,1));
        n(:,4)=Lat1;
        n(:,5)=Long1;
        n(:,1)= repmat(year,1);
        n(:,2)=repmat(month,1);
        n=n(tr,1:5);
        [tc]=find(n(:,4)<34);%exclude data with Lat above 34
        n=n(tc,1:5);
        n=array2table(n);
        n.Properties.VariableNames = {'Year','Month','NDVI','Lat','Long'};
        NDVI{J,1}= n;
        n=[];
        
    end
end
 
save('NDVI.mat','NDVI');
%%
%Create one table for all data
clear
load NDVI.mat
K=1;  T=zeros(5); M=size(NDVI,1);
t=table2array(NDVI{1,1});
    R=size(NDVI{1,1},1);
     T(K:R,1:5)=t; K=K+R; 

for J=2:M 
    t=table2array(NDVI{J,1});
    R=size(NDVI{J,1},1);
     T(K:(R+K-1),1:5)=t;
     K=K+R; 
end

 save('NDVI.mat','NDVI','T');    
 
    %Write table 'T' to CSV FILE 
    T=array2table(T);
    T.Properties.VariableNames = {'Year','Month','NDVI','Lat','Long'};

   writetable(T,'NDVI_Israel.csv');
  