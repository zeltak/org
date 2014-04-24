%run aqua and terra HDF imports
%THIS CODE HAS BEEN UPDATED ON 11.02.2014. IT UPLOADS THE PATHNAMES, LAT AND LON DATA, AND CALLS THE FUNCTION FOR EXTRACTING MAIAC HDF FILES
% load files: Pathname, lat,lon



%% Aqua
%change dir to path
cd '/media/NAS/Uni/org/files/Uni/Projects/code/P46/MAIAC'
clear
%import text file of aqua hdf files
PathNameA=readtable('/media/NAS/Uni/Data/Israel/MAIAC_AOD/path/path_aqua.txt');
PathName=table2cell(PathNameA);
P=PathName;

%lat long hdf file
Lat=hdfread ('/media/NAS/Uni/Data/Israel/MAIAC_AOD/latlon/MAIACLatlon.Israel.hdf','latlon','Fields','lat');
Lon=hdfread ('/media/NAS/Uni/Data/Israel/MAIAC_AOD/latlon/MAIACLatlon.Israel.hdf','latlon','Fields','lon');

%Read function MCread with output variable 'Table' that includes the
%following columns: day,month,year,hour,lat,lon.AOD
f=1;
for Y=2002:2013
 for II=1:length(P)

	yr=str2double( P{II}(59:62) );
	if (yr~=Y || II=length(P); 
	Path=P(f,II-1);P=P{II:length(P)};f=II;
	end
	break
 end


TableA=MCreadv3(Path,Lat,Lon);

%save 'Table' as matlab variable (*.m) 
save (['MAIACAqIsr_'Y],'TableA');


end



%Convert double matrix to table with column names and save as a text file
% colnames={'Day','Month','Year','Hour','Lat','Lon','AOD'};
% 
% r=[colnames;num2cell(TableA)];
% rr=cell2table(r);
%writetable(rr,'/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/maiac_aod/tableA','WriteVariableNames',0);






%% Terra

%change dir to path
cd '/media/NAS/Uni/org/files/Uni/Projects/code/P46/MAIAC'
clear
%import text file of aqua hdf files
PathNameA=readtable('/media/NAS/Uni/Data/Israel/MAIAC_AOD/path/path_tera.txt');
PathName=table2cell(PathNameA);

%lat long hdf file
Lat=hdfread ('/media/NAS/Uni/Data/Israel/MAIAC_AOD/latlon/MAIACLatlon.Israel.hdf','latlon','Fields','lat');
Lon=hdfread ('/media/NAS/Uni/Data/Israel/MAIAC_AOD/latlon/MAIACLatlon.Israel.hdf','latlon','Fields','lon');

%Read function MCread with output variable 'Table' that includes the
%following columns: day,month,year,hour,lat,lon.AOD
TableT=MCreadv2(PathName,Lat,Lon);

%save 'Table' as matlab variable (*.m) 
save ('MAIAC_DATA_Terra','TableT');

%Convert double matrix to table with column names and save as a text file
% colnames={'Day','Month','Year','Hour','Lat','Lon','AOD'};
% 
% r=[colnames;num2cell(TableT)];
% rr=cell2table(r);
% writetable(rr,'/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/maiac_aod/tableT','WriteVariableNames',0);
