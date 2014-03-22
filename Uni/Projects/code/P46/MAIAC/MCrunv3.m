% %run aqua and terra HDF imports
% %THIS CODE HAS BEEN UPDATED ON 11.02.2014. IT UPLOADS THE PATHNAMES, LAT AND LON DATA, AND CALLS THE FUNCTION FOR EXTRACTING MAIAC HDF FILES
% % load files: Pathname, lat,lon
% 
% 
% 
% %% Aqua
% %change dir to path
% cd '/media/NAS/Uni/org/files/Uni/Projects/code/P46/MAIAC';
% clear
% %import text file of aqua hdf files
% PathNameA=readtable('/media/NAS/Uni/Data/Israel/MAIAC_AOD/path/path_aqua.txt');
% PathName=table2cell(PathNameA);
% P=PathName;
% 
% %lat long hdf file
% Lat=hdfread ('/media/NAS/Uni/Data/Israel/MAIAC_AOD/latlon/MAIACLatlon.Israel.hdf','latlon','Fields','lat');
% Lon=hdfread ('/media/NAS/Uni/Data/Israel/MAIAC_AOD/latlon/MAIACLatlon.Israel.hdf','latlon','Fields','lon');
% 
% %Read function MCread with output variable 'Table' that includes the
% %following columns: day,month,year,hour,lat,lon.AOD
% f=1;Path=PathName(1,1);
% for Y=2002:2013
%     for II=1:length(P)
%         yr=str2double( P{II}(59:62) );
%         if (yr~=Y || II==length(P))
%             if II==length(P)
%                 Path=P(f:II);
%             else Path=P(f:II-1);
%             end
%             P=P(II:length(P));
%             f=1;
%             break
%         end
%         
%     end
%     
%      TableA=MCreadv3(Path,Lat,Lon);
%     
%     %save 'Table' as matlab variable (*.m)
%     Years(Y) = {num2str(Y)};
%     save (['/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/maiac_aod/MAIACAqIsr_' Years{Y}],'TableA');
%     
%     %save table as csv
%     fileID=fopen(['/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/maiac_aod/MAIACAqIsr_' Years{Y} '.csv'],'w');
%     fprintf(fileID,'%6s %6s %6s %6s %6s %6s %6s\r\n','Day','Month','Year','Hour','Lat','Lon','AOD');
%     for I=1:length(TableA)
%         fprintf(fileID,'%6.0f, %6.0f ,%6.0f, %6.0f, %6.4f ,%6.4f, %6.4f\r\n',TableA(I,:));
%         fprintf('\n');
%     end
%     fclose(fileID);
% % clears TableA
% TableA=[];
%     
% end



%% tera
%change dir to path
cd '/media/NAS/Uni/org/files/Uni/Projects/code/P46/MAIAC';
clear
%import text file of aqua hdf files
PathNameA=readtable('/media/NAS/Uni/Data/Israel/MAIAC_AOD/path/path_tera.txt');
PathName=table2cell(PathNameA);
P=PathName;

%lat long hdf file
Lat=hdfread ('/media/NAS/Uni/Data/Israel/MAIAC_AOD/latlon/MAIACLatlon.Israel.hdf','latlon','Fields','lat');
Lon=hdfread ('/media/NAS/Uni/Data/Israel/MAIAC_AOD/latlon/MAIACLatlon.Israel.hdf','latlon','Fields','lon');

%Read function MCread with output variable 'Table' that includes the
%following columns: day,month,year,hour,lat,lon.AOD
f=1;Path=PathName(1,1);
for Y=2000:2013
    for II=1:length(P)
        yr=str2double( P{II}(59:62) );
        if (yr~=Y || II==length(P))
            if II==length(P)
                Path=P(f:II);
            else Path=P(f:II-1);
            end
            P=P(II:length(P));
            f=1;
            break
        end
        
    end
 
     TableA=MCreadv3(Path,Lat,Lon);
    
    %save 'Table' as matlab variable (*.m)
    Years(Y) = {num2str(Y)};
    save (['/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/maiac_aod/MAIACTrIsr_' Years{Y}],'TableA');
    
    %save table as csv
    fileID=fopen(['/media/NAS/Uni/Projects/P046_Israel_MAIAC/0.raw/maiac_aod/MAIACTrIsr_' Years{Y} '.csv'],'w');
    fprintf(fileID,'%6s %6s %6s %6s %6s %6s %6s\r\n','Day','Month','Year','Hour','Lat','Lon','AOD');
    for I=1:length(TableA)
        fprintf(fileID,'%6.0f, %6.0f ,%6.0f, %6.0f, %6.4f ,%6.4f, %6.4f\r\n',TableA(I,:));
        fprintf('\n');
    end
    fclose(fileID);
% clears TableA
TableA=[];
    
end

% colnames={'Day','Month','Year','Hour','Lat','Lon','AOD'};

% 




