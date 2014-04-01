

function HDF_Import (whatimport)
%% Import HDF image file and export lat, long, Day, Night, Emis and Reference day
% from a list of HDF image files and a series of lat and long files.
% Lat and long files must be as vectors.
% Type: 
% HDF_Import ('stage1') or HDF_Import ('stage2') or HDF_Import ('stage3')
% or HDF_Import ('stage4') to perform single geographical block analysis or
% HDF_Import ('all') to perform all analyses.
% Inpath and Outpath should be modified and setted to reflect the file
% actual position.
% LEGEND:
% Stage1: h18v04 files and as output a file named OutputMergedh18v04.txt
% Stage2: h18v05 files and as output a file named OutputMergedh18v05.txt
% Stage3: h19v04 files and as output a file named OutputMergedh19v04.txt
% Stage4: h19v05 files and as output a file named OutputMergedh19v05.txt
%
% Legend to the main structure:
% HDF_I_S = struct('inpath', {'/Users/andrea/Documents/MATLAB/Itai/matlab/'}, ...
%                                   'outpath', {'/Users/andrea/Documents/MATLAB/Itai/New2/Output/'}, ...
%                                   'codepath', {'/Users/andrea/Documents/MATLAB/Itai/New2'}, ...
%                                   'lat1', {'lat_h18v04.output'},'long1', {'long_h18v04.output'}, ...
%                                   'lat2', {'lat_h18v05.output'},'long2',{'long_h18v05.output'}, ...
%                                   'lat3', {'lat_h19v04.output'},'long3',{'long_h19v04.output'}, ...
%                                   'lat4', {'lat_h19v05.output'},'long4',{'long_h19v05.output'}, ...
%                                   'stage1', {'*h18v04*.hdf'}, 'stage2', {'*h18v05*.hdf'}, ...
%                                   'stage3', {'*h19v04*.hdf'}, 'stage4', {'*h19v05*.hdf'}, ...
%                                   'stages', {4}, 'FileExtension', {'*.hdf'}, ...
%                                   'OutFileName1', {'OutputMergedh18v04.txt'}, ...
%                                   'OutFileName2', {'OutputMergedh18v05.txt'}, ...
%                                   'OutFileName3', {'OutputMergedh19v04.txt'}, ...
%                                   'OutFileName4', {'OutputMergedh19v05.txt'});


    if (nargin < 1) 
    fprintf ('\n%s\n', 'This function need some parameters.');
        return;
    end
    %My path
    HDF_I_S = struct('inpath', {'f:\Uni\Projects\P020_Temprature_ITALY\1_Raw_data\MODIS\T2011\'}, ...
                                  'outpath', {'f:\Uni\Projects\P020_Temprature_ITALY\1_Raw_data\MODIS\T2011\Output\'}, ...
                                  'codepath', {'f:\Uni\Projects\P020_Temprature_ITALY\1_Raw_data\MODIS\T2011\'}, ...
                                  'lat1', {'lat_h18v04.output'},'long1', {'long_h18v04.output'}, ...
                                  'lat2', {'lat_h18v05.output'},'long2',{'long_h18v05.output'}, ...
                                  'lat3', {'lat_h19v04.output'},'long3',{'long_h19v04.output'}, ...
                                  'lat4', {'lat_h19v05.output'},'long4',{'long_h19v05.output'}, ...
                                  'stage1', {'*h18v04*.hdf'}, 'stage2', {'*h18v05*.hdf'}, ...
                                  'stage3', {'*h19v04*.hdf'}, 'stage4', {'*h19v05*.hdf'}, ...
                                  'stages', {4}, 'FileExtension', {'*.hdf'}, ...
                                  'OutFileName1', {'OutputMergedh18v04.txt'}, ...
                                  'OutFileName2', {'OutputMergedh18v05.txt'}, ...
                                  'OutFileName3', {'OutputMergedh19v04.txt'}, ...
                                  'OutFileName4', {'OutputMergedh19v05.txt'});

    %Create the structure for the edges to trim the blocks.                         
     HDF_I_Edges = struct ('latup', 47.4, 'latlow', 36.5, 'longup', 18.6, 'longlow', 6.4);
    
    
    %Original coordinated given by Itai and Francesco
    % longTu = 19;
    %     longTl = 6;
    %     latTu = 48;
    %     latTl = 36;

    %Latitute and Longitude structure                          
    HDF_I_geog = struct('lat', {}, 'long', {}, 'stage', {});

    %Create the output directory if it doesnt' exist
    if (~isdir(HDF_I_S.outpath)) 
        mkdir(HDF_I_S.outpath);
    end

    path(path, HDF_I_S.codepath);

    %Change the working dir
    chdir(HDF_I_S.inpath);

    %Allocate space
    for i =1:1:HDF_I_S.stages
        HDF_I_geog(i).lat= zeros(1440000,1);
        HDF_I_geog(i).long= zeros(1440000,1);
    end


    %% Perform stage 1 
    % Retrieve dir list of files, with the extension you gave
    % 
    if (strcmpi(whatimport,'stage1') | strcmpi(whatimport,'all'))
        
        clear dirlist NumberOfFiles sYear hugeM

        %Change the working dir
        chdir(HDF_I_S.inpath);

        dirlist =dir(HDF_I_S.stage1);

        %Count the number of files in the current directory
        NumberOfFiles=size(dirlist, 1);
        
        if (NumberOfFiles ~= 0)
            
            fprintf('\n\nLoading stage 1 coordinates ....\n')
            %Load latituge and longitude in memory
            HDF_I_geog(1).lat = load(HDF_I_S.lat1);
            HDF_I_geog(1).long = load(HDF_I_S.long1);
            
            %Obtain all the files al load it in memory (sYear structure)
            [hugeM]= RetrieveAllYear (HDF_I_S.stage1, HDF_I_Edges, HDF_I_geog(1).lat,  HDF_I_geog(1).long);
            
%             %Internal check
%             if NumberOfFiles ~= length(sYear)
%                 error('Missmatch in file and Structure loaded in memory');
%             end
%             
%             %Define the first matrix
%             hugeM = sYear(1).Data;
%             
%             %Concatenate matrix
%             for Index=2:1:length(sYear)
%                 hugeM = [hugeM; sYear(Index).Data];
%             end
            tic;
            %Save the Merged file
            SaveHugeFile([HDF_I_S.outpath HDF_I_S.OutFileName1], hugeM);

            fprintf('\nOutputfile : %s\n', [HDF_I_S.outpath HDF_I_S.OutFileName1]);
            toc
        else
            %Display that no the has been processed
            fprintf('\n\nNo files to process in stage1....\n\n')
             
        end
        
        %Chech for the 'single' class
        if max(max(hugeM(:,1:5))) > 999 
            warning('Class single for hugeM is not enought. Consider double');
        end
       
    end

    %% Perform stage 2 
    % Retrieve dir list of files, with the extension you gave
    % 
    if (strcmpi(whatimport,'stage2') | strcmpi(whatimport,'all'))
    
        clear dirlist NumberOfFiles sYear hugeM
        
        %Change the working dir
        chdir(HDF_I_S.inpath);
        dirlist =dir(HDF_I_S.stage2);

        %Count the number of files in the current directory
        NumberOfFiles=size(dirlist, 1);
        
        if (NumberOfFiles ~= 0)
            
            fprintf('\n\nLoading stage 2 coordinates ....\n')
            %Load latituge and longitude in memory
            HDF_I_geog(2).lat = load(HDF_I_S.lat2);
            HDF_I_geog(2).long = load(HDF_I_S.long2);
            
            %Obtain all the files al load it in memory (sYear structure)
            [hugeM]= RetrieveAllYear (HDF_I_S.stage2,HDF_I_Edges,HDF_I_geog(2).lat,  HDF_I_geog(2).long);
            
%             %Internal check
%             if NumberOfFiles ~= length(sYear)
%                 error('Missmatch in file and Structure loaded in memory');
%             end
%             
%             %Define the first matrix
%             hugeM = sYear(1).Data;
%             
%             %Concatenate matrix
%             for Index=2:1:length(sYear)
%                 hugeM = [hugeM; sYear(Index).Data];
%             end
            tic;
            %Save the Merged file
            SaveHugeFile([HDF_I_S.outpath HDF_I_S.OutFileName2], hugeM);
            fprintf('\nOutputfile : %s\n', [HDF_I_S.outpath HDF_I_S.OutFileName2]);
            toc
        else
            %Display that no the has been processed
            fprintf('\n\nNo files to process in stage2....\n\n');
             
        end
        %Chech for the 'single' class
        if max(max(hugeM(:,1:5))) > 999 
            warning('Class single for hugeM is not enought. Consider double');
        end
        
    end
    

    %% Perform stage 3
    % Retrieve dir list of files, with the extension you gave
    % 
    if (strcmpi(whatimport,'stage3') | strcmpi(whatimport,'all'))
    
    
        clear dirlist NumberOfFiles sYear hugeM
        
        
        %Change the working dir
        chdir(HDF_I_S.inpath);
        dirlist =dir(HDF_I_S.stage3);

        %Count the number of files in the current directory
        NumberOfFiles=size(dirlist, 1);

        
        if (NumberOfFiles ~= 0)
            
            fprintf('\n\nLoading stage 3 coordinates ....\n')
            %Load latituge and longitude in memory
            HDF_I_geog(3).lat = load(HDF_I_S.lat3);
            HDF_I_geog(3).long = load(HDF_I_S.long3);
            
            [hugeM]= RetrieveAllYear (HDF_I_S.stage3,HDF_I_Edges,HDF_I_geog(3).lat, HDF_I_geog(3).long);
            
%             if NumberOfFiles ~= length(sYear)
%                 error('Missmatch in file and Structure loaded in memory');
%             end
% 
%             hugeM = sYear(1).Data;
% 
%             for Index=2:1:length(sYear)
%                 hugeM = [hugeM; sYear(Index).Data];
%             end
            tic;
            %Save the Merge
            fprintf('\nOutputfile : %s\n', [HDF_I_S.outpath HDF_I_S.OutFileName3]);
            SaveHugeFile([HDF_I_S.outpath HDF_I_S.OutFileName3], hugeM);
            toc
        else
            %Display that no the has been processed
            fprintf('\n\nNo files to process in stage3....\n\n');   
        end 
        %Chech for the 'single' class
        if max(max(hugeM(:,1:5))) > 999 
            warning('Class single for hugeM is not enought. Consider double');
        end
    end
    
    
    %% Perform stage 4
    % Retrieve dir list of files, with the extension you gave
    % 
    if (strcmpi(whatimport,'stage4') | strcmpi(whatimport,'all'))
        
        clear dirlist NumberOfFiles sYear hugeM
        
        %Change the working dir
        chdir(HDF_I_S.inpath);
        dirlist =dir(HDF_I_S.stage4);

        %Count the number of files in the current directory
        NumberOfFiles=size(dirlist, 1);

        if (NumberOfFiles ~= 0)
            
            fprintf('\n\nLoading stage 4 coordinates ....\n')
            %Load latituge and longitude in memory
            HDF_I_geog(4).lat = load(HDF_I_S.lat4);
            HDF_I_geog(4).long = load(HDF_I_S.long4);
            
            [hugeM]= RetrieveAllYear (HDF_I_S.stage4, HDF_I_Edges, HDF_I_geog(4).lat,  HDF_I_geog(4).long);
            
%             if NumberOfFiles ~= length(sYear)
%                 error('Missmatch in file and Structure loaded in memory');
%             end
%             tic;
%             hugeM = sYear(1).Data;
% 
%             for Index=2:1:length(sYear)
%                 hugeM = [hugeM; sYear(Index).Data];
%             end
%             toc
            tic;
            %Save the Merge
            SaveHugeFile([HDF_I_S.outpath HDF_I_S.OutFileName4], hugeM);
            fprintf('\nOutputfile : %s\n', [HDF_I_S.outpath HDF_I_S.OutFileName4]);
            toc
        else
            %Display that no the has been processed
            fprintf('\n\nNo files to process in stage4....\n\n');   
        end 
        %Chech for the 'single' class
        if max(max(hugeM(:,1:5))) > 999 
            warning('Class single for hugeM is not enought. Consider double');
        end
    end

    %End of the function
end

