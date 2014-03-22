function NDVI_I_HDF = NDVIRetrieveHDFs (NDVI_I_Tie)

    %NDVI_I_Tie = NDVI_I_Tie (12);
    NDVI_I_HDF = struct ('workingdir', [], 'FullPath', [], 'HDFName', [], 'Reference',[],'ShortName', [], 'HDFNumber', 0);
    %Set the working dir
    workingdir = NDVI_I_Tie.workingdir;
    
    if ~isdir(workingdir)
        error('\n\nError from code: Workind directory does not exist');
        exit;
    end
    
    %% Retrieve dir list of Ties, with the extension you gave
    % 
    if (isunix)
        if (workingdir(length(workingdir)) ~= '/') workingdir = [workingdir '/'];
        end
    else
        if (workingdir(length(workingdir)) ~= '\') workingdir = [workingdir '\'];
        end
    end
    
    FileExtension = [workingdir '*' NDVI_I_Tie.ShortName '*.hdf'];
    dirlist =dir(FileExtension);

    %Count the number of files in the current directory
    NumberOfFiles=size(dirlist, 1);
    
    if (NumberOfFiles == 0)
        fprintf('\n\nNo file matching with Tie %s where found\n',NDVI_I_Tie.ShortName );
        NDVI_I_HDF = [];
        return;
    end

    %Show you the number
    fprintf('\nFound %d matching with Ties [%s] in the directory\n', NumberOfFiles, NDVI_I_Tie.ShortName); 

    %Show you files name
    fprintf('%s\n','Files for Ties found in the current working directory :');
    fprintf('%s \n', dirlist.name);

    for i =1:1:NumberOfFiles
        %Set the structure
        NDVI_I_HDF(i).workingdir = workingdir;
        NDVI_I_HDF(i).HDFName = char(dirlist(i).name);
        NDVI_I_HDF(i).HDFNumber = i;
        %Split in file names
        [PATHSTR, BASE_NAME, EXT]=fileparts(dirlist(i).name);
        %Save the date and day
        NDVI_I_HDF(i).Reference = BASE_NAME(10:16);
        NDVI_I_HDF(i).FullPath = [workingdir char(dirlist(i).name)];
        NDVI_I_HDF(i).ShortName = BASE_NAME(18:23);
        
    end
end
