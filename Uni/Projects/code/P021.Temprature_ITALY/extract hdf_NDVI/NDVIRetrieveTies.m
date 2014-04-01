function NDVI_I_Tie = NDVIRetrieveTies (workingdir)

    NDVI_I_Tie = struct ('workingdir', [], 'ShortName', [], 'latTieName',[],'longTieName',[],'TieNumber', 0);

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
    FileExtension = [workingdir 'lat*.output'];
    dirlist =dir(FileExtension);

    %Count the number of files in the current directory
    NumberOfFiles=size(dirlist, 1);
    
    if (NumberOfFiles == 0)
        fprintf('The current directory contains no file');
        NDVI_I_Tie = [];
        return;
    end

    for i =1:1:NumberOfFiles
        %Set the structure
        [PATHSTR, BASE_NAME, EXT]=fileparts(dirlist(i).name);
        NDVI_I_Tie(i).workingdir = workingdir;
        
        NDVI_I_Tie(i).TieNumber = i;
        %Split in file names
        
        %Obtain the Tie from the filename
        %Obtain the starting position of the date
%         startposition = strfind(BASE_NAME, 'MOD13A3.A');
%         if (length(startposition) ~= 1) 
%             error('Mismatch file name');
%         end
        %Upgrade starting position
        startposition = 6;
        %Now, obtain the ending position
        endposition = 10;
        %Now, obtain the ending position
        %Extract the day from the filename 
        TieName = BASE_NAME(startposition:endposition);
        NDVI_I_Tie(i).ShortName = TieName;
        NDVI_I_Tie(i).latTieName = ['lat_h' TieName '.output'];
        NDVI_I_Tie(i).longTieName = ['long_h' TieName '.output'];
    end


end
