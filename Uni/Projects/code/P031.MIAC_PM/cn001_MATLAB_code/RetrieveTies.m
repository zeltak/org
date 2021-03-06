function AOD_I_Tie = RetrieveTies (workingdir)

    AOD_I_Tie = struct ('workingdir', [], 'ShortName', [], 'TieName',[],'TieNumber', 0);

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
    FileExtension = [workingdir '*.hdf.latlon'];
    dirlist =dir(FileExtension);

    %Count the number of files in the current directory
    NumberOfFiles=size(dirlist, 1);
    
    if (NumberOfFiles == 0)
        fprintf('The current directory contains no file');
        AOD_I_Tie = [];
        return;
    end

    %Show you the number
    fprintf('%s %d %s\n', 'Found ', NumberOfFiles, ' Ties in the directory\n'); 

    %Show you files name
    fprintf('%s\n','Files for Ties found in the current working directory :');
    fprintf('%s \n', dirlist.name);

    for i =1:1:NumberOfFiles
        %Set the structure
        AOD_I_Tie(i).workingdir = workingdir;
        AOD_I_Tie(i).TieName = char(dirlist(i).name);
        AOD_I_Tie(i).TieNumber = i;
        %Split in file names
        [PATHSTR, BASE_NAME, EXT]=fileparts(dirlist(i).name);
        %Obtain the Tie from the filename
        %Obtain the starting position of the date
        startposition = strfind(BASE_NAME, 'MAIACAOT.h');
        if (length(startposition) ~= 1) 
            error('Mismatch file name');
        end
        %Upgrade starting position
        startposition = startposition + 10;
        %Now, obtain the ending position
        endposition = strfind(BASE_NAME, 'MAIACAOT.h');
        %Now, obtain the ending position
        endposition = endposition + 14;
        %Extract the day from the filename 
        TieName = BASE_NAME(startposition:endposition);
        AOD_I_Tie(i).ShortName = TieName;
    end


end
