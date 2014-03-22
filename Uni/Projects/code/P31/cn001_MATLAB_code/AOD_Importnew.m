function AOD_Importnew()
    
    %Set max files.
    MaxFiles = 200;
    RemainingFiles = 0;
    
    fprintf('\n\n\n****************** AOD Import ******************\n\n');
    Dir = RetrieveDir;
    %check for how many directories
    NumberOfDirectories = length(Dir);
    if (NumberOfDirectories == 0)
        fprintf('\n\nNo subdirectory found.\n\n');
        return;
    end
    
    %Allocate memory for Reference vector
    %vReference = zeros(1,600*600,'double');
    
    %loop for all the directories
    for i = 1:1:NumberOfDirectories
        fprintf('\n\nLooking at directory : %s\n', Dir(i).FullPath);
              
        chdir(Dir(i).FullPath);
        AOD_I_Tie = RetrieveTies(Dir(i).FullPath);
        %If the current directory have no tie, jump to the next
        if isempty(AOD_I_Tie)
            continue;
        end
        
        %Do all ties of the directory
        for iTie = 1:1:length(AOD_I_Tie)
            %Don't change the next line. Clear the variables for the new
            %directory
            clear M;
            %Retrieve filenames with the tie short name
            AOD_I_HDF = RetrieveHDFs (AOD_I_Tie(iTie));
            %Il no file where found with that tie, loop to the next tie
            if (length(AOD_I_HDF) == 0)
                continue;
            end
            
            %Set how much files remains
            RemainingFiles = length(AOD_I_HDF);
            %load lat and long
            LatLong = RetrieveLatLong (AOD_I_Tie(iTie));
            
            %Allocate enought memory for AODM and QCM
            %AODM = zeros(600*600,length(AOD_I_HDF),'single');
            %QCM = zeros(600*600,length(AOD_I_HDF),'single');
            if (length(AOD_I_HDF) >= MaxFiles)
                AODM = zeros(600*600,MaxFiles,'single');
                QCM = zeros(600*600,MaxFiles,'single');
            else
                AODM = zeros(600*600,length(AOD_I_HDF),'single');
                QCM = zeros(600*600,length(AOD_I_HDF),'single');
            end
            %Set the index which count before saving file
            partialHDF = 1;
            totalsavedfiles = 1;
            %Read and create the M structure of all the ties in the current
            %folder
            for iHDF = 1:1:length(AOD_I_HDF)
                %Create the vector for reference
                %vReference(1:(600*600)) = str2double(AOD_I_HDF(iHDF).Reference);
                vReference(partialHDF) = str2double(AOD_I_HDF(iHDF).Reference);
                %WYSIWYG
                fprintf('\nAppending (%d) : %s ', iHDF, AOD_I_HDF(iHDF).FullPath);
                if ( (iHDF == 1) || (partialHDF == 1) )
                    %If Mindex is not created
                    AODM (:,1) = RetrieveAOD(AOD_I_HDF(iHDF).FullPath);
                    QCM (:,1)= RetrieveQC(AOD_I_HDF(iHDF).FullPath);
                else
                    %M = [LatLong RetrieveAOD_QC(AOD_I_HDF(iHDF).FullPath) vReference'];
                    %M = [LatLong RetrieveAOD_QC(AOD_I_HDF(iHDF).FullPath)];
                    %M = [M; LatLong RetrieveAOD_QC(AOD_I_HDF(iHDF).FullPath) vReference'];
                    %M = [M; LatLong RetrieveAOD_QC(AOD_I_HDF(iHDF).FullPath)];
                    AODM(:,partialHDF) = RetrieveAOD(AOD_I_HDF(iHDF).FullPath);
                    QCM(:,partialHDF)=RetrieveQC(AOD_I_HDF(iHDF).FullPath);
                end
                if ((partialHDF >= MaxFiles) || (partialHDF == RemainingFiles))
                    tic
                    fprintf('\nCreating the Reference for filename..\n');
                    %Allocate memory for MReference
                    MReference = zeros(600*600*partialHDF,1,'double');
                    %Create the MReference by Reshape
                    MReference = reshape(repmat(vReference, 600*600, 1), 600*600*partialHDF, 1);
                    toc
                    %Concatenate lat long M and Reference
                    fprintf('\nFinal concatenation  ... \n');
                    tic
                    M= horzcat(double(repmat(LatLong, partialHDF,1)), ...
                        double( reshape(AODM, 600*600*partialHDF,1) ), ...
                        double(reshape(QCM,600*600*partialHDF,1)), ...
                        MReference);
                    toc
                    %Save the file every time the directory is changed.
                    fprintf('\nSaving the file : %s\n', ['TileName-' AOD_I_Tie(iTie).ShortName '-part' num2str(totalsavedfiles,0) '.txt']);
                    SaveMergedFile(['TileName-' AOD_I_Tie(iTie).ShortName '-part' num2str(totalsavedfiles,0) '.txt'], M);
                    
                    %Unload memory
                    clear M AODM QCM vReference MReference;
                    %Calculate the number of HDF files to read
                    RemainingFiles = RemainingFiles - partialHDF;
                    %Reallocate memory
                    if (RemainingFiles >= MaxFiles)
                        AODM = zeros(600*600,MaxFiles,'single');
                        QCM = zeros(600*600,MaxFiles,'single');
                    else
                        AODM = zeros(600*600,RemainingFiles,'single');
                        QCM = zeros(600*600,RemainingFiles,'single');
                    end
                    
                    partialHDF = 1;
                    totalsavedfiles = totalsavedfiles+1;
                    
                else
                    %If not save, increase the index
                    partialHDF = partialHDF + 1;
                end
                    
            end
            if (partialHDF ~= 1)
                    fprintf('\nSaving the last file ... \n');
                    %Allocate memory for MReference
                    MReference = zeros(600*600*partialHDF,1,'double');
                    %Create the MReference by Reshape
                    MReference = reshape(repmat(vReference, 600*600, 1), 600*600*partialHDF, 1);
                    %Clear as much memory as possible
                    clear vReference AOD_I_HDF;
                    %Concatenate lat long M and Reference
                    fprintf('\nFinal concatenation  ... \n');
                    tic
                    M= horzcat(double(repmat(LatLong, partialHDF,1)), ...
                        double( reshape(AODM, 600*600*partialHDF,1) ), ...
                        double(reshape(QCM,600*600*partialHDF,1)), ...
                        MReference);
                    %Clear as much memory as possible.
                    clear LatLong MReference AODM QCM;
                    toc
                    % latup = 72
                    % latlow = 29
                    % longup = -77
                    % longlow = 65
                    %Cut lat long by the target area
                    % M = M(find( (M(:,1) <= latup) & (M(:,1) >= latlow) & ...
                    %    (M(:,2) <= longup) & (M(:,2) >= longlow) ), : ); 
                    %Save the file every time the directory is changed.
                    fprintf('\nSaving the file : %s\n', ['TileName-' AOD_I_Tie(iTie).ShortName '-part' num2str(totalsavedfiles,0) '.txt']);
                    SaveMergedFile(['TileName-' AOD_I_Tie(iTie).ShortName '-part' num2str(totalsavedfiles,0) '.txt'], M);
            end
        end
        
    end
    % %For each Tie call the function Combine all files
    % %Each file
    % 600*600*5 
    % 15 ties
    % 365 files
    % 8 bytes
    % 
    % 600*600*15*5*365*8
    % 78 GB of RAM for each directory. 
end
