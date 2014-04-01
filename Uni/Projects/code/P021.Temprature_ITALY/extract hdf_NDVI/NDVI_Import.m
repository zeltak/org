function NDVI_Import()
    
   
    fprintf('\n\n\n****************** NDVI Import ******************\n\n');
    Dir = NDVIRetrieveDir;
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
        NDVI_I_Tie = NDVIRetrieveTies(Dir(i).FullPath);
        %If the current directory have no tie, jump to the next
        if isempty(NDVI_I_Tie)
            continue;
        end
        
        
        
            %Do all ties of the directory
        for iTie = 1:1:length(NDVI_I_Tie)
            %Don't change the next line. Clear the variables for the new
            %directory
            clear M;
            %Retrieve filenames with the tie short name
            NDVI_I_HDF = NDVIRetrieveHDFs (NDVI_I_Tie(iTie));
            %Il no file where found with that tie, loop to the next tie
            if (length(NDVI_I_HDF) == 0)
                continue;
            end
            
            %Set how much files remains
            %RemainingFiles = length(NDVI_I_HDF);
            %load lat and long
            LatLong = NDVIRetrieveLatLong (NDVI_I_Tie(iTie));
            
%             %Allocate enought memory for AODM and QCM
%             %AODM = zeros(600*600,length(NDVI_I_HDF),'single');
%             %QCM = zeros(600*600,length(NDVI_I_HDF),'single');
%             if (length(NDVI_I_HDF) >= MaxFiles)
%                 AODM = zeros(1200*1200,MaxFiles,'single');
%                 QCM = zeros(1200*1200,MaxFiles,'single');
%             else
%                 AODM = zeros(1200*1200,length(NDVI_I_HDF),'single');
%                 QCM = zeros(1200*1200,length(NDVI_I_HDF),'single');
%             end
%             %Set the index which count before saving file
%             partialHDF = 1;
%             totalsavedfiles = 1;
            
            fid = fopen(['TieName-' NDVI_I_Tie(iTie).ShortName '.txt'], 'w');
            fprintf(fid, 'Lat\tLong\tNDVI\tReference\n');    
            
            %Read and create the M structure of all the ties in the current
            %folder
           
            for iHDF = 1:1:length(NDVI_I_HDF)
                tic;
                %Create the vector for reference
                %vReference(1:(600*600)) = str2double(NDVI_I_HDF(iHDF).Reference);
                vReference = str2double(NDVI_I_HDF(iHDF).Reference);
                %WYSIWYG
                fprintf('\nAppending (%d) : %s \n', iHDF, NDVI_I_HDF(iHDF).FullPath);
                
                NDVI = NDVIRetrieve(NDVI_I_HDF(iHDF).FullPath);
                
                %Allocate memory for MReference
                MReference = zeros(1200*1200,1,'double');
                %Create the MReference by Reshape
                MReference = repmat(vReference, 1200*1200, 1);
                
                M= horzcat(double(LatLong), NDVI, MReference);
                
                [rows cols] = size(M);
                x = repmat('%.4f\t',1,(cols-1));
        
                fprintf(fid,[x,'%.4f\n'],M');
                
               
                %Unload memory
                clear M NDVI vReference MReference;
                toc
            end
            fclose(fid);
            
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
