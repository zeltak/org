

function [sYear]= RetrieveAllYear (FileExtension, HDF_I_Edges, lat, long)


    
    %% Retrieve dir list of files, with the extension you gave
    % 
    dirlist =dir(FileExtension);

    %Count the number of files in the current directory
    NumberOfFiles=size(dirlist, 1);

    %Show you the number
    fprintf('%s %d %s\n', 'Found ', NumberOfFiles, ' file(s) in the current directory'); 

    %Show you files name
    fprintf('%s\n','Files found in the current working directory :');
    fprintf('%s \n', dirlist.name);

    %Create the Structure that contains all files and allocate enought memory
    sYear = zeros(1200*1200*NumberOfFiles,5,'single');

    %% For Cycle to process of files contained in the choosen directory.
    tic;

    %Cycle as many times as the number of files. 
    for Index = 1:1:NumberOfFiles

        %Display what it is working on
        display (['Performing operation on file : ', dirlist(Index).name]);
%         %Split file name in each part
%         [PATHSTR, BASE_NAME, EXT]=fileparts(dirlist(Index).name);
%         
%         %Obtain the Day from the filename
%         %Obtain the starting position of the date
%         startposition = strfind(BASE_NAME, '.A');
%         if (length(startposition) ~= 1) 
%             error('Mismatch file name');
%         end
%         %Upgrade starting position
%         startposition = startposition + 2;
% 
%         %Now, obtain the ending position
%         endposition = strfind(BASE_NAME, '.h');
%         if (length(endposition) ~= 1) 
%             error('Mismatch file name');
%         end
%         %Upgrade starting position
%         endposition = endposition -1;
%         %Extract the day from the filename 
%         ExtractedDay = BASE_NAME(startposition:endposition);
        if Index == 1
            sYear = CreateMatrix(dirlist(Index).name, HDF_I_Edges, lat, long);
        else %Otherwise append
            %Call the function to retrieve the lat long data vector and
            %vertically concatenate the matrix
            sYear=[sYear; CreateMatrix(dirlist(Index).name, HDF_I_Edges, lat, long)];
        end
%         %Create the day vector
%         DayVector(1:length(M(:,1))) = str2num(ExtractedDay);
%         
%         sYear(Index).Data = [M DayVector'];
%         %Calculate the elapsedtime
        toc;
    end 
end
