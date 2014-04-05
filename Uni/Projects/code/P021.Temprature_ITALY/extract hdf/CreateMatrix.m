

function [DataMatrix] = CreateMatrix (filename, HDF_I_Edges, lat, long) 
%% Create the matrix with Lat, Long, Day, Night and Emis data and return it.

%% Check consistency
    if (nargin < 1) 
    fprintf ('\n%s\n', 'This function need some parameters.');
        return;
    end
    
   
    
    %% Create the matrix to save, calling the functions
    DataMatrix = [single(lat) single(long) single(ReadDayData(filename)) single(ReadNightData(filename)) single(ReadEmisData(filename))];
    
    %% Trim by target area
    
    
    %Trim by the target area
    DataMatrix = DataMatrix(find( (DataMatrix(:,1) <= HDF_I_Edges.latup) & (DataMatrix(:,1) >= HDF_I_Edges.latlow) & ...
            (DataMatrix(:,2) <= HDF_I_Edges.longup) & (DataMatrix(:,2) >= HDF_I_Edges.longlow) ), : ); 
    
    %Split file name in each part
    [PATHSTR, BASE_NAME, EXT]=fileparts(filename);
    
    %Obtain the Day from the filename
    %Obtain the starting position of the date
    startposition = strfind(BASE_NAME, '.A');
    if (length(startposition) ~= 1) 
        error('Mismatch file name');
    end
    %Upgrade starting position
    startposition = startposition + 2;

    %Now, obtain the ending position
    endposition = strfind(BASE_NAME, '.h');
    if (length(endposition) ~= 1) 
        error('Mismatch file name');
    end
    %Upgrade starting position
    endposition = endposition -1;
    %Extract the day from the filename 
    ExtractedDay = BASE_NAME(startposition:endposition);
    %Create the day vector
    DayVector(1:length(DataMatrix(:,1))) = str2num(ExtractedDay);
  
    %Calculate the elapsedtime
    DataMatrix = [DataMatrix single(DayVector')];     

end


