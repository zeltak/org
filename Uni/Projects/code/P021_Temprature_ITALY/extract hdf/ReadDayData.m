 function [vDayData] = ReadDayData (filename) 

    GRID_NAME='MODIS_Grid_Daily_1km_LST';
    DATAFIELD_NAME='LST_Day_1km';

    % Open the HDF-EOS2 Grid file.
    file_id = hdfgd('open', filename, 'rdonly');

    % Read data from a data field.
    grid_id = hdfgd('attach', file_id, GRID_NAME);

    % extract the data for one day into a 1200x1200 matrix
    [buffer, fail] = hdfgd('readfield', grid_id, DATAFIELD_NAME, [], [], []);

    % Convert the buffer data to double type for plot.
    DayData=double(buffer);


    % This file contains coordinate variables that will not properly plot. 
    % To properly display the data, the latitude/longitude must be remapped.

    [xdimsize, ydimsize, upleft, lowright, status] = hdfgd('gridinfo', grid_id);

    % Detach from the grid object.
    hdfgd('detach', grid_id);

    % Close the File.
    hdfgd('close', file_id);

    %% Read attributes from the data field.
    SD_id = hdfsd('start', filename, 'rdonly');

    sds_index = hdfsd('nametoindex', SD_id, DATAFIELD_NAME);

    sds_id = hdfsd('select',SD_id, sds_index);

    % Read filledValue from the data field.
    fillvalue_index = hdfsd('findattr', sds_id, '_FillValue');
    [fillvalue, status] = hdfsd('readattr',sds_id, fillvalue_index);

    % Read units from the data field.
    units_index = hdfsd('findattr', sds_id, 'units');
    [units, status] = hdfsd('readattr',sds_id, units_index);

    % Read scale_factor from the data field.
    scale_index = hdfsd('findattr', sds_id, 'scale_factor');
    [scale, status] = hdfsd('readattr',sds_id, scale_index);

    % Read long_name from the data field.
    long_name_index = hdfsd('findattr', sds_id, 'long_name');
    [long_name, status] = hdfsd('readattr',sds_id, long_name_index);

    % Read valid_range from the data field.
    valid_range_index = hdfsd('findattr', sds_id, 'valid_range');
    [valid_range, status] = hdfsd('readattr',sds_id, valid_range_index);

    % Terminate access to the corresponding data set.
    hdfsd('endaccess', sds_id);
    % Close the file.
    hdfsd('end', SD_id);

    % Replace the filled value with NaN.
    DayData(DayData == fillvalue) = NaN;

    % Process valid_range.
    DayData(DayData < valid_range(1)) = NaN;
    DayData(DayData > valid_range(2)) = NaN;

    % Apply scale factor according to the field attribute LST.
    % "LST: LST data * scale_factor".
    DayData = DayData * scale;
    
    % Transpose the data to match the map projection. We saw that transpose
    % is not correct
    %DayData=DayData';
    
       %% Create the array from the matrix
    
    %Retrieve nrow ncol of data2
    [nrowDayData,ncolDayData] = size(DayData);

    %Reshape from matrix to linear vector
    vDayData = reshape(DayData,nrowDayData*ncolDayData,1);
   
end