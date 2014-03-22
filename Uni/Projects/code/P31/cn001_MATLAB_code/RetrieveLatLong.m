function LatLong = RetrieveLatLong (AOD_I_Tie)
    
    fprintf('\nReading tie : %s', char(AOD_I_Tie.TieName));
    %Open the file
    fid = fopen(AOD_I_Tie.TieName);
    %Read the file. Don't change the single
    iLatLong = fread(fid, [360000 2], 'single');
    %Close
    fclose(fid);
    %Flip lat with long
    %LatLong= double(fliplr(iLatLong));
    LatLong= single(fliplr(iLatLong));
end