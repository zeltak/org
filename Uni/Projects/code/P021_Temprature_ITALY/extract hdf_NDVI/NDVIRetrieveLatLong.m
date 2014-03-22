function iLatLong = NDVIRetrieveLatLong (NDVI_I_Tie)
    
    fprintf('\nReading tie for lat: %s', char(NDVI_I_Tie.latTieName));
    fprintf('\nReading tie for long: %s', char(NDVI_I_Tie.longTieName));
    %Open the file
    iLatLong = zeros(1440000, 2);
    iLatLong = [load(NDVI_I_Tie.latTieName) load(NDVI_I_Tie.longTieName)];
end