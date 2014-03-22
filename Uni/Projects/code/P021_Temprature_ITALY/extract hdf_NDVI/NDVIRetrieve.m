function NDVIdata = NDVIRetrieve (filename)
    %Set memory
    rawNDVI= zeros(1200,1200,'int16');
    NDVI = zeros (1200, 1200, 'double');
    NDVIdata = zeros (1200*1200,1, 'double');
    %Read Fields
    
    rawNDVI = hdfread(filename, '/MOD_Grid_monthly_1km_VI/Data Fields/1 km monthly NDVI');
    
    NDVI=double(rawNDVI)*0.0001;
    %Transpose
    NDVI=NDVI';
        
    %Reshape AOD and QC
    %M= [reshape(AOD,600*600,1) reshape(QC,600*600,1)];
    NDVIdata= reshape(NDVI,1200*1200,1);
end