function AODdata = RetrieveAOD (filename)
    %Set memory
    RawAOD= zeros(600,600,'int16');
    AOD = zeros(600,600,'double');
    AODdata = zeros(600*600,1,'double');
    
    %Read Fields
    RawAOD=hdfread(filename,'grid1km', 'Fields', 'Optical_Depth_Land');
    
    %Manipulate AOD
    %convert -9999 to NAN values in AOD field and apply scale factor for MIAC AOD
    RawAOD=double(RawAOD);
    RawAOD(RawAOD==-9999)=nan;
    %Create the final matrix
    AOD=RawAOD*0.001;
    %Transpose
    AOD=AOD';
    %Check size
    if ( (numel(RawAOD) ~= (600*600)))
        warning('Internal consistency not satisfied.');
    end
    
    %Reshape AOD and QC
    %M= [reshape(AOD,600*600,1) reshape(QC,600*600,1)];
    AODdata= single(reshape(AOD,600*600,1));
end