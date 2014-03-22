function [AODdata,QCdata] = RetrieveAOD_QC (filename)
    %Set memory
    RawAOD= zeros(600,600,'int16');
    QC = zeros(600,600,'uint8');
    AOD = zeros(600,600,'double');
    M = zeros(600,600,'double');
    
    %Read Fields
    RawAOD=hdfread(filename,'grid1km', 'Fields', 'Optical_Depth_Land');
    
    %Manipulate AOD
    %convert -9999 to NAN values in AOD field and apply scale factor for MIAC AOD
    RawAOD=double(RawAOD);
    RawAOD(RawAOD==-9999)=nan;
    %Create the final matrix
    AOD=RawAOD*0.001;
    
    QC = double(hdfread(filename,'grid1km', 'Fields', 'AOT_QA'));

    %Check size
    if ( (numel(RawAOD) ~= (600*600)) &  (numel(QC) ~= (600*600)) )
        warning('Internal consistency not satisfied.');
    end

    %Reshape AOD and QC
    %M= [reshape(AOD,600*600,1) reshape(QC,600*600,1)];
    AODData= single(reshape(AOD,600*600,1));
    QCdata= single(reshape(QC,600*600,1))];
end