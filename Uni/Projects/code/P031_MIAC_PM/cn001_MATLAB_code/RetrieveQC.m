function QCdata = RetrieveQC (filename)
    %Set memory
    
    QC = zeros(600*600,1,'uint8');
    QCdata = zeros(600*600,1,'uint8');
    
    QC = double(hdfread(filename,'grid1km', 'Fields', 'AOT_QA'));
    %Transpose
    QC=QC';
    %Check size
    if ( (numel(QC) ~= (600*600)) )
        warning('Internal consistency not satisfied.');
    end
    
    %Reshape AOD and QC
    QCdata = single(reshape(QC,600*600,1));
end