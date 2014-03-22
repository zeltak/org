function SaveHugeFile (filename, M)

    %Save as tab delimited
    %Code to write the file. 10 time faster
    tic
    fid = fopen(filename, 'w');
    [rows cols] = size(M);
    x = repmat('%.4f\t',1,(cols-1));
    fprintf(fid, 'Lat\tLong\tDay\tNight\tEmis\tReference\n');
    fprintf(fid,[x,'%.4f\n'],M');
    fclose(fid);
    toc
end