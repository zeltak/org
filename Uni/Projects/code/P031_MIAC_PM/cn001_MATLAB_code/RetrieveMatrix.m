function M = RetrieveMatrix (LatLong,AOD_QC, vReference)

    M = zeros(600*600, 5);
    M = [LatLong AOD_QC vReference];
   % latup = 72
   % latlow = 29
   % longup = -77
   % longlow = 65
    %Trim by the target area
   % M = M(find( (M(:,1) <= latup) & (M(:,1) >= latlow) & ...
   %    (M(:,2) <= longup) & (M(:,2) >= longlow) ), : ); 

end