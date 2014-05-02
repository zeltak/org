function [year month day] = julian2date(seqday,year)
% Function to convert the sequential day & year input to year, month, & day
% output
% Usage:
% [year month day] = julian2date(seqday,year)
% 'seqday' is the sequential day that we are interested in. This can vary 
% from 1 to 365 (or 366).
% 'year' is the year

% Author: Ramesh Praveenkumar

if ~(isnumeric(seqday)) || ~(isnumeric(year))
    error('Invalid input. Numeric input expected');
end

dayCount=0;

for monthCount = 1 : 12
dayCount = dayCount+eomday(year,monthCount);
if(seqday<=dayCount)
    month=monthCount;
    day = seqday-(dayCount-eomday(year,monthCount));
    % bug change: araja
    if day == 0
        month = monthCount-1;
        day = eomday(year,month);
    end
    % bug change: araja
    break;
end
end

% You may use following code to concvert the date to string
% datestr(datenum([year month day]),'mmmm,dd,yyyy')
