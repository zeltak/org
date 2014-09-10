%add path to all matlab functions
addpath('/media/NAS/Uni/org/files/Uni/Projects/code/$Matlab/') ;


cd /media/NAS/Uni/Data/Israel/modis3k/
name={'2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014'};

for I=12:12
myFolder=['/media/NAS/Uni/Data/Israel/modis3k/' name{I}];

% loop through the files and open. Note that dir also lists the directories, so you have to check for them. 
% open your file here
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if ~isdir(myFolder);
  errorMessage = sprintf('Error: The following folder does not exist:\n%s', myFolder);
  uiwait(warndlg(errorMessage));
  return;
end
filePattern = fullfile(myFolder, '*.csv');
Files = dir(filePattern);
temp=zeros(10,11); W=1;

for k = 1:length(Files)
  baseFileName = Files(k).name;
  fullFileName = fullfile(myFolder, baseFileName);
  fprintf(1, 'Now reading %s\n', fullFileName);
  Array = csvread(fullFileName);
  temp(W:W+size(Array,1)-1,5:11)=Array;
  
  y=str2double(baseFileName(11:14));
  m=str2double(baseFileName(15:17));
  hr=str2double(baseFileName(19:22));
  [year,month,day]=julian2date(m,y);
  
  temp(W:W+size(Array,1)-1,1)=day;
  temp(W:W+size(Array,1)-1,2)=month;
  temp(W:W+size(Array,1)-1,3)=y;
  temp(W:W+size(Array,1)-1,4)=hr;
  
  W=size(temp,1)+1;
 
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% create table for each year

for i=1:size(temp,1)
    for j=1:size(temp,2)
        if temp(i,j)==-999
           temp(i,j)=NaN;
        end
    end
end

T=array2table(temp);
x={'day','month','yr','hr','DT_L_O_QA','Lat','Long','DT_L_O','DTsuref470','DTsuref660','DTsuref2130'};
T.Properties.VariableNames=x;

% uses a function that writes csv with headers
csvwrite_with_headers(['C06_3k_' name{I} '.csv'],temp,x)

end