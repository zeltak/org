cd /media/NAS/Uni/Data/Israel/modis3k/
%x=C06_2002.Properties.VariableNames;
name={'2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014'};

for I=2:14
myFolder=['/media/NAS/Uni/Data/Israel/modis3k/' name{I} '\' name{I}];

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
temp=zeros(10,24); W=1;

for k = 1:length(Files)
  baseFileName = Files(k).name;
  fullFileName = fullfile(myFolder, baseFileName);
  fprintf(1, 'Now reading %s\n', fullFileName);
  Array = csvread(fullFileName);
  temp(W:W+size(Array,1)-1,5:24)=Array;
  
  y=str2double(baseFileName(11:14));
  m=str2double(baseFileName(15:17));
  hr=str2double(baseFileName(19:22));
  %cd to folder with julian to date function
  cd /media/NAS/Uni/org/files/Uni/Projects/code/P046.Israel_MAIAC/NDVI-matlab/ ;
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
           temp(i,j)=NA;
        end
    end
end

T=array2table(temp);
x={'day','month','yr','hr','Comb550','comb550QA','DB550','DB550QA','DBspect412','DBspect470','DBspect660','DBssa412','DBssa470','DBssa660','DBsuref412','DBsuref470','DBsuref660','DT_L_O_QA','Lat','Long','DT_L_O','DTsuref470','DTsuref660','DTsuref2130'};
T.Properties.VariableNames=x;

cd /media/NAS/Uni/Data/Israel/modis3k/2012;
csvwrite_with_headers(['C06_10k_' name{I} '.csv'],temp,x)

end