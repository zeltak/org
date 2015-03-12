%%%%%%%%%%Written by Liuhua Shi%%%%%%%%%%%%%%%%%
%%%%%%%%%%2015-03-01%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    
%%%%%% p=netcdf('I:/Research/Northeastern PM modeling/1.Raw_data/PBL/hpbl.2013.nc');
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clc;clear;close all;fclose all;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
output_path='./';
%    ncdisp('hpbl.2013.nc');
  lat = ncread('hpbl.2013.nc','lat');
  long=ncread('hpbl.2013.nc','lon');
  time=ncread('hpbl.2013.nc','time');
  hpbl=ncread('hpbl.2013.nc','hpbl');
  
  Time_num=double(time)/24+datenum('1800-01-01 00:00');%%%%Time is counted since 0000-01-01
  Date_Str=datestr(Time_num);%%%%%%%get the date(2013-month-day)

  [n1,n2,n3]=size(hpbl);
  hpbl_1D=reshape(hpbl,[n1*n2*n3,1]);
 
  lat_1D=repmat(reshape(lat,[n1*n2,1]),[n3,1]);
  long_1D=repmat(reshape(long,[n1*n2,1]),[n3,1]);
  
  year_1D=2013.*ones([n1*n2*n3,1]);
  DOY=reshape([1:1:n3],[1,1,n3]);
  DOY_mat=repmat(DOY,[n1,n2]);
  DOY_1D=reshape(DOY_mat,[n1*n2*n3,1]);
  
  
  hpbl_2013=[lat_1D,long_1D,hpbl_1D,DOY_1D, year_1D];
  hpbl_table_2013=array2table(hpbl_2013);
  hpbl_table_2013.Properties.VariableNames={'Lat','Long','hpbl','DOY','year'};
  
%   hpbl_day=reshape(hpbl(:,:,1),[n1*n2,1]);
  
% screen data for northeastern (including Pen)
    lat_min=38;
    lat_max=48;
    long_min=-81;
    long_max=-66;    

   
tr=find(hpbl_table_2013.Long>=long_min & hpbl_table_2013.Long<=long_max &hpbl_table_2013.Lat>=lat_min & hpbl_table_2013.Lat<=lat_max); %exclude data outside the box 
hpbl_ne_2013=hpbl_table_2013(tr,:);

name=['pbl_2013','.mat'];
save([output_path,name],'hpbl_ne_2013');















%    ncid=netcdf.open('hpbl.2013.nc','NOWRITE');
%    for j=0:100
%        try 
%            varname{j+1} =netcdf.inqVar(ncid,j);
%            data{j+1}=netcdf.getVar(ncid,j);
%        end
%    end
%    Varname=netcdf.getAtt(ncid,3,'long_name');
%    Units=netcdf.getAtt(ncid,3,'units');
%    netcdf.close(ncid)
%    
%    lon=data{3};
%    lat=data{2};
%    time=data{1};
%    hpbl=data{8};
%    y=data{4};
%    x=data{5};