%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read MAIAC AOD hdf
% extract AOD data in Northeastern USA from unclipped AOD file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear;clc;close all;fclose all;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data_path='../1.Raw_data/MAIAC_AOD/';
fn_clip='extract_lat_long_AOD/lat_long_AOD_clip.csv';
lat_long_AOD_clip=readtable([data_path,fn_clip]);
n_grids=size(lat_long_AOD_clip,1);

ID_GUID_h08v07=lat_long_AOD_clip(lat_long_AOD_clip.ID>=1&lat_long_AOD_clip.ID<=360000,:);
ID_GUID_h08v08=lat_long_AOD_clip(lat_long_AOD_clip.ID>=1+360000&lat_long_AOD_clip.ID<=360000+360000,:);
ID_GUID_h09v07=lat_long_AOD_clip(lat_long_AOD_clip.ID>=1+2*360000&lat_long_AOD_clip.ID<=360000+2*360000,:);
ID_GUID_h09v08=lat_long_AOD_clip(lat_long_AOD_clip.ID>=1+3*360000&lat_long_AOD_clip.ID<=360000+3*360000,:);

ID_GUID_all={ID_GUID_h08v07;ID_GUID_h08v08;ID_GUID_h09v07;ID_GUID_h09v08};
tile_name={'h08v07','h08v08','h09v07','h09v08'};
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ntile=4;
year_select=2014;

data_path_year=[data_path,num2str(year_select),'/'];
n_days = yeardays(year_select);

AOD_stage3_year=[];

%for dd=1:1
for dd=1:n_days
    tic
    [y m d] = julian2date(dd,year_select);
    DOY=dd;
    disp(['Year = ',num2str(year_select),' Month = ',num2str(DOY)]);
    AOD_stage3_day=table(year_select.*ones(n_grids,1));
    AOD_stage3_day.Properties.VariableNames={'Year'};
%     AOD_stage3_day.year=year_select.*ones(n_grids,1);
    AOD_stage3_day.Month=m.*ones(n_grids,1);
    AOD_stage3_day.Day=d.*ones(n_grids,1);
    AOD_stage3_day.DOY=DOY.*ones(n_grids,1);
    AOD_stage3_day.GUID=table2array(lat_long_AOD_clip(:,1));
    AOD_stage3_day.Lat=table2array(lat_long_AOD_clip(:,3));
    AOD_stage3_day.Long=table2array(lat_long_AOD_clip(:,4));
    AOD_stage3_day.AOD=nan(n_grids,1);
    
    for tt=1:ntile
%     for tt=1:1    
        allfile=dir([data_path_year,tile_name{tt},'/','MAIACTAOT.',tile_name{tt},'.',num2str(year_select),num2str(DOY,'%03d'),'*']); %%%dir:list all Terra data  
        if numel(allfile)>0   %%%%avoid error
            AOT_1D_tile=nan(360000,numel(allfile));
            for ii=1:numel(allfile)
                fn=allfile(ii).name;
%                 disp(fn);
                AOT_temp=hdfread ([data_path_year,tile_name{tt},'/',fn],'grid1km','Fields','Optical_Depth_Land');
                AOT_temp=double(AOT_temp);                
                AOT_temp(AOT_temp<-100|AOT_temp>5000)=NaN;
                AOT_temp=AOT_temp.*0.001;%.0001 scale factor
                [nn1,nn2]=size(AOT_temp);
                AOT_1D_temp=reshape(AOT_temp,[nn1*nn2,1]);%convert to 1 dimension
                AOT_1D_tile(:,ii)=AOT_1D_temp;
            end
            AOT_1D_mean=nanmean(AOT_1D_tile,2);%ignore nan and calculate mean; 2 means for each row; 1 stands for each column
            ID_GUID_temp=ID_GUID_all{tt};
            ind_temp=table2array(ID_GUID_temp(:,2))-(tt-1)*360000;    
            ind_GUID_temp=table2array(ID_GUID_temp(:,1))-1e6; 
            AOT_1D_tile_clip=AOT_1D_mean(ind_temp,:);  
            AOD_stage3_day.AOD(ind_GUID_temp)=AOT_1D_tile_clip;           
        end        
    end 
    
    AOD_stage3_year=[AOD_stage3_year;AOD_stage3_day];
    toc
end