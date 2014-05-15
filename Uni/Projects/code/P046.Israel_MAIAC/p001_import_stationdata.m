cd  '/media/NAS/Uni/Data/Israel/IPA_stations/';

load('SO2.mat')
cell2csv('SO2.csv',DATA,',');

load('WS.mat')
cell2csv('WS.csv',WS,',');