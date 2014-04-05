PROC IMPORT OUT= g1
            DATAFILE= "f:\Uni\Projects\P042_Medicare_THROM\3.1.10.1.Raw_data\grids\gird1.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

PROC IMPORT OUT= g2
            DATAFILE= "f:\Uni\Projects\P042_Medicare_THROM\3.1.10.1.Raw_data\grids\gird2_mia.dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

data g2 (drop=guid);
set g2;
guidx=guid*1;
run; 

data g2 (drop=guidx);
set g2;
guid=guidx;
run;

data g1 (drop=guid);
set g1;
guidx=guid;
run; 

data g1 (drop=guidx);
set g1;
guid=guidx;
run;  


data gg;
set g1 g2;
run; 

PROC EXPORT DATA= gg
            OUTFILE= "f:\Uni\Projects\P042_Medicare_THROM\3.1.10.1.Raw_data\grids\gg.csv" 
			            DBMS=CSV REPLACE;
						     PUTNAMES=YES;
							 RUN;
							  
