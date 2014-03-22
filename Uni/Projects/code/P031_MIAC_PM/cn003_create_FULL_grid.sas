



options mprint;
%macro import(Tile = );

PROC IMPORT OUT= Grid_h&Tile
            DATAFILE= "P:\P031_MIAC_PM\3.Work\2.Gather_data\FN007_Key_tables\Grid_h&Tile..dbf" 
			            DBMS=DBF   REPLACE;
						     GETDELETED=NO;
							 RUN; 

%mend;

%import(Tile = 00v00);
%import(Tile = 00v01);
%import(Tile = 00v02);
%import(Tile = 00v03);
%import(Tile = 00v04);
%import(Tile = 01v00);
%import(Tile = 01v01);
%import(Tile = 01v02);
%import(Tile = 01v03);
%import(Tile = 01v04);
%import(Tile = 02v00);
%import(Tile = 02v01);
%import(Tile = 02v02);
%import(Tile = 02v03);


data GRIDGRID;
set Grid_h00v00 Grid_h00v01 Grid_h00v02 Grid_h01v00 Grid_h01v01 Grid_h01v02 Grid_h02v00 Grid_h02v01 Grid_h02v02;
run; 

PROC EXPORT DATA= GRIDGRID 
            OUTFILE= "P:\P031_MIAC_PM\3.Work\2.Gather_data\FN007_Key_tables\EAST_USA_GRID.dbf" 
			            DBMS=DBF REPLACE;
						RUN;
						 

