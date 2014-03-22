
grid <- unique(ndvi, by="ndviid")
gridX <- grid[,NDVI :=NULL]

write.csv(gridX, "/home/zeltak/smb4k/ZUNISYN/ZUraid/Uni/Projects/P031_MIAC_PM/3.Work/2.Gather_data/FN007_Key_tables/ndvi_ID.csv")
