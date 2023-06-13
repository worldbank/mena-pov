## Merge datasets


# Load Data --------------------------------------------------------------------
seg1 <- st_read(file.path(lbn_file_path,
                          "Team",
                          "Projects",
                          "Sampling",
                          "01- LVAP Listing map data",
                          "Phase1",
                          "Segment",
                          "Shapefile",
                          "Segmentation_Phase1.shp"))


seg2 <- st_read(file.path(lbn_file_path,
                          "Team",
                          "Projects",
                          "Sampling",
                          "01- LVAP Listing map data",
                          "Phase2",
                          "Segment",
                          "Shapefile",
                          "Segmentation_Phase2.shp"))

## merge the two shapefiles
merged_seg <- rbind(seg1, seg2)

## subset data
merged_seg <- merged_seg %>%
  select(OBJECTID,UID_Name,Layer_name,geometry,UID) %>%
  mutate(id = seq_len(nrow(merged_seg)))


# Loading hazard data -----------------------------------------------------
IN_PATH <- file.path(lbn_file_path,
                     "Hazards",
                     "final")

list_names <- c("co2_2020.Rds", "drought_2020.Rds", "floodrisk.Rds", 
                "no2_2020.Rds", "pm2_5_2019.Rds", "temp_2020.Rds")

for(list_name_i in list_names){
  print(list_name_i)
  
  data_i <- readRDS(file.path(IN_PATH, list_name_i)) %>% as.data.table
  merged_seg <- merge(merged_seg, data_i, by="id") %>%
    select(-ends_with(c(".y", ".x")))
  
  # Cleanup as memory intensive
  rm(data_i)
  gc()
  
}


# Clean dataset -----------------------------------------------------------
clean_df <- merged_seg %>%
  select(UID,year,co2_value,avg_drought_value,min_drought_value,max_drought_value,no2_value,pm2_5_value,avg_temp,
         avg_temp_diff,min_temp_diff,max_temp_diff,
         FU_1in5,FU_1in10,FU_1in20,FU_1in50,FU_1in75,FU_1in100,
         FU_1in250,FU_1in500,FU_1in1000,P_1in5,P_1in10,P_1in20,
         P_1in50,P_1in75,P_1in100,P_1in250,P_1in500,P_1in1000)

clean_df<- st_drop_geometry(clean_df)

# Export ------------------------------------------------------------------
saveRDS(clean_df, file.path(lbn_file_path,
                            "Hazards",
                            "final",
                            "lbn_hazards_2020_bysegment.Rds"))

write.csv(clean_df,file.path(lbn_file_path,
                             "Hazards",
                             "final",
                             "lbn_hazards_2020_bysegment.csv"), row.names = F)




