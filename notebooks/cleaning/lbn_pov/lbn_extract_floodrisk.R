# Extract Fathom Flood data


# Load segment shapefile --------------------------------------------------
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


# Load fathom data --------------------------------------------------------
# Path to the fluvial and pluvial flood datasets
folder_path_fluvial <- file.path(lbn_file_path, "Hazards", "raw", "fathom_floods", "fluvial_undefended")
folder_path_pluvial <- file.path(lbn_file_path, "Hazards", "raw", "fathom_floods", "pluvial")

return_list <- c("1in5", "1in10", "1in20", "1in50", "1in75", "1in100", "1in250", "1in500", "1in1000")

raster_list <- list()
extracted_values <- list()


# Create empty dataframes for fluvial and pluvial data
fluvial_df <- data.frame()
pluvial_df <- data.frame()

for (return_val in return_list) {
  # Process fluvial rasters
  pattern_fluvial <- paste0("FU_", return_val, ".tif")
  files_fluvial <- list.files(path = folder_path_fluvial, pattern = pattern_fluvial, full.names = TRUE)
  
  # Read and process fluvial rasters
  for (file in files_fluvial) {
    raster <- raster(file)
    raster_list[[return_val]] <- raster
    
    cropped_raster <- crop(projectRaster(raster, crs = st_crs(merged_seg)), merged_seg)
    values <- extract(cropped_raster, merged_seg, fun = mean, na.rm = TRUE, df = FALSE)
    extracted_values[[return_val]] <- values
    
    # Create a dataframe for the current fluvial segment
    fluvial_segment_df <- data.frame(id = return_val, fluvial_values = values)
    fluvial_df <- rbind(fluvial_df, fluvial_segment_df)
  }
  
  # Process pluvial rasters
  pattern_pluvial <- paste0("P_", return_val, ".tif")
  files_pluvial <- list.files(path = folder_path_pluvial, pattern = pattern_pluvial, full.names = TRUE)
  
  # Read and process pluvial rasters
  for (file in files_pluvial) {
    raster <- raster(file)
    raster_list[[return_val]] <- raster
    
    cropped_raster <- crop(projectRaster(raster, crs = st_crs(merged_seg)), merged_seg)
    values <- extract(cropped_raster, merged_seg, fun = mean, na.rm = TRUE, df = FALSE)
    extracted_values[[return_val]] <- values
    
    # Create a dataframe for the current pluvial segment
    pluvial_segment_df <- data.frame(id = return_val, pluvial_values = values)
    pluvial_df <- rbind(pluvial_df, pluvial_segment_df)
  }
}

# Perform left join on segment_id
merged_df <- merge(fluvial_df, pluvial_df, by = "id", all.x = TRUE)





