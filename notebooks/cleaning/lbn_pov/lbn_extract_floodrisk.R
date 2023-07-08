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


for (return in return_list) {
  pattern <- paste0("FU_", return, ".tif")
  
  # Filter raster files based on pattern and date
  files <- list.files(path = folder_path_fluvial, pattern = pattern, full.names = TRUE)
  
  # Read the raster files
  for (file in files) {
    raster <- raster(file)
    raster_list[[return]] <- raster
    
    # Reproject the raster to match the shapefile
    raster_proj <- projectRaster(raster, crs = st_crs(merged_seg))
    
    # Clip the raster to the extent of the shapefile
    cropped_raster <- crop(raster_proj, merged_seg)
    
    # Extract raster values for the segments
    values <- extract(cropped_raster, merged_seg, fun = mean, na.rm = T, df = F)
    extracted_values[[return]] <- values
  }
}

# Convert the extracted raster values to a data frame
fluvial_df <- as.data.frame(do.call(cbind, lapply(extracted_values, function(x) unlist(x, use.names = FALSE))))

# Rename the columns of the data frame
colnames(fluvial_df) <- return_list

# Add a unique ID column
fluvial_df$id <- seq_len(nrow(fluvial_df))

# Rename the columns of the data frame with date_list
colnames(fluvial_df) <- c(paste0("FU_", return_list),"id")

## Pluvial
folder_path_pluvial <- file.path(lbn_file_path, "Hazards", "raw", "fathom_floods", "pluvial")

return_list <- c("1in5", "1in10", "1in20", "1in50", "1in75", "1in100", "1in250", "1in500", "1in1000")

raster_list <- list()
extracted_values <- list()


for (return in return_list) {
  pattern <- paste0("P_", return, ".tif")
  
  # Filter raster files based on pattern and date
  files <- list.files(path = folder_path_pluvial, pattern = pattern, full.names = TRUE)
  
  # Read the raster files
  for (file in files) {
    raster <- raster(file)
    raster_list[[return]] <- raster
    
    # Reproject the raster to match the shapefile
    raster_proj <- projectRaster(raster, crs = st_crs(merged_seg))
    
    # Clip the raster to the extent of the shapefile
    cropped_raster <- crop(raster_proj, merged_seg)
    
    # Extract raster values for the segments
    values <- extract(cropped_raster, merged_seg, fun = mean, na.rm = T, df = F)
    extracted_values[[return]] <- values
  }
}

# Convert the extracted raster values to a data frame
pluvial_df <- as.data.frame(do.call(cbind, lapply(extracted_values, function(x) unlist(x, use.names = FALSE))))

# Rename the columns of the data frame
colnames(pluvial_df) <- return_list

# Add a unique ID column
pluvial_df$id <- seq_len(nrow(pluvial_df))

# Rename the columns of the data frame with date_list
colnames(pluvial_df) <- c(paste0("P_", return_list),"id")


# Merge -------------------------------------------------------------------
flood_df <- merge(fluvial_df, pluvial_df, by = "id")


# Export ------------------------------------------------------------------
saveRDS(flood_df,file.path(lbn_file_path,
                           "Hazards",
                           "final",
                           "floodrisk.Rds"))

