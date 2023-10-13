# Step 1: Load shapefile --------------------------------------------------
# Read the shapefile grid
grid_sf <- st_read(file.path(mena_file_path, "Boundaries", "final", "grid_10km.shp"))




# Step 2: Set Path for Rasters -------------------------------------------

# Define the main path for raster files
main_path <- "M:/MENA/GEO/Hazards/FLOOD/raw"

# List all .tif files in the directory
all_tif_files <- list.files(main_path, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

# Filter rasters with filenames starting with FU (Fluvial Undefended)
fu_rasters <- grep("/FU_.*\\.tif$", all_tif_files, value = TRUE)





# Step 3: Get Paths for Rasters by Return Period -------------------------
# Define unique return periods
unique_periods <- c("1in5","1in10","1in20","1in50","1in75", "1in100","1in200","1in250","1in500","1in1000")

# Initialize a list to store raster paths
raster_paths <- vector("list", length(unique_periods))
names(raster_paths) <- unique_periods

# Loop to generate raster paths
for (period in unique_periods) {
  raster_paths[[period]] <- grep(paste0("/FU_", period, "\\.tif$"), fu_rasters, value = TRUE)
  
  if(length(raster_paths[[period]]) == 0){
    warning(paste("No raster found for period:", period))
  }
}


# Step 4: Load Rasters ----------------------------------------------------
# Initialize a list to store raster objects
raster_list <- vector("list", length(unique_periods))
names(raster_list) <- unique_periods

# Loop to load the rasters into raster_list using raster_paths
for (period in unique_periods) {
  
  # Create a sub-list to store rasters for each country
  raster_list[[period]] <- vector("list", length(raster_paths[[period]]))
  
  # Loop through each path for the return period
  for(i in seq_along(raster_paths[[period]])) {
    raster_list[[period]][[i]] <- raster(raster_paths[[period]][i])
  }
  
  # Check if any raster was loaded for the return period
  if(all(sapply(raster_list[[period]], is.null))) {
    warning(paste("No raster could be loaded for period:", period))
  }
}





# Step 5: Extract rasters to shapefile ------------------------------------
grid_sf

