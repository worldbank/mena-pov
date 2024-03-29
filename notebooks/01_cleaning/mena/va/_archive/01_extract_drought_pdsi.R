# Extract Palmer Severity Drought Index

# Set working directory and list raster files
setwd("M:/MENA/GEO/Hazards/PDSI/raw")
raster_files <- list.files(pattern = "\\.tif$")
MENA_shp <- st_read(file.path(mena_file_path,
                              "Boundaries",
                              "MENA_ADM2.shp"))

# Initialize an empty list to store the individual dataframes
df_list <- list()

# Initialize a progress bar
pb <- txtProgressBar(min = 0, max = length(raster_files), style = 3)

# Loop through each raster file
for (i in seq_along(raster_files)) {
  rast_file <- raster_files[i]
  
  # Load the .nc4 file as a raster
  rast <- raster(rast_file)
  
  # Ensure CRS of raster matches that of the shapefile
  raster_crs <- crs(rast)
  if (raster_crs != st_crs(MENA_shp)) {
    MENA_shp <- st_transform(MENA_shp, raster_crs)
  }
  
  # Convert sf object to Spatial object for compatibility with raster package
  MENA_sp <- as(MENA_shp, "Spatial")
  
  # Crop the raster
  cropped_rast <- crop(rast, extent(MENA_sp))
  
  # Extract the mean values using bilinear interpolation
  results <- extract(cropped_rast, MENA_shp, method = "bilinear", fun = mean, na.rm = TRUE)
  
  df_results <- data.frame(ID_ADM = MENA_shp$ID_ADM, Mean = results)
  
  
  # Add the dataframe to the list
  df_list[[rast_file]] <- df_results
  
  # Update the progress bar
  setTxtProgressBar(pb, i)
}

# Close the progress bar
close(pb)

# Combine the individual dataframes into one dataframe
final_df <- do.call(rbind, df_list)
