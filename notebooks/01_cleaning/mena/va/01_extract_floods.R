# Required libraries
library(raster)
library(sf)

# The function defined earlier (provided again for context)
extract_raster_values <- function(raster_files, MENA_shp) {
  df_list <- list()
  pb <- txtProgressBar(min = 0, max = length(raster_files), style = 3)
  
  for (i in seq_along(raster_files)) {
    rast_file <- raster_files[i]
    rast <- raster(rast_file)
    
    raster_crs <- crs(rast)
    if (raster_crs != st_crs(MENA_shp)) {
      MENA_shp <- st_transform(MENA_shp, raster_crs)
    }
    
    MENA_sp <- as(MENA_shp, "Spatial")
    cropped_rast <- crop(rast, extent(MENA_sp))
    
    results <- extract(cropped_rast, MENA_sp, na.rm = TRUE) # Note: Extracting raw values without interpolation
    
    # Assuming each shape corresponds to multiple raster values, you may get a list of vectors
    # Here, I'm converting those lists to a mean value for simplicity; modify as needed
    means <- sapply(results, function(vals) mean(vals, na.rm = TRUE))
    
    
    df_results <- data.frame(ID_ADM = MENA_shp$ID_ADM, Mean = means)
    df_list[[rast_file]] <- df_results
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(df_list)
}

# Country codes
countries <- c("DZA", "BHR", "DJI")

# Overall results list
all_results <- list()

# Loop through each country
for (country in countries) {
  # Define the path for the raster files for the current country
  raster_dir <- paste0("//menapov/menapov/", country, "/GEO/Hazards/fathom_floods/pluvial")
  raster_files <- list.files(path = raster_dir, pattern = "\\.tif$", full.names = TRUE)
  
  # Load the MENA_shp shapefile for the current country (you'll need to adjust this if the shapefile naming is different)
  MENA_shp <- st_read(file.path(mena_file_path,
                                "Boundaries",
                                "MENA_ADM2.shp"))
  
  # Execute the function for the current country
  country_results <- extract_raster_values(raster_files, MENA_shp)
  
  # Store the results for the current country
  all_results[[country]] <- country_results
}

# Now all_results will contain the results for each country
