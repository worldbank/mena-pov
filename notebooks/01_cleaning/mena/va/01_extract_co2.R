library(raster)
library(sf)

# Set working directory and list raster files
setwd("M:/MENA/GEO/Hazards/CO2/monthly")
raster_files <- list.files(pattern = "\\.nc4$")

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
  
  # Extract raster values for each polygon in MENA_shp
  values_list <- extract(cropped_rast, MENA_sp)
  
  # Compute statistics for each list of values and associate with ID_ADM
  stats_df <- data.frame(
    ID_ADM = MENA_shp$ID_ADM,
    Mean = sapply(values_list, function(vals) mean(vals, na.rm = TRUE)),
    Min = sapply(values_list, function(vals) min(vals, na.rm = TRUE)),
    Max = sapply(values_list, function(vals) max(vals, na.rm = TRUE)),
    Median = sapply(values_list, function(vals) median(vals, na.rm = TRUE)),
    SD = sapply(values_list, function(vals) sd(vals, na.rm = TRUE)),
    month_year = sub(".*_month_(\\d{6}).*", "\\1", rast_file)
  )
  
  # Add the dataframe to the list
  df_list[[rast_file]] <- stats_df
  
  # Update the progress bar
  setTxtProgressBar(pb, i)
}

# Close the progress bar
close(pb)

# Combine the individual dataframes into one dataframe
final_df <- do.call(rbind, df_list)

# View the final dataframe
head(final_df)
