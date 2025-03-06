# ================================================================
# Script Name: 02_EXTRACT_RWI.R
# Purpose: Create a 10*10km grid with RWI values
# Input Dataset: grid_10km.shp ; RWI , pattern='.csv$'
# Output Dataset:  grid_rwi_10km.Rds
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-06
# Time Taken: 3 mins
# ================================================================


system.time({
  
  # 1. Load Data ---------------------------------------------------------------
  
  # Read the shapefile grid
  grid_sf <- st_read(file.path(final_replication, "grid_10km.shp"))
  
  # Load RWI CSV files
  setwd(mena_file_path_rwi)
  
  # First import all files in a single folder as a list 
  list_rwi <- list.files(path = mena_file_path_rwi, pattern = '.csv$', 
                         all.files = TRUE, full.names = TRUE)
  
  # Initialize list to store rasters
  all_rwi_rasters <- vector("list", length(list_rwi))
  
  for (i in seq_along(list_rwi)) {
    sFile <- list_rwi[i]
    
    # Load CSV and convert to sf object
    sfRWI <- read.csv(sFile) %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
    # Create an empty raster with the same extent as the data
    extent_rast <- terra::ext(sfRWI)
    r_template <- terra::rast(extent = extent_rast, resolution = c(0.021972700, 0.021715400))
    
    # Rasterize the sf object
    rRWI <- terra::rasterize(sfRWI, r_template, field = "rwi")
    
    # Store the raster in the list
    all_rwi_rasters[[i]] <- rRWI
  }
  
  # 2. Extract raster values to the grid ---------------------------------------
  
  grid_sf$rwi_dza <- exact_extract(all_rwi_rasters[[1]], grid_sf, 'mean')
  grid_sf$rwi_dji <- exact_extract(all_rwi_rasters[[2]], grid_sf, 'mean')
  grid_sf$rwi_egy <- exact_extract(all_rwi_rasters[[3]], grid_sf, 'mean')
  grid_sf$rwi_jor <- exact_extract(all_rwi_rasters[[4]], grid_sf, 'mean')
  grid_sf$rwi_lbn <- exact_extract(all_rwi_rasters[[5]], grid_sf, 'mean')
  grid_sf$rwi_lby <- exact_extract(all_rwi_rasters[[6]], grid_sf, 'mean')
  grid_sf$rwi_mar <- exact_extract(all_rwi_rasters[[7]], grid_sf, 'mean')
  grid_sf$rwi_tun <- exact_extract(all_rwi_rasters[[8]], grid_sf, 'mean')
  
  # Combine RWI values
  grid_sf$rwi_all <- coalesce(grid_sf$rwi_dza, grid_sf$rwi_dji, grid_sf$rwi_egy, 
                              grid_sf$rwi_jor, grid_sf$rwi_lbn, grid_sf$rwi_lby, 
                              grid_sf$rwi_mar, grid_sf$rwi_tun)
  
  # Plot the combined RWI
  plot(grid_sf['rwi_all'])
  
  # 3. Export ------------------------------------------------------------------
  
  saveRDS(grid_sf, file.path(final_replication, "grid_rwi_10km.Rds"))
  
})
