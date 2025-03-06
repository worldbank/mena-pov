# ================================================================
# Script Name: 02_EXTRACT_CROP_YIELDS.R
# Purpose: Create a 10*10km grid with crop yield ratio data from FAO
# Input Dataset: grid_10km.shp,all_2010_yld.tif
# Output Dataset: grid_yield_achievement_ratio_10km.Rds
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# Time taken: 1 min
# ================================================================

system.time({
  


options(scipen = 999)

# 1. Load data from FAO WMS ---------------------------------------------------------------


grid_sf <- st_read(file.path(final_replication, "grid_10km.shp")) # Read the shapefile grid
yield_raster <- raster(file.path(raw_replication,"CROP_YIELD","all_2010_yld.tif")) # crop yield raster 2010


plot(yield_raster)
summary(yield_raster)



# Extract Yield Values to Grid --------------------------------------------


yield_raster_prj <- projectRaster(yield_raster, crs = "+init=epsg:4326") #make sure the projection is the same


yield_raster_prj[yield_raster_prj < 0] <- NA # Note: keep higher than 100. There is no value below 0.

# extract the values to the grid
grid_sf$yield_ach_ratio <- exact_extract(yield_raster_prj,grid_sf,'mean')
summary(grid_sf$yield_ach_ratio)




# Export ------------------------------------------------------------------
saveRDS(grid_sf, file.path(final_replication,
                           "grid_yield_achievement_ratio_10km.Rds"))
})
