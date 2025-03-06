# ================================================================
# Script Name: 00_EXTRACT_HEAT_STRESS.R
# Purpose: Create a 10*10km grid with extreme heat stress index 
# Input Dataset: global_file_path_heatstress, pattern='.tif$', grid_10km.shp
# Output Dataset: grid_wbgt_10km.Rds
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-06
# Time Taken: 2 mins
# ================================================================

system.time({
  

# 1. Load Data ---------------------------------------------------------------

# Load Heat Stress (Wet Bulb Global Temperature Rasters)
setwd(mena_file_path_heat)

#first import all files in a single folder as a list 
rastlist_wbgt <- list.files(path = mena_file_path_heat, pattern='.tif$', 
                            all.files=TRUE, full.names=FALSE)



#import all raster files in folder using lapply
allrasters_wbgt <- lapply(rastlist_wbgt, raster)

# Read the shapefile grid
grid_sf <- st_read(file.path(final_replication, "grid_10km.shp"))







# 2. Extract values from Heat Stress raster to grid --------------------------

##make sure the projection is the same
rasters_projected <- lapply(allrasters_wbgt, function(r) {
  projectRaster(r, crs = st_crs(grid_sf)$proj4string)
})

rasters_projected

# extract the values to the grid
grid_sf$temp_rp_100y <- exact_extract(rasters_projected[[1]],grid_sf,'mean')
grid_sf$temp_rp_20y <- exact_extract(rasters_projected[[2]],grid_sf,'mean')
grid_sf$temp_rp_5y <- exact_extract(rasters_projected[[3]],grid_sf,'mean')






# 3. Compute Expected Mean (create upper bound) --------------------------
grid_sf$upper_bound <- grid_sf$temp_rp_100y*((1/20))+
  grid_sf$temp_rp_20y*((1/5)-(1/20)) + 
  grid_sf$temp_rp_5y*((1)-(1/5))






# 4. Explore outliers ----------------------------------------------------

summary(grid_sf$upper_bound)
hist(grid_sf$upper_bound, main = "Histogram of Upper Bound", xlab = "Value")

## check upper bound below 10
# Most areas with temp below 0 are coastal cities
up_bound_below_10 <- grid_sf %>%
  filter(upper_bound < 10)






# 5. Export ------------------------------------------------------------------
saveRDS(grid_sf, file.path(final_replication,
                           "grid_wbgt_10km.Rds"))


})
