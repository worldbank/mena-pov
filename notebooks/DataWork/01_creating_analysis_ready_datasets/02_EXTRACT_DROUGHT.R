# ================================================================
# Script Name: 02_EXTRACT_DROUGHT.R
# Purpose: Create a 10*10km grid with drought frequency values from FAO
# Input Dataset: grid_10km.shp,FAO_ASI.tif
# Output Dataset: grid_drought_10km.Rds
# Author: Chitra Balasubramanian / Sandra Baquie
# Last Updated: 2024-08-05
# Time taken: 4 mins
# ================================================================
system.time({
  

# Parellelization -----------------------------------------------------------

# Register parallel backend
n_cores <- parallel::detectCores() - 1  # Leave one core free
doParallel::registerDoParallel(cores = n_cores)

# 1. Load data from FAO WMS ---------------------------------------------------------------

# Read the shapefile grid
grid_sf <- st_read(file.path(final_replication, "grid_10km.shp"))





# Load Data ---------------------------------------------------------------
drought_asi<-raster(file.path(raw_replication, "DROUGHT","FAO_ASI.tif"))




# 2. Extract values from Heat Stress raster to grid --------------------------

#make sure the projection is the same
drought_asi_prj <- projectRaster(drought_asi, crs = st_crs(grid_sf)$proj4string)

# Replace any values less than 0 and greater than 100 with NA
drought_asi_prj[drought_asi_prj < 0 | drought_asi_prj > 100] <- NA

# extract the values to the grid
grid_sf$drought_freq <- exact_extract(drought_asi_prj,grid_sf,'mean')



# Compute the drought frequency intervals ---------------------------------
breaks <- c(0, 5, 10, 15, 20, 25, 30,100)
labels <- c("<5", "5-10", "11-15", "16-20", "21-25", "26-30", ">=31")
grid_sf$discrete_drought_freq <- cut(grid_sf$drought_freq, breaks = breaks, 
                                     labels = labels, right = FALSE, include.lowest = TRUE)



# Export ------------------------------------------------------------------
saveRDS(grid_sf, file.path(final_replication,
                           "grid_drought_10km.Rds"))



})
