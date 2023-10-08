
# Load Data ---------------------------------------------------------------

# Load Heat Stress (Wet Bulb Global Temperature Rasters)
setwd("M:/GEOGlobal/HeatStress")


#first import all files in a single folder as a list 
rastlist_wbgt <- list.files(path = "M:/GEOGlobal/HeatStress", pattern='.tif$', 
                            all.files=TRUE, full.names=FALSE)

#import all raster files in folder using lapply
allrasters_wbgt <- lapply(rastlist_wbgt, raster)


# Read the shapefile grid
grid_sf <- st_read(file.path(mena_file_path, "Boundaries", "final", "grid_10km.shp"))



# Extract values from Heat Stress raster to grid --------------------------

##make sure the projection is the same
rasters_projected <- lapply(allrasters_wbgt, function(r) {
  projectRaster(r, crs = st_crs(grid_sf)$proj4string)
})

rasters_projected

# extract the values to the grid
grid_sf$temp_rp_100y <- exact_extract(rasters_projected[[1]],grid_sf,'mean')
grid_sf$temp_rp_20y <- exact_extract(rasters_projected[[2]],grid_sf,'mean')
grid_sf$temp_rp_5y <- exact_extract(rasters_projected[[3]],grid_sf,'mean')


# compute Expected Mean (create upper bound)

grid_sf$upper_bound <- grid_sf$temp_rp_100y*((1/20)-(1/100))+
  grid_sf$temp_rp_20y*((1/5)-(1/20)) + 
  grid_sf$temp_rp_5y*((1)-(1/5) + (1/100))



# Export ------------------------------------------------------------------
saveRDS(grid_sf, file.path(mena_file_path,
                           "Hazards",
                           "TEMP",
                           "final",
                           "grid_wbgt_10km.Rds"))

st_write(grid_sf, file.path(mena_file_path,
                           "Hazards",
                           "TEMP",
                           "final",
                           "grid_wbgt_10km.shp"))



# Plot --------------------------------------------------------------------
heat <- grid_sf %>% select(upper_bound)

