
# Load Data ---------------------------------------------------------------

# Read the shapefile grid
grid_sf <- st_read(file.path(mena_file_path, "Boundaries", "final", "grid_10km.shp"))



# Load RWI rasters
setwd("M:/MENA/GEO/RWI/raw")

#first import all files in a single folder as a list 
list_rwi <- list.files(path ="M:/MENA/GEO/RWI/raw" , pattern='.csv$', 
                            all.files=TRUE, full.names=FALSE)


all_rwi_rasters <- vector("list", length(list_rwi))

for (i in seq_along(list_rwi)) {
  sFile <- list_rwi[i]
  sfRWI <- read.csv(sFile) %>% st_as_sf(coords=c("longitude","latitude"), crs=4326)
  rRWI <- terra::rasterize(sfRWI, rast(extent=ext(sfRWI), resolution=c(0.021972700, 0.021715400)), field="rwi")
  
  all_rwi_rasters[[i]] <- rRWI
}

list_rwi

# Change projection of each raster ----------------------------------------
#all_rwi_rasters[[1]] <- projectRaster(all_rwi_rasters[[1]], crs = st_crs(grid_sf)$proj4string)



# Extract raster values to the grid ---------------------------------------

grid_sf$rwi_dza <- exact_extract(all_rwi_rasters[[1]], grid_sf,'mean')
grid_sf$rwi_dji <- exact_extract(all_rwi_rasters[[2]], grid_sf,'mean')
grid_sf$rwi_egy <- exact_extract(all_rwi_rasters[[3]], grid_sf,'mean')
grid_sf$rwi_jor <- exact_extract(all_rwi_rasters[[4]], grid_sf,'mean')
grid_sf$rwi_lbn <- exact_extract(all_rwi_rasters[[5]], grid_sf,'mean')
grid_sf$rwi_lby <- exact_extract(all_rwi_rasters[[6]], grid_sf,'mean')
grid_sf$rwi_mar <- exact_extract(all_rwi_rasters[[7]], grid_sf,'mean')
grid_sf$rwi_tun <- exact_extract(all_rwi_rasters[[8]], grid_sf,'mean')

grid_sf$rwi_all <- coalesce(grid_sf$rwi_dza,grid_sf$rwi_dji ,grid_sf$rwi_egy, 
                            grid_sf$rwi_jor, grid_sf$rwi_lbn,grid_sf$rwi_lby, grid_sf$rwi_mar, grid_sf$rwi_tun)

plot(grid_sf['rwi_all'])




# Export ------------------------------------------------------------------

saveRDS(grid_sf, file.path(mena_file_path,
                           "RWI",
                           "final",
                           "grid_rwi_10km.Rds"))

st_write(grid_sf, file.path(mena_file_path,
                            "RWI",
                            "final",
                            "grid_rwi_10km.shp"),append = F)



