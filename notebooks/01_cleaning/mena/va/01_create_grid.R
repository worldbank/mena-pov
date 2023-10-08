

# Load Data ---------------------------------------------------------------
# Read the shapefile
shp <- st_read(file.path(mena_file_path,
                         "Boundaries",
                         "raw",
                         "MENA_ADM2.shp"))




# Change Projection to UTM ------------------------------------------------
# Reproject shapefile to a CRS in meters. This step is crucial if your original data isn't already in a CRS that uses meters.
# Reproject shapefile to Albers Equal-Area Conic tailored for the Middle East
shp_proj <- st_transform(shp, crs = "+proj=aea +lat_1=20 +lat_2=40 +lon_0=45 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")



# Create grid -------------------------------------------------------------
grid_size <- c(10000, 10000)  # 10km x 10km
grid_sf <- st_make_grid(shp_proj, cellsize = grid_size, square = TRUE)


# intersecting the grid and polygon to ensure it has all the polygon attributes
grid_sf_intersection <- st_intersection(shp_proj,grid_sf)

# create a unique ID for the grid
grid_sf_intersection$grid_id <- 1:nrow(grid_sf_intersection)




# Export ------------------------------------------------------------------
# Save the grid as a new shapefile
saveRDS(grid_sf_intersection, file.path(mena_file_path,
                            "Boundaries",
                            "final",
                            "grid_10km.Rds"))


st_write(grid_sf_intersection, file.path(mena_file_path,
                    "Boundaries",
                    "final",
                    "grid_10km.shp"),append=FALSE)

