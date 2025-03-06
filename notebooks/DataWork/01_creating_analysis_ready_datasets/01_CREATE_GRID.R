# ================================================================
# Script Name: 01_CREATE_GRID.R
# Purpose: Create a 10km x 10km grid and intersect with shapefile
# Input Dataset: MENA_ADM2.shp
# Output Dataset: grid_10km.Rds, grid_10km.shp
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# Time taken to run script: Elapsed Time: 168 seconds
# ================================================================
system.time({

# Load Data ---------------------------------------------------------------
# Read the shapefile
shp <- st_read(file.path(raw_replication,"BOUNDARIES", "MENA_ADM2.shp"))





# Change Projection to UTM ------------------------------------------------
# Reproject shapefile to Albers Equal-Area Conic tailored for the Middle East
shp_proj <- st_transform(shp, crs = "+proj=aea +lat_1=20 +lat_2=40 +lon_0=45 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")






# Create Grid -------------------------------------------------------------
# Define grid size (10km x 10km)
grid_size <- c(10000, 10000)

# Create the grid
grid_sf <- st_make_grid(shp_proj, cellsize = grid_size, square = TRUE)



# Check the overlap
plot(shp_proj$geometry, col = "red")
plot(grid_sf, add = TRUE)

# Intersect the grid and polygon to ensure it has all the polygon attributes
grid_sf_intersection <- st_intersection(shp_proj, grid_sf)

# Check if intersection was successful
if (is.null(grid_sf_intersection)) stop("Intersection failed. Check the input geometries.")

# Create a unique ID for the grid
grid_sf_intersection$grid_id <- 1:nrow(grid_sf_intersection)






# Export ------------------------------------------------------------------
# Save the grid as an RDS file
saveRDS(grid_sf_intersection, file.path(final_replication, "grid_10km.Rds"))

# Save the grid as a shapefile
st_write(grid_sf_intersection, file.path(final_replication, "grid_10km.shp"), append = FALSE)

# Check if files were saved successfully
if (!file.exists(file.path(mena_file_path, "Boundaries", "final", "grid_10km.Rds"))) stop("RDS file was not saved successfully.")
if (!file.exists(file.path(mena_file_path, "Boundaries", "final", "grid_10km.shp"))) stop("Shapefile was not saved successfully.")

})
