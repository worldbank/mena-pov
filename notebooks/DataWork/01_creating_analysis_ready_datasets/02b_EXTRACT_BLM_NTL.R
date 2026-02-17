# ================================================================
# Script Name: 02b_EXTRACT_BLM_NTL.R
# Purpose: Extract NASA Black Marble NTL data from 2012 to 2022
# Input Dataset: "grid_10km.shp"
# Output Dataset: VNP46A4_annual_rasters
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-06
# ================================================================



##Using a package called blackmarbler created by Rob Marty : https://github.com/worldbank/blackmarbler
##Note: Follow the instructions in the Github to generate token
bearer <- ""

# Load Data ---------------------------------------------------------------
grid_sf <- st_read(file.path(final_replication, "grid_10km.shp"))






# Downloading Black Marble Data -------------------------------------------
#transform the grid shapefile
grid_sf_prj <- st_transform(grid_sf, 4326)


ntl_df <- bm_raster(roi_sf = grid_sf_prj,
                    product_id = "VNP46A4",
                    date = 2012:2022,
                    bearer = bearer,
                    output_location_type = "file",
                    file_dir = file.path(raw_replication,"NTL", "raw", "VNP46A4_annual_rasters"))



