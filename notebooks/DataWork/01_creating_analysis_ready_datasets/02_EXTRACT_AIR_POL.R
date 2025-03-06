# ================================================================
# Script Name: 02_extract_air_pol.R
# Purpose: Creates 10*10lm grid of PM2.5
# Input Dataset: grid_10km.shp, raster files of PM2.5(Source: Van Donkelaar et. al)
# Output Dataset: grid_PM25_10km.Rds
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
#Time Taken: 4.5 minutes
# ================================================================

system.time({
# Parellelization -----------------------------------------------------------

# Register parallel backend
n_cores <- parallel::detectCores() - 1  # Leave one core free
doParallel::registerDoParallel(cores = n_cores)





# 1. Load Data ---------------------------------------------------------------

#Load air pollution rasters
setwd(mena_file_path_air_pol)

# List the files in the directory
nc_files <- list.files(mena_file_path_air_pol, pattern = "*.nc",recursive = TRUE, full.names = T)


# Read the shapefile grid
grid_sf <- st_read(file.path(final_replication, "grid_10km.shp"))


# Define a function to process each raster file
process_raster <- function(nc_file, grid_sf) {
  # Load raster
  r <- raster(nc_file, varname = "GWRPM25")
  
  # Extract the year from the file path
  year <- regmatches(nc_file, regexpr("\\d{4}(?=01-\\d{4})", nc_file, perl = T))
  
  
  # Change projection of grid
  grid_sf_transformed <- st_transform(grid_sf, crs = crs(r))
  
  # Extract and compute the mean
  year_col_name <- paste0("pm25_year_", year)
  extracted_values <- exact_extract(r, grid_sf_transformed, 'mean')
  
  list(year_col_name = year_col_name, extracted_values = extracted_values)
}

# Run parallel computation
results <- foreach(nc_file = nc_files, .packages = c("raster", "sf", "exactextractr")) %dopar% {
  process_raster(nc_file,grid_sf)
}


# Merge results back into grid_sf
for (res in results) {
  grid_sf[[res$year_col_name]] <- res$extracted_values
}





grid_sf


# Compute annual mean -----------------------------------------------------
grid_df <- grid_sf %>%
  st_drop_geometry()


# Define the year range
years <- 1998:2021

# Create the column names for PM2.5 values for each year
year_cols_names <- paste0("pm25_year_", years)

#compute mean across all years[1998-2021]
grid_df$annual_mean_pm25 = rowMeans(grid_df[, year_cols_names], na.rm = TRUE)

#add var to sf
grid_sf$annual_mean_pm25 <- grid_df$annual_mean_pm25


#compute mean across all years[2015-2021]
column_names <- c("pm25_year_2015", "pm25_year_2016", "pm25_year_2017", "pm25_year_2018", "pm25_year_2019", 
                  "pm25_year_2020", "pm25_year_2021")
grid_sf$annual_mean2_pm25 <- rowMeans(st_drop_geometry(grid_sf[,column_names]), na.rm = TRUE)








# Export ------------------------------------------------------------------

saveRDS(grid_sf, file.path(final_replication,
                           "grid_PM25_10km.Rds"))

})
