# ================================================================
# Script Name: 02_EXTRACT_FLOOD.R
# Purpose: Create a 10*10km grid of flood estimates from FATHOM
# Input Dataset: grid_10km.shp,FU_[RETURN PERIOD].tif
# Output Dataset: grid_flood_10km
# Author: Chitra Balasubramanian 
# Last Updated: 2024-08-05
# Time Taken: 
# ================================================================



#Increase R memory
rasterOptions(chunksize=1e+06, maxmemory=1e+09)

##################################################
# 0. Load data
##################################################



# Load Data ---------------------------------------------------------------
setwd(mena_file_path_flood)
all_tif_files <- list.files(mena_file_path_flood, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
fu_rasters <- grep("/FU_.*\\.tif$", all_tif_files, value = TRUE, perl = TRUE)
grid_sf <- st_read(file.path(mena_file_path, "Boundaries", "final", "grid_10km.shp"))


# Split the rasters and df by country -------------------------------------
# keep only the grid ID in grid sf
grid_sf_id <- grid_sf %>% dplyr::select(ISO_A2, grid_id)

# Split the data frame based on country codes
list_of_countries <- split(grid_sf, grid_sf$ISO_A2)

# Convert two-digit ISO_A2 codes in list_of_countries to ISO-3 codes using the countrycode function
three_digit_codes <- countrycode(names(list_of_countries), origin = 'iso2c', destination = 'iso3c')

# Rename the list_of_countries with the corresponding ISO-3 codes
names(list_of_countries) <- three_digit_codes

#Test: 
#list_of_countries <- list_of_countries[2:4]

# Extract country codes using a regex
country_codes <- gsub(".*raw/([A-Z]+)/.*", "\\1", fu_rasters)

# Use split to separate the file paths by country
fu_raster_list_country <- split(fu_rasters, country_codes)


#Load Population raster

pop <- rast(file.path(raw_replication,"POPULATION","ppp_2020_1km_Aggregated.tif"))



#################################################
# 1. Downscale Population to Flood Resolution: Calculation at rp*country level --------------------------------
#################################################
return_periods <- c("1in5", "1in10","1in20","1in50", "1in75","1in100", "1in200", "1in250","1in500", "1in1000")

# Create code for parallel processing -------------------------------------
# Number of cores to use
#numCores <- detectCores() - 1  # Reserve one core for system processes
#registerDoParallel(cores=numCores)

### Note for improvemen. The loop below does not use parallel processing
#foreach (rp=return_periods) %dopar% {
# It crashes on my computer and we may need to change the output code.

for (rp in return_periods) {
  
  # Skip the loop iteration if the return period is missing
  if (is.na(rp) || rp == "") {
    next
  }
  
  
  # Dynamically construct the file name to search for
  file_name_pattern <- sprintf("FU_%s.tif", rp)
  
  # Dynamically extract paths for the given return period
  extracted_paths <- sapply(fu_raster_list_country, function(paths) {
    return(paths[grep(file_name_pattern, paths)])
  })
  
  # Initialize an empty list to store extracted data for the current return period
  all_transformed_shp_list <- list()
  
  # Iterate over the country codes
  for (country_code in names(list_of_countries)) {
    print(rp)
    print(country_code)
    
    # Skip processing for "BHR" - Only Pluvial data for BHR
    if (country_code == "BHR") {
      next
    }
    
    # Check if raster exists for the country
    if (!is.null(extracted_paths[[country_code]])) {
      country_rast <- rast(extracted_paths[[country_code]])
      
      # Extract CRS from raster
      raster_crs <- crs(country_rast)
      
      # Transform the country shapefile to the raster CRS
      country_shp <- list_of_countries[[country_code]]
      transformed_country_shp <- st_transform(country_shp, crs=as.character(raster_crs))
      
      
      #Change values for -9999/999 [Convert no flood to 0]
      all_values <-values(country_rast)
      filtered_values<-all_values[all_values!=999]
      
      country_rast[country_rast == 999] <- max(filtered_values)
      country_rast[country_rast == -9999] <- 0
      
      # extract flood level to the shapefile
      transformed_country_shp$grid_flood <- exact_extract(country_rast,transformed_country_shp,'mean')
      
      # crop population 
      country_pop_raster <- crop(pop, transformed_country_shp)
      
      # Resampled population
      pop_resampled <- resample(country_pop_raster,country_rast, "near", threads =T)
      
      # categorical variable for flood (> 0.5)
      country_flood <- ifel(country_rast >0.5, 1,0)
      
      # Check: country_flood2 <- ifel(country_rast >-1, 1,0)
      # Check: country_flood2[is.na(country_rast)] <- 1
      
      #Reweight population to take care of resampling (assuming homogenous distribution)
      weight<-(res(country_rast)[1]/res(country_pop_raster)[1])^2
      
      #multiply population with flood cat
      exp <- pop_resampled*country_flood*weight
      # Check exp2 <- pop_resampled*country_flood2*weight
      
      
      #extract exposed people to shapefile
      transformed_country_shp$grid_pop_flood <- exact_extract(exp,transformed_country_shp,'sum')
      # Check: grid_pop_flood2 <- exact_extract(exp2,transformed_country_shp,'sum')
      
      # Population with the resampling method 
      ##Note to improve: can be calculated only for one return period
      transformed_country_shp$pop_resampled10km <-exact_extract(pop_resampled*weight,transformed_country_shp,'sum')
      
      # Add return period and country code as columns
      transformed_country_shp$return_period <- rp
      transformed_country_shp$country_code <- country_code
      
      # Export ------------------------------------------------------------------
      saveRDS(transformed_country_shp, file.path(raw_replication,
                                                 "FLOOD",
                                                 "temp",
                                                 paste0("grid_flood_10km_resampled_", rp,"_", country_code, ".Rds")))
      
      
      gc()
    }
  }
}
