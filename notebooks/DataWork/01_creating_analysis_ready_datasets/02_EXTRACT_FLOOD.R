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

system.time ({
  

# Load Data ---------------------------------------------------------------
setwd(mena_file_path_flood)
all_tif_files <- list.files(mena_file_path_flood, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
fu_rasters <- grep("/FU_.*\\.tif$", all_tif_files, value = TRUE, perl = TRUE)
grid_sf <- st_read(file.path(final_replication, "grid_10km.shp"))


# Split the rasters and df by country -------------------------------------
# keep only the grid ID in grid sf
grid_sf_id <- grid_sf %>% dplyr::select(ISO_A2, grid_id)

# Split the data frame based on country codes
list_of_countries <- split(grid_sf, grid_sf$ISO_A2)


# Convert two-digit ISO_A2 codes in list_of_countries to ISO-3 codes using the countrycode function
three_digit_codes <- countrycode(names(list_of_countries), origin = 'iso2c', destination = 'iso3c')

# Rename the list_of_countries with the corresponding ISO-3 codes
names(list_of_countries) <- three_digit_codes


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
# # Number of cores to use
# numCores <- detectCores() - 1  # Reserve one core for system processes
# registerDoParallel(cores=numCores)
# 
# ### Note for improvemen. The loop below does not use parallel processing
# #foreach (rp=return_periods) %dopar% {
# # It crashes on my computer and we may need to change the output code.

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
      saveRDS(transformed_country_shp, file.path(raw_replication, "FLOOD",
                                                 "temp",
                                                 paste0("grid_flood_10km_resampled_", rp,"_", country_code, ".Rds")))
      
      
      gc()
    }
  }
}

#################################################
# 2. Append all of them --------------------------------
#################################################

# Initialize an empty list to store all the extracted data for all return periods
all_periods_data_list <- list()

# Combine the extracted data from all countries into a single data frame for the current return period

for (rp in return_periods) {
  
  # Skip the loop iteration if the return period is missing
  if (is.na(rp) || rp == "") {
    next
  }
  
  if (rp == "1in5") {
    
    combined_transformed_data <-  readRDS(file.path(raw_replication, "FLOOD",
                                                    "temp",
                                                    paste0("grid_flood_10km_resampled_", rp, ".Rds")))
    
    all_periods_data_list[[rp]] <- combined_transformed_data
  }
  else {
    
    all_transformed_shp_list <- list()
    
    # Iterate over the country codes
    for (country_code in names(list_of_countries)) {
      print(rp)
      print(country_code)
      
      # Skip processing for "BHR" - Only Pluvial data for BHR
      if (country_code == "BHR") {
        next
      }
      transform_country_shp <-  readRDS(file.path(raw_replication, "FLOOD",
                                                  "temp",
                                                  paste0("grid_flood_10km_resampled_", rp,"_", country_code, ".Rds")))
      
      all_transformed_shp_list[[country_code]] <- transform_country_shp
      
    }
    
    combined_transformed_data <- do.call(rbind, all_transformed_shp_list)
    # Store the combined data frame in the all periods list
    all_periods_data_list[[rp]] <- combined_transformed_data
  }
}

#all_periods_data_list

# Stop the parallel cluster
#stopImplicitCluster()

#######################################
# 3. Clean dataset
#######################################

# Convert the list into a dataframe ---------------------------------------

merged_data <- as.data.frame(all_periods_data_list)



# Cleaning variable names -------------------------------------------------

# Step 1: Clean variable names
names(merged_data) <- gsub("^X", "", names(merged_data))

# Step 2: Select and rename certain variables
# Define the return periods you want to handle

return_periods <- c("1in5","1in10","1in20","1in50", "1in75","1in100", "1in200", "1in250","1in500", "1in1000")

# Base variables to select for each return period
base_vars <- c("grid_flood", "grid_pop_flood", "pop_resampled10km")

# Generate variable names dynamically
vars_to_select <- unlist(lapply(return_periods, function(rp) {
  paste0(rp, ".", base_vars)
}))


# Add other variables that are constant across return periods
constant_vars <- c("1in5.OBJECTID", "1in5.ADM0_CODE", "1in5.ADM1_CODE", 
                   "1in5.ADM2_CODE", "1in5.ISO_A2", "1in5.WB_ADM1_CO", 
                   "1in5.WB_ADM0_CO", "1in5.WB_ADM0_NA", "1in5.WB_ADM1_NA", 
                   "1in5.WB_ADM2_CO", "1in5.WB_ADM2_NA", "1in5.ID_ADM", 
                   "1in5.grid_id")

# Combine all variables to select
all_vars_to_select <- c(constant_vars, vars_to_select)

# Select the variables from merged_data
merged_data <- merged_data %>%
  dplyr::select(all_of(all_vars_to_select)) %>%
  rename_with(~ sub("1in5\\.", "", .x), all_of(gsub("`", "", constant_vars)))

#############################################
# 4. Expected flood level and exposed people
###############################################
## Update names
for (number in c(5, 10, 20, 50, 75, 100, 200, 250, 500, 1000)) {
  for (var in c("grid_flood", "grid_pop_flood", "pop_resampled10km")) {
    old_col_name <- paste0("1in", number, ".", var)
    new_col_name <- paste0(var,"_rp", number)
    names(merged_data)[names(merged_data) == old_col_name] <- new_col_name
  }
}

# Compute lower bound
merged_data <- merged_data %>%
  dplyr::mutate(grid_flood_lower_bound = grid_flood_rp1000*(1/1000)+ grid_flood_rp500*(1/500-1/1000)+ grid_flood_rp250*(1/250-1/500)+
                  grid_flood_rp200*(1/200-1/250)+grid_flood_rp100*(1/100-1/200)+grid_flood_rp75*(1/75-1/100)+grid_flood_rp50*(1/50-1/75)+grid_flood_rp20*(1/20-1/50)+
                  grid_flood_rp10*(1/10-1/20)+grid_flood_rp5*(1/5-1/10)+0*(1-1/5))

merged_data <- merged_data %>%
  dplyr::mutate(grid_pop_flood_lower_bound = grid_pop_flood_rp1000*(1/1000)+ grid_pop_flood_rp500*(1/500-1/1000)+ grid_pop_flood_rp250*(1/250-1/500)+
                  grid_pop_flood_rp200*(1/200-1/250)+grid_pop_flood_rp100*(1/100-1/200)+grid_pop_flood_rp75*(1/75-1/100)+grid_pop_flood_rp50*(1/50-1/75)+grid_pop_flood_rp20*(1/20-1/50)+
                  grid_pop_flood_rp10*(1/10-1/20)+grid_pop_flood_rp5*(1/5-1/10)+0*(1-1/5))

# Compute upper bound
merged_data <- merged_data %>%
  dplyr::mutate(grid_flood_upper_bound = grid_flood_rp1000*(1/500)+ grid_flood_rp500*(1/250-1/500)+
                  grid_flood_rp250*(1/200-1/250)+grid_flood_rp200*(1/100-1/200)+grid_flood_rp100*(1/75-1/100)+grid_flood_rp75*(1/50-1/75)+grid_flood_rp50*(1/20-1/50)+
                  grid_flood_rp20*(1/10-1/20)+grid_flood_rp10*(1/5-1/10)+0*(1-1/5))

merged_data <- merged_data %>%
  dplyr::mutate(grid_pop_flood_upper_bound = grid_pop_flood_rp1000*(1/500)+ grid_pop_flood_rp500*(1/250-1/500)+
                  grid_pop_flood_rp250*(1/200-1/250)+grid_pop_flood_rp200*(1/100-1/200)+grid_pop_flood_rp100*(1/75-1/100)+grid_pop_flood_rp75*(1/50-1/75)+grid_pop_flood_rp50*(1/20-1/50)+
                  grid_pop_flood_rp20*(1/10-1/20)+grid_pop_flood_rp10*(1/5-1/10)+0*(1-1/5))
# We still assume that the last rectangle is 0; ow, we overestimate by a lot.

# compute the average
merged_data$grid_flood_expected<-(merged_data$grid_flood_upper_bound+merged_data$grid_flood_lower_bound)/2
merged_data$grid_pop_flood_expected<-(merged_data$grid_pop_flood_upper_bound+merged_data$grid_pop_flood_lower_bound)/2


# convert to sf
grid_sf_sub <- grid_sf %>% dplyr::select(grid_id,geometry)

merged_data <- merged_data %>%
  left_join(grid_sf_sub, by = c("grid_id"))


##################################
# 5. Export ------------------------------------------------------------------
##################################

saveRDS(merged_data, file.path(final_replication,
                               "grid_flood_10km.Rds"))


})

