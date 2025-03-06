# ================================================================
# Script Name: 02_EXTRACT_HEALTH_FACILITIES.R
# Purpose: Create shapefiles with health facilities across MENA
# Input Dataset: gis_osm_buildings_a_free_1.shp,gis_osm_pois_a_free_1.shp,grid_10km.Rds
# Output Dataset: mena_health_facilities.Rds
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-06
# Time Taken: 15 mins
# ================================================================

system.time({
  


# Creating a function to import files and filter by country ---------------

# Define the function to process shapefiles for a given country
process_health_facilities <- function(country_name, mena_file_path) {
  
  # Read the buildings shapefile
  buildings <- st_read(file.path(raw_replication,"OSM","raw", country_name,
                                 "gis_osm_buildings_a_free_1.shp"))
  
  # Read the landuse shapefile
  places <- st_read(file.path(raw_replication,"OSM", "raw", country_name,
                              "gis_osm_pois_a_free_1.shp"))
  
  # Filter for health facilities in landuse
  health_facilities <- places %>% 
    dplyr::filter(fclass == "hospital" | fclass == "clinic" | fclass == "doctors")
  
  # Rename vars
  health_facilities <- health_facilities %>% 
    dplyr::rename(type = fclass)
  
  health_facilities2 <- buildings %>% 
    dplyr::filter(type == "hospital" | type == "clinic") %>%
    dplyr::select(-fclass)
  
  # Combine both datasets
  combined_health_facilities <- rbind(health_facilities, health_facilities2)
  
  # Save the combined dataset to a new shapefile
  st_write(combined_health_facilities, 
           file.path(raw_replication,"OSM", "raw",
                     country_name,
                     paste0("health_facilities_", country_name, ".shp")), 
           delete_layer = TRUE)
}

#"
# List of countries to process
countries <- c("Algeria", "Djibouti", "GCC", "Arab Republic of Egypt", 
               "Morocco","Islamic Republic of Iran", "Iraq","Israel and Palestine",
               "Jordan", "Lebanon", "Libya","Syrian Arab Republic",
               "Tunisia", "Republic of Yemen") # Extend this list as needed


# Loop over the list of countries and process each one
for(country in countries) {
  process_health_facilities(country,mena_file_path)
}



# Separate countries from GCC and Israel and Palestine --------------------
health_gcc <- st_read(file.path(raw_replication,"OSM","raw", "GCC", "health_facilities_GCC.shp"))
health_isr_pal <- st_read(file.path(raw_replication,"OSM","raw", "Israel and Palestine", "health_facilities_Israel and Palestine.shp"))


#grid
grid_sf <- readRDS(file.path(final_replication,"grid_10km.Rds")) 
countries_gcc <- c("Saudi Arabia", "Oman","United Arab Emirates", "Qatar",  
                   "Bahrain", "Kuwait")


for (country in countries_gcc) {
  # Filter grid_sf for the current country
  grid_sf_country <- grid_sf %>% 
    filter(WB_ADM0_NA == country)
  
  # Transform grid_sf to EPSG 4326
  grid_sf_country <- st_transform(grid_sf_country, 4326)
  
  # Crop health_gcc based on the filtered grid
  health_fac <- st_crop(health_gcc, grid_sf_country)
  
  
  # Save to a new shapefile
  st_write(health_fac, 
           file.path(raw_replication,"OSM", "raw",
                     country,
                     paste0("health_facilities_", country, ".shp")), 
           delete_layer = TRUE)
  
}

countries_isr_pal <- c("Israel", "West Bank and Gaza")

# Iterate over each country
for (country in countries_isr_pal) {
  # Filter grid_sf for the current country
  grid_sf_country <- grid_sf %>% 
    filter(WB_ADM0_NA == country)
  
  # Transform grid_sf to EPSG 4326
  grid_sf_country <- st_transform(grid_sf_country, 4326)
  
  # Crop health_gcc based on the filtered grid
  health_isr_pal$indicator <- st_within(health_isr_pal,grid_sf_country)%>% lengths > 0
  health_fac <- health_isr_pal %>% filter(indicator == T)
  
  
  # Save to a new shapefile
  st_write(health_fac, 
           file.path(raw_replication,"OSM","raw",
                     country,
                     paste0("health_facilities_", country, ".shp")), 
           delete_layer = TRUE)
  
}





# Import the created health facilties -------------------------------------

# Function to read a shapefile, add a country_name column, and return the modified sf object
read_shapefile_and_add_country <- function(country, base_path) {
  file_path <- file.path(raw_replication,"OSM", "raw", country, paste0("health_facilities_", country, ".shp"))
  # Read the shapefile
  sf_data <- st_read(file_path, quiet = TRUE)
  # Add the country_name column
  sf_data$country_name <- country
  return(sf_data)
}

# Read each shapefile, add the country name, and store the results in a list
countries_all <- c("Algeria", "Djibouti","Arab Republic of Egypt", 
                   "Morocco","Islamic Republic of Iran", "Iraq",
                   "Jordan", "Lebanon", "Libya","Syrian Arab Republic",
                   "Tunisia", "Republic of Yemen","Saudi Arabia", "Oman","United Arab Emirates", "Qatar",  
                   "Bahrain", "Kuwait", "Israel","West Bank and Gaza") 

shapefiles_list <- lapply(countries_all, read_shapefile_and_add_country, base_path = raw_replication)

#clean lists to have equal number of columns
shapefiles_list[[19]] <- shapefiles_list[[19]] %>% dplyr::select(-indicator)
shapefiles_list[[20]] <- shapefiles_list[[20]] %>% dplyr::select(-indicator)


combined_shapefile <- do.call(rbind, shapefiles_list)


# Export ------------------------------------------------------------------

# Save the combined shapefile as an Rds file
saveRDS(combined_shapefile, file.path(final_replication,
                                      "mena_health_facilities.Rds"))



})
