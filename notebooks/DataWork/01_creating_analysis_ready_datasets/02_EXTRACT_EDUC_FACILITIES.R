# ================================================================
# Script Name: 02_EXTRACT_EDUC_FACILITIES.R
# Purpose: Create shapefiles with location of education facilities
# Input Dataset: grid_10km.shp,gis_osm_buildings_a_free_1.shp,
# gis_osm_pois_a_free_1.shp
# Output Dataset: "educ_facilities_", country_name, ".shp"
# Author: Chitra Balasubramanian 
# Last Updated: 2024-08-05
# Time Taken: 17 mins
# ================================================================

system.time({
  


# Creating a function to import files and filter by country ---------------

# Define the function to process shapefiles for a given country
process_educ_facilities <- function(country_name, mena_file_path) {
  
  # Read the buildings shapefile
  buildings <- st_read(file.path(raw_replication,"OSM","raw", country_name,
                                 "gis_osm_buildings_a_free_1.shp"))
  
  # Read the pois shapefile
  places <- st_read(file.path(raw_replication,"OSM","raw",country_name,
                              "gis_osm_pois_a_free_1.shp"))
  
  # Filter for educ facilities
  educ_facilities <- places %>% 
    dplyr::filter(fclass == "university"|fclass == "school"|fclass=="college"|
                    fclass == "kindergarten")
  
  # Rename vars
  educ_facilities <- educ_facilities %>% 
    dplyr::rename(type = fclass)
  
  educ_facilities2 <- buildings %>% 
    dplyr::filter(type == "college"|type == "school"|type == "university"|
                    type == "kindergarten") %>%
    dplyr::select(-fclass)
  
  # Combine both datasets
  combined_educ_facilities <- rbind(educ_facilities, educ_facilities2)
  
  # Save the combined dataset to a new shapefile
  st_write(combined_educ_facilities, 
           file.path(raw_replication,"OSM","raw",
                     country_name,
                     paste0("educ_facilities_", country_name, ".shp")), 
           delete_layer = TRUE)
}

# 
# List of countries to process
countries <- c("Algeria","Djibouti", "GCC", "Arab Republic of Egypt", "Morocco",
               "Islamic Republic of Iran", "Iraq","Israel and Palestine",
               "Jordan", "Lebanon", "Libya","Syrian Arab Republic",
               "Tunisia", "Republic of Yemen") # Extend this list as needed


# Loop over the list of countries and process each one
for(country in countries) {
  process_educ_facilities(country, mena_file_path)
}



# Separate countries from GCC and Israel and Palestine --------------------
educ_gcc <- st_read(file.path(raw_replication,"OSM","raw", "GCC", "educ_facilities_GCC.shp"))
educ_isr_pal <- st_read(file.path(raw_replication,"OSM","raw", "Israel and Palestine", "educ_facilities_Israel and Palestine.shp"))


#grid
grid_sf <- readRDS(file.path(final_replication,
                             "grid_10km.Rds")) 

#"Saudi Arabia", "Oman","United Arab Emirates", "Qatar",
countries_gcc <- c(  
  "Bahrain", "Kuwait")



for (country in countries_gcc) {
  # Filter grid_sf for the current country
  grid_sf_country <- grid_sf %>% 
    filter(WB_ADM0_NA == country)
  
  # Transform grid_sf to EPSG 4326
  grid_sf_country <- st_transform(grid_sf_country, 4326)
  
  # Crop educ_gcc based on the filtered grid
  educ_fac <- st_crop(educ_gcc, grid_sf_country)
  
  
  # Save to a new shapefile
  st_write(educ_fac, 
           file.path(raw_replication,"OSM","raw",
                     country,
                     paste0("educ_facilities_", country, ".shp")), 
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
  
  # Crop educ_gcc based on the filtered grid
  educ_isr_pal$indicator <- st_within(educ_isr_pal,grid_sf_country)%>% lengths > 0
  educ_fac <- educ_isr_pal %>% filter(indicator == T)
  
  
  # Save to a new shapefile
  st_write(educ_fac, 
           file.path(raw_replication,"OSM","raw",
                     country,
                     paste0("educ_facilities_", country, ".shp")), 
           delete_layer = TRUE)
  
}



# Import the created educ facilties -------------------------------------

# Function to read a shapefile, add a country_name column, and return the modified sf object
read_shapefile_and_add_country <- function(country, base_path) {
  file_path <- file.path(base_path,"OSM","raw", country, paste0("educ_facilities_", country, ".shp"))
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

# Read each shapefile, add the country name, and store the results in a list
shapefiles_list <- lapply(countries_all, read_shapefile_and_add_country, base_path = raw_replication)

#clean lists to have equal number of columns
shapefiles_list[[19]] <- shapefiles_list[[19]] %>% dplyr::select(-indicator)
shapefiles_list[[20]] <- shapefiles_list[[20]] %>% dplyr::select(-indicator)

combined_shapefile <- do.call(rbind, shapefiles_list)


# Export ------------------------------------------------------------------

# Save the combined shapefile as an Rds file
saveRDS(combined_shapefile, file.path(final_replication,
                                      "mena_educ_facilities.Rds"))



})
