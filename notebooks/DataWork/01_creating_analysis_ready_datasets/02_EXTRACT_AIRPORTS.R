# ================================================================
# Script Name: 02_EXTRACT_AIRPORTS.R
# Purpose: Creates shapefiles with points that are locations of airports for all of MENA
# Input Dataset: gis_osm_transport_a_free_1.shp
# Output Dataset: "airports_", country, ".shp"
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# Time Taken : 36 seconds
# ================================================================


system.time({
  


# Airports -------------------------------------------------------------------

# Define the function
process_airports <- function(country_name, mena_file_path) {
  
  # Read the landuse shapefile
  airport <- st_read(file.path(raw_replication,"OSM", "raw", country_name,
                               "gis_osm_transport_a_free_1.shp")) %>% 
    filter(fclass == "airport")
  
  
  # Save the combined dataset to a new shapefile
  st_write(airport, 
           file.path(raw_replication,"OSM", "raw",
                     country_name,
                     paste0("airports_", country_name, ".shp")), 
           delete_layer = TRUE)
  
}

# List of countries to process
countries <- c("Algeria","Djibouti", "GCC", "Arab Republic of Egypt", "Morocco",
               "Islamic Republic of Iran", "Iraq","Israel and Palestine",
               "Jordan", "Lebanon", "Libya","Syrian Arab Republic",
               "Tunisia", "Republic of Yemen")



# Execute the function for each country
# Loop over the list of countries and process each one
for(country in countries) {
  process_airports(country, mena_file_path)
}



### SEPARATE GCC COUNTRIES and ISRAEL/PALESTINE
airports_gcc <- st_read(file.path(raw_replication,"OSM", "raw", "GCC", "airports_GCC.shp"))
airports_isr_pal <- st_read(file.path(raw_replication,"OSM","raw", "Israel and Palestine", "airports_Israel and Palestine.shp"))




#grid
grid_sf <- readRDS(file.path(final_replication,
                             "grid_10km.Rds")) 


countries_gcc <- c("Saudi Arabia", "Oman","United Arab Emirates", "Qatar",  
                   "Bahrain", "Kuwait")


for (country in countries_gcc) {
  # Filter grid_sf for the current country
  grid_sf_country <- grid_sf %>% 
    filter(WB_ADM0_NA == country)
  
  # Transform grid_sf to EPSG 4326
  grid_sf_country <- st_transform(grid_sf_country, 4326)
  
  # Crop ports_fac based on the filtered grid
  airports_fac <- st_crop(airports_gcc, grid_sf_country)
  
  
  # Save to a new shapefile
  st_write(airports_fac, 
           file.path(raw_replication,"OSM", "raw",
                     country,
                     paste0("airports_", country, ".shp")), 
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
  airports_isr_pal$indicator <- st_within(airports_isr_pal,grid_sf_country)%>% lengths > 0
  airports_fac <- airports_isr_pal %>% filter(indicator == T)
  
  
  # Save to a new shapefile
  st_write(airports_fac, 
           file.path(raw_replication,"OSM","raw",
                     country,
                     paste0("airports_", country, ".shp")), 
           delete_layer = TRUE)
  
}

})
