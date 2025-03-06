# ================================================================
# Script Name: 02_EXTRACT_PORTS.R
# Purpose: Create a shapefile with port locations from OSM for all of MENA
# Input Dataset: gis_osm_landuse_a_free_1.shp,grid_10km.Rds
# Output Dataset: "ports_", country, ".shp"
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-06
# Time Taken: 2 mins
# ================================================================

system.time({
  



# Ports -------------------------------------------------------------------
# Define the function
process_ports <- function(country_name, mena_file_path) {
  
  # Read the landuse shapefile
  landuse <- st_read(file.path(raw_replication,"OSM","raw", country_name,
                               "gis_osm_landuse_a_free_1.shp")) %>% 
    filter(fclass == "industrial")
  
  # Filter the transport data for entries containing the word "Port"
  ports <- landuse[grep("נמל|بندر|Port|ميناء|مرفأ", landuse$name, ignore.case = TRUE), ]
  
  
  # Save the combined dataset to a new shapefile
  st_write(ports,
           file.path(raw_replication,"OSM", "raw",
                     country_name,
                     paste0("ports_", country_name, ".shp")),
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
  process_ports(country, mena_file_path)
}



### SEPARATE GCC COUNTRIES and ISRAEL/PALESTINE
ports_gcc <- st_read(file.path(raw_replication,"OSM","raw", "GCC", "ports_GCC.shp"))
ports_isr_pal <- st_read(file.path(raw_replication,"OSM", "raw", "Israel and Palestine", "ports_Israel and Palestine.shp"))

leaflet() %>% addTiles() %>% addPolygons(data = ports_gcc)


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
  
  # Crop ports_fac based on the filtered grid
  ports_fac <- st_crop(ports_gcc, grid_sf_country)
  
  
  # Save to a new shapefile
  st_write(ports_fac,
           file.path(raw_replication,"OSM", "raw",
                     country,
                     paste0("ports_", country, ".shp")),
           delete_layer = TRUE)
  
}

leaflet() %>% addTiles() %>%addPolygons(data = ports_fac)

countries_isr_pal <- c("Israel", "West Bank and Gaza")


# Iterate over each country
for (country in countries_isr_pal) {
  # Filter grid_sf for the current country
  grid_sf_country <- grid_sf %>% 
    filter(WB_ADM0_NA == country)
  
  # Transform grid_sf to EPSG 4326
  grid_sf_country <- st_transform(grid_sf_country, 4326)
  
  #Create a small half-kilometer buffer
  grid_sf_country_buffered <- st_buffer(grid_sf_country, dist = 500) # Buffer distance in meters
  
  # Crop health_gcc based on the filtered grid
  ports_isr_pal$indicator <- st_intersects(ports_isr_pal,grid_sf_country_buffered)%>% lengths > 0
  ports_fac <- ports_isr_pal %>% filter(indicator == T)
  
  
  # Save to a new shapefile
  st_write(ports_fac, 
           file.path(raw_replication,"OSM", "raw",
                     country,
                     paste0("ports_", country, ".shp")), 
           delete_layer = TRUE)
  
}

})