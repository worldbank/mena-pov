# ================================================================
# Script Name: 02_EXTRACT_CITY_CENTERS.R
# Purpose: Creates shapefiles with points that are locations of city centers for all of MENA
# Input Dataset: World_Cities.geojson,MENA_ADM1.shp
# Output Dataset: "cities_", country, ".shp"
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
#Time taken: 36 seconds
# ================================================================

system.time ({
  

# Load Data ---------------------------------------------------------------
cities <- st_read(file.path(raw_replication,"OSM", "raw","World_Cities.geojson")) %>% st_as_sf()
mena_shp <- st_read(file.path(raw_replication,"BOUNDARIES", "MENA_ADM1.shp")) 






# Filter cities within MENA -----------------------------------------------
#create list of countries to filter
countries <- c("Tunisia","Libya", "Malta","Egypt","Syria","Lebanon","Iraq","Saudi Arabia",
               "Jordan","Israel","Gaza Strip","West Bank", "Yemen","Morocco", "Algeria",
               "Iran", "Kuwait","Bahrain","United Arab Emirates", "Qatar", "Oman","Djibouti")

cities_within_mena <- cities %>%
  dplyr::filter(CNTRY_NAME %in% countries)






# Change projection -------------------------------------------------------
cities_within_mena <- st_transform(cities_within_mena,4326)






# Add country name --------------------------------------------------------

cities_within_mena <- cities_within_mena %>%
  dplyr::mutate(WB_ADM0_NA = case_when(
    CNTRY_NAME == "Tunisia" ~ "Tunisia",
    CNTRY_NAME == "Libya" ~ "Libya",
    CNTRY_NAME == "Egypt" ~ "Arab Republic of Egypt",
    CNTRY_NAME == "Syria" ~ "Syrian Arab Republic",
    CNTRY_NAME == "Lebanon" ~ "Lebanon",
    CNTRY_NAME == "Iraq" ~ "Iraq",
    CNTRY_NAME == "Saudi Arabia" ~ "Saudi Arabia",
    CNTRY_NAME == "Jordan" ~ "Jordan",
    CNTRY_NAME == "Israel" ~ "Israel",
    CNTRY_NAME == c("Gaza Strip", "West Bank") ~ "West Bank and Gaza",
    CNTRY_NAME == "Yemen" ~ "Republic of Yemen",
    CNTRY_NAME == "Morocco" ~ "Morocco",
    CNTRY_NAME == "Algeria" ~ "Algeria",
    CNTRY_NAME == "Iran" ~ "Islamic Republic of Iran",
    CNTRY_NAME == "Kuwait" ~ "Kuwait",
    CNTRY_NAME == "Bahrain" ~ "Bahrain",
    CNTRY_NAME == "United Arab Emirates" ~ "United Arab Emirates",
    CNTRY_NAME == "Qatar" ~ "Qatar",
    CNTRY_NAME == "Oman" ~ "Oman",
    CNTRY_NAME == "Djibouti" ~ "Djibouti",
    TRUE ~ as.character(NA) # Default case for any country not matched
  ))


# Export ------------------------------------------------------------------
countries <- c("Saudi Arabia", "Oman","United Arab Emirates", "Qatar",  
               "Bahrain", "Kuwait","Israel", "West Bank and Gaza",
               "Algeria","Djibouti", "GCC", "Arab Republic of Egypt", "Morocco",
               "Islamic Republic of Iran", "Iraq","Israel and Palestine",
               "Jordan", "Lebanon", "Libya","Syrian Arab Republic",
               "Tunisia", "Republic of Yemen")


#Export by country
# Save to a new shapefile
for (country in countries) {
  st_write(cities_within_mena, 
           file.path(raw_replication,"OSM", "raw",
                     country,
                     paste0("cities_", country, ".shp")), 
           delete_layer = TRUE)
  
}

})
