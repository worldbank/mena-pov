# ================================================================
# Script Name: 03a_MERGE_CITIES_PORTS_AIRPORTS.R
# Purpose: Merge the locations of markets, health and educational institutions
# Input Dataset: "ports_", country_name, ".shp","cities_", country_name, ".shp",
#"airports_", country_name, ".shp"
# Output Dataset: mena_markets.Rds
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-09
# ================================================================



# Merge cities, ports and sea ports to create a list of "markets"


# Load Data ---------------------------------------------------------------
# List of countries
countries <- c("Saudi Arabia", "Oman", "United Arab Emirates", "Qatar",  
               "Bahrain", "Kuwait", "Israel", "West Bank and Gaza",
               "Algeria", "Djibouti", "Arab Republic of Egypt", "Morocco",
               "Islamic Republic of Iran", "Iraq", 
               "Jordan", "Lebanon", "Libya", "Syrian Arab Republic",
               "Tunisia", "Republic of Yemen")


# Function to load files --------------------------------------------------

########### PORTS #########################
# Create file paths for each country's port shapefile
file_paths_ports <- sapply(countries, function(country_name) {
  file.path(raw_replication,"OSM", "raw", country_name, paste0("ports_", country_name, ".shp"))
})


ports_list <- lapply(file_paths_ports, function(path) {
  if(file.exists(path)) {
    st_read(path, quiet = TRUE)  # 'quiet = TRUE' to suppress reading messages
  } else {
    NULL  # Handle missing files
  }
})

#Add a country column
ports_list <- Map(function(df, country_name) {
  if (!is.null(df)) {
    df %>% dplyr::mutate(country = country_name)
  } else {
    NULL
  }
}, ports_list, countries)


########### CITIES #########################
# Load cities from each country
file_paths_cities <- sapply(countries, function(country_name) {
  file.path(raw_replication,"OSM","raw", country_name, paste0("cities_", country_name, ".shp"))
})

cities_list <- lapply(file_paths_cities, function(path) {
  if(file.exists(path)) {
    st_read(path, quiet = TRUE)  # 'quiet = TRUE' to suppress reading messages
  } else {
    NULL  # Handle missing files
  }
})

#Rename columns for the list and subset
cities_list <- lapply(cities_list, function(df) {
  if (!is.null(df)) {
    df %>%
      dplyr::select(FID, CITY_NAME,CNTRY_NAME,geometry) %>%  # Select the variables you want to keep
      dplyr::rename(osm_id = FID, name = CITY_NAME, country = CNTRY_NAME)%>% # Rename selected variables
      dplyr::mutate(fclass = "city")
  } else {
    NULL  # Return NULL if the input was NULL
  }
})


########### AIRPORTS #########################
file_paths_airports <- sapply(countries, function(country_name) {
  file.path(raw_replication,"OSM","raw", country_name, paste0("airports_", country_name, ".shp"))
})

airports_list <- lapply(file_paths_airports, function(path) {
  if(file.exists(path)) {
    st_read(path, quiet = TRUE)  # 'quiet = TRUE' to suppress reading messages
  } else {
    NULL  # Handle missing files
  }
})


#Add a country column
airports_list <- Map(function(df, country_name) {
  if (!is.null(df)) {
    df %>% dplyr::mutate(country = country_name)
  } else {
    NULL
  }
}, airports_list, countries)



# Combine the three - PORTS, CITIES AND AIRPORTS
combined_list <- c(ports_list,cities_list,airports_list)


# convert osm id into a character type to merge all of them
combined_list <- lapply(combined_list, function(df) {
  df$osm_id <- as.character(df$osm_id)
  return(df)
})

# Now, attempt to bind rows again
markets <- bind_rows(combined_list)


# Bind all data frames into one)
markets <- bind_rows(combined_list)

#Remove the indicator variable
markets <- markets %>% dplyr::select(-indicator)



# Export ------------------------------------------------------------------
saveRDS(markets, file.path(final_replication,
                           "mena_markets.Rds"))


