# ================================================================
# Script Name: 03e_MERGE_TT_GRID.R
# Purpose: Merge all the three travel time grids, health, educ, and markets
# Input Dataset: grid_10km.shp, 
# Output Dataset: tt_health_educ_markets.Rds
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================



# Load Data ---------------------------------------------------------------

#Load Grid
grid_sf <- st_read(file.path(final_replication,"grid_10km.shp"))




read_and_merge_csv_files <- function(directory_path) {
  # Get a list of all CSV files in the directory
  csv_files <- list.files(path = directory_path, pattern = "final_output_.*\\.csv$", full.names = TRUE)
  
  # Read each CSV file into a list of data frames and add the country name as a new column
  list_of_data_frames <- lapply(csv_files, function(file) {
    data_frame <- read.csv(file)
    
    # Extract the country name from the file name (remove "final_output_" and ".csv")
    country_name <- gsub("final_output_", "", basename(file))
    country_name <- gsub(".csv", "", country_name)
    
    data_frame$Country <- country_name  # Add the country name as a new column
    return(data_frame)
  })
  
  # Combine all data frames into a single data frame
  combined_data_frame <- do.call(rbind, list_of_data_frames)
  
  # Return the combined data frame
  return(combined_data_frame)
}



# Process CSV files for Health
tt_health <- read_and_merge_csv_files("C:/Users/wb569257/OneDrive - WBG/MENAPOV Geospatial Documents - MENAPOV Geospatial Files/MENAPOV GEO/Projects/vulnerability/REPLICATION_RAW_DATA/OSM/intermediate/health")

# Process CSV files for Education
tt_educ <- read_and_merge_csv_files("C:/Users/wb569257/OneDrive - WBG/MENAPOV Geospatial Documents - MENAPOV Geospatial Files/MENAPOV GEO/Projects/vulnerability/REPLICATION_RAW_DATA/OSM/intermediate/educ")

# Process CSV files for Markets
tt_markets <- read_and_merge_csv_files("C:/Users/wb569257/OneDrive - WBG/MENAPOV Geospatial Documents - MENAPOV Geospatial Files/MENAPOV GEO/Projects/vulnerability/REPLICATION_RAW_DATA/OSM/intermediate/markets")






# Check for duplicates ----------------------------------------------------
#As a check compare the grid ids in all three grids

health_grid_count <- tt_health %>%
  group_by(Country) %>%
  summarise(distinct_grid_count = n_distinct(grid_id))

educ_grid_count <- tt_educ %>%
  group_by(Country) %>%
  summarise(distinct_grid_count = n_distinct(grid_id))

markets_grid_count <- tt_markets %>%
  group_by(Country) %>%
  summarise(distinct_grid_count = n_distinct(grid_id))


grid_sf_count <- grid_sf %>%
  group_by(WB_ADM0_NA) %>%
  summarise(distinct_grid_count = n_distinct(grid_id))

combined_counts <- full_join(health_grid_count, educ_grid_count, by = "Country", suffix = c("_health", "_educ")) %>%
  full_join(markets_grid_count %>% 
              rename(district_count_markets = distinct_grid_count), 
            by = "Country")




# 
# # Assuming tt_health is your dataframe and you're checking for duplicates in 'grid_id'
# tt_health <- tt_health %>%
#   # Arrange by grid_id to ensure the first occurrence is considered the 'Original'
#   arrange(grid_id) %>%
#   # Group by grid_id
#   group_by(grid_id) %>%
#   # Add a Tag column: mark the first as 'Original' and the rest as 'Duplicate'
#   mutate(Tag = if_else(row_number() == 1, "Original", "Duplicate")) %>%
#   # Ungroup to prevent grouping affecting further operations
#   ungroup()
# 
# 
# #Filter to include only grid_ids that are duplicated (having both 'Original' and 'Duplicate' tags)
# tt_health_duplicated <- tt_health %>%
#   group_by(grid_id) %>%
#   # Create a temporary column to count occurrences of each grid_id
#   mutate(count = n()) %>%
#   # Filter to keep only groups where the count is greater than 1 (i.e., duplicated grid_ids)
#   filter(count > 1) %>%
#   # Remove the temporary count column, as it's no longer needed
#   dplyr::select(-count) %>%
#   # Ungroup the data after filtering
#   ungroup()



# Subset data -------------------------------------------------------------
tt_educ <- tt_educ %>% dplyr::rename(min_tt_educ = min_tt,
                                     educ_facility_id = facility_osm_id) %>%
  dplyr::select(-Country)


tt_health <- tt_health %>% dplyr::rename(min_tt_health = min_tt,
                                         health_facility_id = facility_osm_id) %>%
  dplyr::select(-Country)


tt_markets <- tt_markets %>% dplyr::rename(min_tt_markets = min_tt,
                                           market_facility_id = facility_osm_id) %>%
  dplyr::select(-Country)






# Merge by grid id --------------------------------------------------------
merged_tt <- merge(grid_sf,tt_health, by = c("grid_id")) %>%
  left_join(tt_educ, by = c("grid_id")) %>%
  left_join(tt_markets, c("grid_id"))






# Export ------------------------------------------------------------------
saveRDS(merged_tt, file.path(final_replication, "grid_tt_health_educ_markets.Rds"))




