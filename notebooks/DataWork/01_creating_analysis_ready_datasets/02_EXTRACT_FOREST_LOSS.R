# ================================================================
# Script Name: 00_EXTRACT_FOREST_LOSS.R
# Purpose: Create a 10*10km grid with the forest cover loss layer
# Input Dataset: grid_10km.shp,Hansen" , pattern = "\\.tif
# Output Dataset: grid_forest_loss_10km.Rds
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-06
# Time Taken: 23 mins
# ================================================================

system.time({
  


# Load Data ---------------------------------------------------------------
#Grid File
grid_sf <- st_read(file.path(final_replication,"grid_10km.shp"))
grid_sf <- st_transform(grid_sf, 4326)

#Import rasters
raster_files <- list.files(path = file.path(raw_replication,"FOREST_LOSS") , pattern = "\\.tif$", full.names = TRUE)
raster_list <- lapply(raster_files, raster)

raster_files
raster_list





# Subset grid by country --------------------------------------------------
country_names <- unique(grid_sf$WB_ADM0_NA)

# Initialize an empty list to store subsets
grid_list <- list()

# Loop through each country and create a subset
for (country in country_names) {
  grid_list[[country]] <- grid_sf %>% filter(WB_ADM0_NA == country)
}





# Extract forest loss values to each grid ---------------------------------

# Define a list with countries and their corresponding raster indexes 

country_rasters = list(
  "Algeria" = c(1,2,3,4), 
  "Bahrain" = 5,
  "Djibouti" = 6,
  "Arab Republic of Egypt" = 7,
  "West Bank and Gaza" = 25,
  "Islamic Republic of Iran" = c(8,9),
  "Kuwait" = 13,
  "Iraq" = 10,
  "Jordan" = 12,
  "Israel" = 11,
  "Lebanon" = 14,
  "Libya" = 15,
  "Morocco" = 17,
  "Malta" = 16,
  "Oman" = 18,
  "Qatar" = 19,
  "Saudi Arabia" = c(20,21),
  "Syrian Arab Republic" = 22,
  "Tunisia" = 23,
  "Republic of Yemen" = 26,
  "United Arab Emirates" = 24
)










# Parellel Process --------------------------------------------------------
# Determine the number of cores
no_cores <- detectCores() - 1  # Leave one core free for system use

# Initiate parallel backend
cl <- makeCluster(no_cores)


# Export the necessary objects and functions to each worker
clusterExport(cl, varlist = c("raster_list", "grid_list", "exact_extract", "country_rasters"))

# Apply exact_extract in parallel
results <- parLapply(cl, names(country_rasters), function(country) {
  raster_indices <- country_rasters[[country]]
  
  # Check if the country has multiple rasters
  if (length(raster_indices) > 1) {
    for (i in 1:length(raster_indices)) {
      grid_list[[country]][[paste0("forest_loss", i)]] <- exact_extract(raster_list[[raster_indices[i]]], grid_list[[country]], fun = "mean")
    }
  } else {
    grid_list[[country]]$forest_loss <- exact_extract(raster_list[[raster_indices]], grid_list[[country]], fun = "mean")
  }
  return(grid_list[[country]])
})

stopCluster(cl)



# Combine the results back into grid_list
grid_list <- setNames(results, names(country_rasters))

#Combine data in different forest loss columns into one
grid_list[["Islamic Republic of Iran"]] <- grid_list[["Islamic Republic of Iran"]] %>%
  dplyr::mutate(forest_loss = ifelse(is.na(forest_loss1),forest_loss2,forest_loss1)) %>%
  dplyr::select(-c(forest_loss1, forest_loss2))

grid_list[["Saudi Arabia"]] <- grid_list[["Saudi Arabia"]] %>%
  dplyr::mutate(forest_loss = ifelse(is.na(forest_loss1),forest_loss2,forest_loss1)) %>%
  dplyr::select(-c(forest_loss1, forest_loss2))

grid_list[["Algeria"]] <- grid_list[["Algeria"]] %>%
  dplyr::mutate(forest_loss = coalesce(forest_loss1, forest_loss2, forest_loss3, forest_loss4)) %>%
  dplyr::select(-c(forest_loss1, forest_loss2, forest_loss3, forest_loss4))



combined_df <- do.call(rbind,grid_list)
combined_df <- combined_df %>% dplyr::select(grid_id,forest_loss) %>% st_drop_geometry()






# Join it back to the original grid ---------------------------------------
grid_sf <- grid_sf %>% left_join(combined_df, by = c("grid_id"))



# Create dummy variable here
grid_sf <- grid_sf %>%
  mutate(loss_category = ifelse(forest_loss > 0, "Forest Cover Loss", "No Forest Cover Loss"))






# Export ------------------------------------------------------------------

saveRDS(grid_sf, file.path(final_replication,"grid_forest_loss_10km.Rds"))

})


