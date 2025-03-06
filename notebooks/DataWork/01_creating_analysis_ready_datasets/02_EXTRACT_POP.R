# ================================================================
# Script Name: 00_EXTRACT_POPULATION.R
# Purpose: Create a 10*10km grid with population estimates from 2020
# Input Dataset: ppp_2020_1km_Aggregated.tif,grid_10km.shp,MENA_ADM2.shp
# Output Dataset: grid_pop_10km_v2.Rds
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-06
# Time Taken: 28 mins
# ================================================================

system.time({
  
options(scipen=999)

# 1. Load Data ---------------------------------------------------------------
#Import raster
pop2020 <- raster(file.path(raw_replication,"POPULATION","ppp_2020_1km_Aggregated.tif"))
# Read the shapefile grid
grid_sf <- st_read(file.path(final_replication, "grid_10km.shp"))
# Read MENA boundary file
shp <- st_read(file.path(raw_replication,"BOUNDARIES","MENA_ADM2.shp"))






# 2. Extract values from Population to grid --------------------------

##make sure the projection is the same
pop2020_mna <- crop(pop2020,shp)
pop2020_mna <- projectRaster(pop2020_mna, crs = st_crs(grid_sf)$proj4string)

# Find negative values
negative_values <- pop2020_mna[pop2020_mna < 0]
number_of_negative_cells <- length(negative_values) 
print(number_of_negative_cells)

pop2020_mna[pop2020_mna < 0] <- 0


# extract the values to the grid
grid_sf$pop_count <- exact_extract(pop2020_mna,grid_sf,'sum')



# Check the total population of each country level for 2020
countries_pop <- grid_sf %>%
  group_by(WB_ADM0_NA) %>%
  summarise(pop = sum(pop_count))
# It looks like it is the same magnitude but underestimate when density is high.





# Create Population Density variable --------------------------------------
grid_sf$pop_density <- (grid_sf$pop_count)/100




# 3. Export ------------------------------------------------------------------
saveRDS(grid_sf, file.path(final_replication,
                           "grid_pop_10km.Rds"))


})
