options(scipen=999)
# Load Data ---------------------------------------------------------------
#Import raster
pop2020 <- raster(file.path(global_file_path,"Population","ppp_2020_1km_Aggregated.tif"))


# Read the shapefile grid
grid_sf <- st_read(file.path(mena_file_path, "Boundaries", "final", "grid_10km.shp"))


# Read MENA boundary file
shp <- st_read(file.path(mena_file_path,"Boundaries","raw","MENA_ADM2.shp"))


# Extract values from Population to grid --------------------------

##make sure the projection is the same
pop2020_mna <- crop(pop2020,shp)
pop2020_mna <- projectRaster(pop2020_mna, crs = st_crs(grid_sf)$proj4string)



# extract the values to the grid
grid_sf$pop_count <- exact_extract(pop2020_mna,grid_sf,'sum')


# to check the total population of each country level for 2020
countries_pop <- grid_sf %>%
  group_by(WB_ADM0_NA) %>%
  summarise(pop = sum(pop_count))


# Create Population Density variable --------------------------------------
grid_sf$pop_density <- (grid_sf$pop_count)/100



# Export ------------------------------------------------------------------
saveRDS(grid_sf, file.path(mena_file_path,
                           "Population",
                           "final",
                           "grid_pop_10km.Rds"))

st_write(grid_sf, file.path(mena_file_path,
                            "Population",
                            "final",
                            "grid_pop_10km.shp"), append = FALSE)






# Plot --------------------------------------------------------------------
pop <- grid_sf %>% select(pop_density)
plot(pop)
