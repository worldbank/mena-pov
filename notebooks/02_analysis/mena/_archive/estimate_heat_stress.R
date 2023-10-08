
# Load data ---------------------------------------------------------------
setwd("M:/GEOGlobal/HeatStress")

#first import all files in a single folder as a list 
rastlist_wbgt <- list.files(path = "M:/GEOGlobal/HeatStress", pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)

#import all raster files in folder using lapply
allrasters_wbgt <- lapply(rastlist_wbgt, raster)


##import population
pop_2020 <- raster("M:/GEOGlobal/Population/ppp_2020_1km_Aggregated.tif")


## import shapefile
shp <- st_read(file.path(mena_file_path, "Boundaries", "MENA_ADM2.shp"))


## second load the grid shape at 10km x10km resolution
# Read the shapefile grid
grid_sf <- st_read(file.path(mena_file_path, "Boundaries", "final", "grid_10km.shp"))





# Crop and mask rasters ---------------------------------------------------

##population raster
cropped <- crop(x = pop_2020, y = extent(shp))



## crop multiple rasters
cropped_100y <- crop(x = allrasters_wbgt[[1]], y = extent(shp))
cropped_20y <- crop(x = allrasters_wbgt[[2]], y = extent(shp))
cropped_5y <- crop(x = allrasters_wbgt[[3]], y = extent(shp))




# Create a dataset --------------------------------------------------------
## each row represents a raster cell, and each column is the raster value from
## each return period

# Extract cell values and coordinates
values_100y <- as.data.frame(cropped_100y, xy = TRUE)
values_20y <- as.data.frame(cropped_20y, xy = TRUE)
values_5y <- as.data.frame(cropped_5y, xy = TRUE)


# Name the columns to avoid confusion
colnames(values_100y) <- c("x", "y", "Temp_RP_100")
colnames(values_20y) <- c("x", "y", "Temp_RP_20")
colnames(values_5y) <- c("x", "y", "Temp_RP_5")




# Combine the data frames by x and y coordinates to get values from all rasters
combined_data <- values_100y %>%
  inner_join(values_20y, by = c("x", "y")) %>%
  inner_join(values_5y, by = c("x", "y"))




# Compute Expected Mean (create upper bound and lower bound)

combined_data$upper_bound <- combined_data$Temp_RP_100*((1/20)-(1/100))+
  combined_data$Temp_RP_20*((1/5)-(1/20)) + 
  combined_data$Temp_RP_5*((1)-(1/5) + (1/100))

combined_data$lower_bound <- combined_data$Temp_RP_100*(1/100)+
  combined_data$Temp_RP_20*((1/20)-(1/100)) +
  combined_data$Temp_RP_5*((1/5)-(1/20))

combined_data$expected_mean <- (combined_data$upper_bound + 
                                  combined_data$lower_bound)/2





# Convert dataframe to spatial dataframe ----------------------------------


combined_data_sf <- st_as_sf(x = combined_data, coords = c("x","y"), crs = st_crs(shp))





# Extract population to this dataframe ------------------------------------

## Intersect the raster layer
combined_data_sf$population <- extract(pop_2020, st_coordinates(combined_data_sf), fun = sum, 
                                       na.rm = TRUE)





# Create heat thresholds --------------------------------------------------

## As per the scientific literature, we classify the temperatures into
## three categories slight/low (<28°C), moderate/high (28-32°C) 
## and severe/very high (>32°C)

combined_data_sf <- combined_data_sf %>%
  mutate(heat_stress = case_when(
    expected_mean < 28 ~ "slight/low",
    expected_mean > 28 & expected_mean < 32 ~ "moderate/high",
    expected_mean > 32 ~ "severe/very high",
    TRUE ~ NA
  ))

## Creating a variable for the population affected by severe heat
combined_data_sf$severe_affected_pop <- ifelse(combined_data_sf$heat_stress == "severe/very high",
                                            combined_data_sf$population,0)





hist(combined_data_sf$expected_mean)




# Plot --------------------------------------------------------------------




