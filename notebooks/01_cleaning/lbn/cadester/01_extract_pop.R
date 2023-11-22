# Extract Population by Cadester




# Load Data ---------------------------------------------------------------
shp <- st_read(file.path(lbn_file_path,
                         "Boundaries",
                         "cadaster.shp"))

grid_blank <- readRDS("M:/LBN/GEO/Nighttime_Lights/final/grid_blank.Rds")



setwd("M:/LBN/GEO/Population/raw")
rastlist <- list.files(path = "M:/LBN/GEO/Population/raw", pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)

#import all raster files in folder using lapply
allrasters <- lapply(rastlist, raster)




# Extract to shapefile ----------------------------------------------------
years <- 2000:2020
cas_pop <- list()

for (i in 1:length(years)) {
  cas_pop[[paste0("cas_pop_", years[i])]] <- exact_extract(allrasters[[i]], shp, 'sum')
}


# Convert the list to a data frame
cas_pop_2000_2020 <- as.data.frame(do.call(cbind, cas_pop))

# Add the uid column
cas_pop_2000_2020$ACS_CODE_1 <- shp$ACS_CODE_1


# Melt the dataframe
cas_pop_2000_2020_melted <- cas_pop_2000_2020 %>%
  pivot_longer(cols = starts_with("cas_pop_"), 
               names_to = "year", 
               values_to = "population") %>%
  mutate(year = as.numeric(str_remove(year, "cas_pop_"))) %>%
  filter(ACS_CODE_1 != 0)


# Merge with blank grid
grid_blank <- grid_blank %>%
  left_join(.,cas_pop_2000_2020_melted, by = c("ACS_CODE_1", "year"))



# Export ------------------------------------------------------------------

saveRDS(grid_blank,"M:/LBN/GEO/Population/final/grid_pop.Rds")
