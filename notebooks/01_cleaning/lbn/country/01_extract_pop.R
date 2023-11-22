# Extract Population


# Load data ---------------------------------------------------------------

shp <- st_read(file.path(lbn_file_path,
                         "Boundaries",
                         "gadm41_LBN_0.shp"))



# Extract population ------------------------------------------------------
setwd("M:/LBN/GEO/Population/raw")
rastlist <- list.files(path = "M:/LBN/GEO/Population/raw", pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)

#import all raster files in folder using lapply
allrasters <- lapply(rastlist, raster)
rastlist



# extract to shapefile
years <- 2000:2020
cas_pop <- list()

for (i in 1:length(years)) {
  cas_pop[[paste0("cas_pop_", years[i])]] <- exact_extract(allrasters[[i]], shp, 'sum')
}


# Convert the list to a data frame
cas_pop_2000_2020 <- as.data.frame(do.call(cbind, cas_pop))

# Add the uid column
cas_pop_2000_2020$GID_0 <- shp$GID_0


# Melt the dataframe
cas_pop_2000_2020_melted <- cas_pop_2000_2020 %>%
  pivot_longer(cols = starts_with("cas_pop_"), 
               names_to = "year", 
               values_to = "population") %>%
  mutate(year = as.numeric(str_remove(year, "cas_pop_")))


# Export ------------------------------------------------------------------

saveRDS(cas_pop_2000_2020_melted, file.path(lbn_file_path,
                                            "Population",
                                            "final",
                                            "pop_2000_2023.Rds"))
