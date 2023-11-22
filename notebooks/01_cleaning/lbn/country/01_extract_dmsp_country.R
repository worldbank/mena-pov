# Extract DMSP NTL



# Load Data ---------------------------------------------------------------
shp <- st_read(file.path(lbn_file_path,
                         "Boundaries",
                         "gadm41_LBN_0.shp"))


#DMSP pre 2012
setwd("M:/LBN/GEO/Nighttime_Lights/raw/monthly/DMSP")
rastlist <- list.files(path = "M:/LBN/GEO/Nighttime_Lights/raw/monthly/DMSP", pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)

#import all raster files in folder using lapply
allrasters <- lapply(rastlist, raster)



# Extract values ----------------------------------------------------------
# extract to shapefile
months <- 1:12
dmsp_ntl_mean <- list()
dmsp_ntl_median <- list()

for (i in 1:length(months)) {
  dmsp_ntl_mean[[paste0("dmsp_ntl_mean_2011_", months[i])]] <- exact_extract(allrasters[[i]], shp, 'mean')
}


for (i in 1:length(months)) {
  dmsp_ntl_median[[paste0("dmsp_ntl_median_2011_", months[i])]] <- exact_extract(allrasters[[i]], shp, 'median')
}

#Convert to a data frame
dmsp_ntl_mean_2011 <- as.data.frame(do.call(cbind, dmsp_ntl_mean))
dmsp_ntl_median_2011 <- as.data.frame(do.call(cbind, dmsp_ntl_median))


# Add the uid column
dmsp_ntl_mean_2011$GID_0 <- shp$GID_0
dmsp_ntl_median_2011$GID_0 <- shp$GID_0

# Melt the dataframe
dmsp_ntl_mean_2011_melted <- dmsp_ntl_mean_2011 %>%
  pivot_longer(cols = starts_with("dmsp_ntl_mean_"), 
               names_to = "month", 
               values_to = "ntl_mean") %>%
  mutate(month = as.numeric(str_remove(month, "dmsp_ntl_mean_2011_")),
         year = 2011) 

dmsp_ntl_median_2011_melted <- dmsp_ntl_median_2011 %>%
  pivot_longer(cols = starts_with("dmsp_ntl_median_"), 
               names_to = "month", 
               values_to = "ntl_median") %>%
  mutate(month = as.numeric(str_remove(month, "dmsp_ntl_median_2011_")),
         year = 2011)

merged_df <- merge(dmsp_ntl_mean_2011_melted,dmsp_ntl_median_2011_melted, by = c("GID_0", "year", "month"))


# Export ------------------------------------------------------------------

saveRDS(merged_df, file.path(lbn_file_path,
                             "Nighttime_Lights",
                             "final",
                             "ntl_dmsp_2011.Rds"))

