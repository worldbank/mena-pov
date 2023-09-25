# Create datasets that include:
# NTL at municipality level


# Load data ---------------------------------------------------------------
r <- stack(file.path(lbn_file_path,
                                 "Nighttime_Lights",
                                 "raw", 
                                 "monthly", 
                                 "lebanon_viirs_raw_monthly_start_201204_end_202303_avg_rad.tif"))

district_sf <- st_read(file.path(lbn_file_path,
                                 "Boundaries",
                                 "gadm41_LBN_2.shp"))



# Check Projection --------------------------------------------------------

# Check CRS of both
raster_crs <- crs(r)
sf_crs <- st_crs(district_sf)


# Extract -----------------------------------------------------------------

## create uid
district_sf$uid <- 1:nrow(district_sf)

ntl_mean    <- exact_extract(r, district_sf, fun = "mean")
ntl_median  <- exact_extract(r, district_sf, fun = "median")


process_data <- function(my_data, data_name, start_year = 2012, start_month = 4) {
  
  # Melt the data
  data_melted <- my_data %>%
    mutate(uid = 1:nrow(.)) %>%
    melt(id.vars = "uid") %>%
    mutate(variable = gsub("mean.avg_rad", "", variable))
  
  # Initialize the year and month values
  year <- start_year
  month <- start_month
  
  # Assign year and month based on unique variable
  for(i in unique(data_melted$variable)) {
    data_melted$year[data_melted$variable %in% i] <- year 
    data_melted$month[data_melted$variable %in% i] <- month 
    
    month <- month + 1
    
    if(month == 13) {
      month <- 1
      year <- year + 1
    }
  }
  
  # Rename the 'value' column using the data_name and remove the 'variable' column
  data_melted <- data_melted %>%
    select(-variable) %>%
    dplyr::rename_with(~ data_name, .cols = "value")
  
  return(data_melted)
}

# Call the function on each dataset
ntl_mean_melted <- process_data(ntl_mean, "ntl_mean")
ntl_median_melted <- process_data(ntl_median, "ntl_median")


# Merge with admin boundaries ---------------------------------------------
# List all the data frames you want to merge
list_of_dfs <- list(ntl_mean_melted, ntl_median_melted)

# Use reduce() to sequentially merge all data frames in the list
merged_df<- reduce(list_of_dfs, left_join, by = c("uid", "month", "year"))


# Extract population ------------------------------------------------------
setwd("M:/LBN/GEO/Population/raw")
rastlist <- list.files(path = "M:/LBN/GEO/Population/raw", pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)

#import all raster files in folder using lapply
allrasters <- lapply(rastlist, raster)
rastlist

# Extract values ----------------------------------------------------------

# Create an empty list to store the results
district_pop_list <- list()

for(i in 1:9) { 
  year <- 2011 + i
  district_pop_list[[year]] <- extract(allrasters[[i]], as(district_sf, "Spatial"), 
                                       fun = sum, na.rm = TRUE)
}


# Convert list to a data frame
district_pop_2012_2020 <- as.data.frame(do.call(cbind, district_pop_list))

# Name the columns
names(district_pop_2012_2020) <- paste0("district_pop_", 2012:2020)

# Add the uid column
district_pop_2012_2020$uid <- 1:nrow(district_pop_2012_2020)

# Melt the dataframe
district_pop_2012_2020_melted <- district_pop_2012_2020 %>%
  pivot_longer(cols = starts_with("district_pop_"), 
               names_to = "year", 
               values_to = "population") %>%
  mutate(year = as.numeric(str_remove(year, "district_pop_")))





## Check against WDI numbers
district_pop_2012_2020_melted %>%
  group_by(year) %>%
  summarise(total_pop = sum(population))


# Merge NTL and population ------------------------------------------------
merged_df <- merged_df %>%
  left_join(district_pop_2012_2020_melted, by = c("uid", "year"))


# Export ------------------------------------------------------------------
saveRDS(merged_df, file.path(lbn_file_path,
                             "Nighttime_Lights",
                             "final",
                             "lbn_distict_ntl_pop.Rds"))

saveRDS(merged_df, file.path(lbn_onedrive_dir,
                             "data",
                             "municipalities",
                             "lbn_district_ntl_pop.Rds"))

write_csv(merged_df, file.path(lbn_onedrive_dir,
                             "data",
                             "municipalities",
                             "lbn_district_ntl_pop.csv"))

