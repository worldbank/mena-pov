# Create datasets that include:
# NTL at municipality level


# Load data ---------------------------------------------------------------
r <- stack(file.path(lbn_file_path,
                                 "Nighttime_Lights",
                                 "raw", 
                                 "monthly", 
                                 "lebanon_viirs_raw_monthly_start_201204_end_202303_avg_rad.tif"))

municipality_sp <- st_read(file.path(lbn_file_path,
                                 "Boundaries",
                                 "gadm41_LBN_3.shp")) %>% as_Spatial()

# Extract -----------------------------------------------------------------
r_t2 <- r
r_t2[] <- as.numeric(r_t2[] >= 2)

## create uid
municipality_sp@data$uid <- 1:nrow(municipality_sp@data)

ntl_mean    <- exact_extract(r, municipality_sp, fun = "mean")
ntl_median  <- exact_extract(r, municipality_sp, fun = "median")
ntl_prop_g2 <- exact_extract(r_t2, municipality_sp, fun = "mean")


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
ntl_prop_g2_melted <- process_data(ntl_prop_g2, "ntl_prop_g2")


# Merge with admin boundaries ---------------------------------------------
# List all the data frames you want to merge
list_of_dfs <- list(ntl_mean_melted, ntl_median_melted, ntl_prop_g2_melted)

# Use reduce() to sequentially merge all data frames in the list
merged_df <- reduce(list_of_dfs, left_join, by = c("uid", "month", "year"))



municipality_sf <- municipality_sp %>%
  st_as_sf() %>%
  mutate(uid = 1:n()) %>%
  left_join(merged_df, by = "uid")



# Extract population ------------------------------------------------------

# Load Data ---------------------------------------------------------------
setwd("M:/LBN/GEO/Population/raw")

library(exactextractr)
rastlist <- list.files(path = "M:/LBN/GEO/Population/raw", pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)

#import all raster files in folder using lapply
allrasters <- lapply(rastlist, raster)
rastlist

# Extract values ----------------------------------------------------------
municipality_pop_2012 <- exact_extract(allrasters[[1]], municipality_sp, fun = "sum")
municipality_pop_2013 <- exact_extract(allrasters[[2]], municipality_sp, fun = "sum")
municipality_pop_2014 <- exact_extract(allrasters[[3]], municipality_sp, fun = "sum")
municipality_pop_2015 <- exact_extract(allrasters[[4]], municipality_sp, fun = "sum")
municipality_pop_2016 <- exact_extract(allrasters[[5]], municipality_sp, fun = "sum")
municipality_pop_2017 <- exact_extract(allrasters[[6]], municipality_sp, fun = "sum")
municipality_pop_2018 <- exact_extract(allrasters[[7]], municipality_sp, fun = "sum")
municipality_pop_2019 <- exact_extract(allrasters[[8]], municipality_sp, fun = "sum")
municipality_pop_2020 <- exact_extract(allrasters[[9]], municipality_sp, fun = "sum")


municipality_pop_2012_2020 <- as.data.frame(cbind(municipality_pop_2012,
                                                  municipality_pop_2013,
                                                  municipality_pop_2014,
                                                  municipality_pop_2015,
                                                  municipality_pop_2016,
                                                  municipality_pop_2017,
                                                  municipality_pop_2018,
                                                  municipality_pop_2019,
                                                  municipality_pop_2020))

municipality_pop_2012_2020_melted <- municipality_pop_2012_2020 %>%
  mutate(uid = 1:n()) %>%
  melt(.,id = "uid") %>%
  mutate(year = as.numeric(gsub("municipality_pop_","",variable))) %>%
  select(-variable) %>%
  dplyr::rename("pop_count" = "value")


## Check against WDI numbers
municipality_pop_2012_2020_melted %>%
  group_by(year) %>%
  summarise(total_pop = sum(pop_count))



# Merge NTL and population ------------------------------------------------
merged_df <- merged_df %>%
  left_join(municipality_pop_2012_2020_melted, by = c("uid", "year"))


# Export ------------------------------------------------------------------
saveRDS(merged_df, file.path(lbn_file_path,
                             "Nighttime_Lights",
                             "final",
                             "lbn_municipality_ntl_pop.Rds"))

saveRDS(merged_df, file.path(lbn_onedrive_dir,
                             "data",
                             "municipalities",
                             "lbn_municipality_ntl_pop.Rds"))

write_csv(merged_df, file.path(lbn_onedrive_dir,
                             "data",
                             "municipalities",
                             "lbn_municipality_ntl_pop.csv"))

