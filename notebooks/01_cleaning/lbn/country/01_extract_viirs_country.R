# Extract VIIRS at the country level


# Load Data ---------------------------------------------------------------

# VIIRS post 2012
r_post2012 <- stack(file.path(lbn_file_path,
                     "Nighttime_Lights",
                     "raw", 
                     "monthly", 
                     "lebanon_viirs_raw_monthly_start_201204_end_202303_avg_rad.tif"))


shp <- st_read(file.path(lbn_file_path,
                         "Boundaries",
                         "gadm41_LBN_0.shp"))




# Check Projection --------------------------------------------------------

# Check CRS of both
raster_crs <- crs(r)
sf_crs <- st_crs(shp)


# Extract -----------------------------------------------------------------

ntl_mean_post2012    <- exact_extract(r_post2012, shp, fun = "mean")
ntl_median_post2012  <- exact_extract(r_post2012, shp, fun = "median")

ntl_mean_post2012$GID_0 <- shp$GID_0
ntl_median_post2012$GID_0 <- shp$GID_0





# Process data ------------------------------------------------------------

process_data <- function(my_data, data_name, start_year = 2012, start_month = 4) {
  
  # Melt the data
  data_melted <- my_data %>%
    mutate(uid = 1:nrow(.)) %>%
    melt(id.vars = "GID_0") %>%
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
ntl_mean_melted <- process_data(ntl_mean_post2012, "ntl_mean")
ntl_median_melted <- process_data(ntl_median_post2012, "ntl_median")

# Merge with admin boundaries ---------------------------------------------
# List all the data frames you want to merge
list_of_dfs <- list(ntl_mean_melted,ntl_median_melted)

# Use reduce() to sequentially merge all data frames in the list
merged_df<- reduce(list_of_dfs, left_join, by = c("GID_0", "month", "year"))




# Export ------------------------------------------------------------------

saveRDS(merged_df,file.path(lbn_file_path,
                            "Nighttime_Lights",
                            "final",
                            "viirs_2012_2023.Rds"))



