## Extract VIIRS for Yemen


# Load Data ---------------------------------------------------------------
# Step 1: Read shapefile

yemen <- readRDS(file.path(yem_file_path,
                           "Boundaries",
                           "final",
                           "yem_landuse_clusters.Rds"))

# Step 2: Load VIIRS data
viirs_all <- raster(file.path(yem_file_path,
                                 "Nighttime_Lights",
                                 "raw",
                                 "monthly",
                                 "yemen_viirs_raw_monthly_start_201204_end_202303_avg_rad.tif"))



# Extract VIIRS ----------------------------------------------------------------
viirs_stacked_df <- lapply(1:130, function(i){
  
  print(i)
  
  viirs <- raster(file.path(yem_file_path, 
                             "Nighttime_Lights", 
                             "raw", 
                             "monthly", 
                             "yemen_viirs_raw_monthly_start_201204_end_202303_avg_rad.tif"), i) %>% velox()
  

  viirs_mean <- viirs$extract(sp = yemen, fun=function(x) mean(x, na.rm=T))
  
  viirs_df <- data.frame(viirs_mean = viirs_mean,
                         uid = yemen$uid,
                         viirs_time_id = i)
  
  return(viirs_df)
}) %>% bind_rows()

#### Add year / month
year <- 2012
month <- 4
viirs_stacked_df$year <- NA
viirs_stacked_df$month <- NA
for(i in unique(viirs_stacked_df$viirs_time_id)){
  
  viirs_stacked_df$year[viirs_stacked_df$viirs_time_id %in% i]  <- year 
  viirs_stacked_df$month[viirs_stacked_df$viirs_time_id %in% i] <- month 
  
  month <- month + 1
  
  if(month == 13){
    month <- 1
    year <- year + 1
  }
}

#### Add Data
yemen_df <- merge(yemen@data, viirs_stacked_df, by = "uid")


#Export -----------------------------------------------------------------------
saveRDS(yemen_df, file.path(yem_file_path,
                            "Nighttime_Lights",
                            "final",
                            "yem_landuse_clusters_viirs_monthly.Rds"))

write.csv(yemen_df,
          file.path(yem_file_path,
          "Nighttime_Lights",
          "final",
          "yem_landuse_clusters_viirs_monthly.csv"))


