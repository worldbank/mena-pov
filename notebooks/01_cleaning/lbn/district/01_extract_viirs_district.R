# Create datasets that include:
# 2. Population, NTL, NDVI (district level)
# 3. Expenditure, NTL (segment level)


# Load data ---------------------------------------------------------------
viirs_avg_rad <- stack(file.path(lbn_file_path,
                                 "Nighttime_Lights",
                                 "raw", 
                                 "monthly", 
                                 "lebanon_viirs_raw_monthly_start_201204_end_202303_avg_rad.tif"))

district_sp <- st_read(file.path(lbn_file_path,
                                 "Boundaries",
                                 "gadm41_LBN_3.shp")) %>% as_Spatial()


# Extract VIIRS ----------------------------------------------------------------
viirs_stacked_df <- lapply(1:130, function(i){
  
  print(i)
  
  viirs <- raster(file.path(data_file_path,
                            "viirs",
                            "rawdata",
                            "monthly",
                            "iraq_viirs_raw_monthly_start_201204_avg_rad.tif"), i) %>% velox()
  
  viirs_mean <- viirs$extract(sp = iraq_adm3, fun=function(x) mean(x, na.rm=T))
  
  viirs_df <- data.frame(viirs_mean = viirs_mean,
                         uid = iraq_adm3$uid,
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
iraq_adm3_df <- merge(iraq_adm3@data, viirs_stacked_df, by = "uid")

# Export -----------------------------------------------------------------------
saveRDS(iraq_adm3_df, file.path(data_file_path,
                                "cities_towns",
                                "finaldata",
                                "individual_files",
                                "irq_viirs_monthly.Rds"))
