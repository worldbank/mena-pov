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


# Repeat this for ntl > 2 -------------------------------------------------

ntl_mean_prop2 <- exact_extract(r_t2, municipality_sp, fun = "mean")

ntl_mean_prop2_melted <- ntl_mean_prop2 %>%
  mutate(uid = 1:n()) %>%
  melt(, id = "uid") %>%
  mutate(variable = gsub("mean.avg_rad_","",as.numeric(variable)))



#### Add year / month
year <- 2012
month <- 4
ntl_mean_prop2_melted$year <- NA
ntl_mean_prop2_melted$month <- NA
for(i in unique(ntl_mean_prop2_melted$variable)){
  
  ntl_mean_prop2_melted$year[ntl_mean_prop2_melted$variable %in% i]  <- year 
  ntl_mean_prop2_melted$month[ntl_mean_prop2_melted$variable %in% i] <- month 
  
  month <- month + 1
  
  if(month == 13){
    month <- 1
    year <- year + 1
  }
}

ntl_mean_prop2_melted <- ntl_mean_prop2_melted %>%
  select(-variable) %>%
  rename("ntl_mean_prop2" = "value")


# Merge with admin boundaries ---------------------------------------------
municipality_sf <- municipality_sp %>%
  st_as_sf() %>%
  mutate(uid = 1:n()) %>%
  left_join(ntl_mean_prop2_melted, by = "uid")



# Extract population ------------------------------------------------------

# Load Data ---------------------------------------------------------------
setwd("M:/LBN/GEO/Population/raw")

municipality_sp <- st_read(file.path(lbn_file_path,
                                     "Boundaries",
                                     "gadm41_LBN_3.shp")) %>% as_Spatial()



rastlist <- list.files(path = "M:/LBN/GEO/Population/raw", pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)

#import all raster files in folder using lapply
allrasters <- lapply(rastlist, raster)



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
  rename("population" = "value")


# Merge NTL and population ------------------------------------------------
merged_df <- ntl_mean_prop2_melted %>%
  left_join(municipality_pop_2012_2020_melted, by = c("uid", "year"))



# Export ------------------------------------------------------------------
saveRDS(merged_df, file.path(lbn_file_path,
                             "Nighttime_Lights",
                             "final",
                             "lbn_municipality_pop.Rds"))



