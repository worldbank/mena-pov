## Load NTL and create trends


# Load data ---------------------------------------------------------------
lbn_shp <- readOGR(file.path(lbn_file_path,
                         "Boundaries",
                         layer = "gadm41_LBN_0.shp"))

#### VIIRS
viirs_avg_rad <- stack(file.path(lbn_file_path,
                                 "Nighttime_Lights",
                                 "raw", 
                                 "monthly", 
                                 "lebanon_viirs_raw_monthly_start_201204_end_202303_avg_rad.tif"))


num_bands <- dim(viirs_avg_rad)[3]

# Prep Country SpatialPolygon that Limit Cells in Analysis ------------------
lbn_shp$in_country <- 1
lbn_shp <- lbn_shp %>% spTransform(CRS("+init=epsg:4326"))
lbn_shp <- gSimplify(lbn_shp, tol = .01)

## Add Variable
lbn_shp$id <- 1

# Determine which cells are in Analysis -------------------------------------
# Determine if cell should be in analysis: within lbn

## Create spatial points file from VIIRS
r_tmp <- raster(file.path(lbn_file_path,
                          "Nighttime_Lights",
                          "raw", 
                          "monthly", 
                          "lebanon_viirs_raw_monthly_start_201204_end_202303_avg_rad.tif"), band = 1)


r_tmp_coords <- coordinates(r_tmp) %>% as.data.frame
coordinates(r_tmp_coords) <- ~x+y
crs(r_tmp_coords) <- CRS("+init=epsg:4326")
r_tmp_coords$id <- 1:length(r_tmp_coords)

#ensure its the same projection
crs(lbn_shp) <- crs(r_tmp_coords)

## Indicate whether intersects country/road
r_OVER_lbn   <- over_chunks(r_tmp_coords, lbn_shp, "sum", 10000)

cell_in_analysis <- (r_OVER_lbn$id %in% 1) 

# Coordinates ------------------------------------------------------------------
viirs_coords_in_lbn <- r_tmp_coords[cell_in_analysis,] %>% coordinates %>% as.data.frame
names(viirs_coords_in_lbn) <- c("lon","lat")

viirs_coords_in_lbn$id <- 1:nrow(viirs_coords_in_lbn)

num_obs_per_band <- nrow(viirs_coords_in_lbn)

# Extract Values to Dataframe --------------------------------------------------
extract_raster_value_in_country <- function(band_num, raster_file_path, cell_in_analysis, var_name){
  
  r <- raster(raster_file_path, band=band_num)
  r_values <- r[]
  r_values <- r_values[cell_in_analysis]
  
  return(r_values)
}

## Extract values as vectors
avg_rad_df <- pbmclapply(1:num_bands, extract_raster_value_in_country, 
                         file.path(lbn_file_path,
                                   "Nighttime_Lights",
                                   "raw", 
                                   "monthly", 
                                   "lebanon_viirs_raw_monthly_start_201204_end_202303_avg_rad.tif"), 
                         cell_in_analysis, "viirs_rad", mc.cores=1) %>% unlist

## id and band vectors
id <- rep(1:num_obs_per_band, num_bands)
band <- rep(1:num_bands, each=num_obs_per_band)

## Make dataframe
# Dataframe from radiance/cloud cover/id/bands vectors
lbn_grid_viirs <- data.frame(avg_rad_df=avg_rad_df, 
                              id=id, 
                              band=band)
# Add in coordinates
lbn_grid_viirs <- merge(lbn_grid_viirs, viirs_coords_in_lbn, by="id")

head(lbn_grid_viirs)

# Add Month and Year -----------------------------------------------------------
lbn_grid_viirs$month <- NA
lbn_grid_viirs$year <- NA

month <- 4
year <- 2012
for(band_num in 1:max(lbn_grid_viirs$band)){
  print(band_num)
  lbn_grid_viirs$month[lbn_grid_viirs$band %in% band_num] <- month
  lbn_grid_viirs$year[lbn_grid_viirs$band %in% band_num] <- year
  
  month <- month + 1
  if(month == 13){
    month <- 1
    year <- year + 1
  }
}


# Export ------------------------------------------------------------------
saveRDS(lbn_grid_viirs,file.path(lbn_file_path,
                                 "Nighttime_Lights",
                                 "final",
                                 "lbn_grid_viirs.Rds"))


