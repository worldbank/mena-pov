## Extract NO2

# Extract PM2.5

# Load segment shapefile --------------------------------------------------
seg1 <- st_read(file.path(lbn_file_path,
                          "Team",
                          "Projects",
                          "Sampling",
                          "01- LVAP Listing map data",
                          "Phase1",
                          "Segment",
                          "Shapefile",
                          "Segmentation_Phase1.shp"))


seg2 <- st_read(file.path(lbn_file_path,
                          "Team",
                          "Projects",
                          "Sampling",
                          "01- LVAP Listing map data",
                          "Phase2",
                          "Segment",
                          "Shapefile",
                          "Segmentation_Phase2.shp"))

## merge the two shapefiles
merged_seg <- rbind(seg1, seg2)


# Load raster -------------------------------------------------------------
raster <- raster(file.path(mena_file_path,
                           "Hazards",
                           "NO2",
                           "2020_NO2.tif"))


# Projection --------------------------------------------------------------
# Reproject the raster to match the shapefile
raster_proj <- projectRaster(raster, crs = st_crs(merged_seg))

# Clip the raster to the extent of the shapefile
cropped_raster <- crop(raster_proj, merged_seg)


# Extract raster values ---------------------------------------------------

# Extract raster values for the segments
data <- extract(cropped_raster, merged_seg, fun = mean, na.rm = TRUE) %>% as.data.frame()

# Interpolate missing values using bilinear approximation
data_interpolated_bilinear <- na.approx(data, method = "linear")

# Replace missing values with the interpolated values
data[is.na(data)] <- data_interpolated_bilinear[is.na(data)]

# Add vars ----------------------------------------------------------------
# Add a unique ID column
data$id <- seq_len(nrow(data))

# Add year column
data$year <- 2020

# rename column
names(data)[names(data) == "V1"] <- "no2_value"


# Export ------------------------------------------------------------------
saveRDS(data, file.path(lbn_file_path,
                        "Hazards",
                        "final",
                        "no2_2020.Rds"))



