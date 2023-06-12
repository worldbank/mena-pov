# Extract CO2

# Extract Drought for 2020 - Lebanon


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



# Load drought rasters and extract raster values --------------------------

raster <- raster(file.path(mena_file_path,
                          "Hazards",
                          "SEDAC_PM2_5",
                          "sdei-global-annual-gwr-pm2-5-modis-misr-seawifs-aod-v4-gl-03-2019.tif"))



# Match projection --------------------------------------------------------

# Reproject the raster to match the shapefile
raster_proj <- projectRaster(raster, crs = st_crs(merged_seg))

# Clip the raster to the extent of the shapefile
cropped_raster <- crop(raster_proj, merged_seg)


# Extract raster values ---------------------------------------------------



# Extract raster values for the segments
values <- extract(cropped_raster, merged_seg, fun = mean, na.rm = T, df = F)

