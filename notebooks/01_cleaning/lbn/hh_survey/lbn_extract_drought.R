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
# Path to the drought dataset
folder_path <- file.path(mena_file_path, "Hazards", "Drought(CDI)")

date_list <- c("202001", "202002", "202003", "202004", "202005",
               "202006", "202007", "202008", "202009", "202010",
               "202011", "202012")

raster_list <- list()
extracted_values <- list()


for (date in date_list) {
  pattern <- paste0("FINAL_CDI_", date, ".tif")
  
  # Filter raster files based on pattern and date
  files <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)
  
  # Read the raster files
  for (file in files) {
    raster <- raster(file)
    raster_list[[date]] <- raster
  
    # Reproject the raster to match the shapefile
    raster_proj <- projectRaster(raster, crs = st_crs(merged_seg))
    
    # Clip the raster to the extent of the shapefile
    cropped_raster <- crop(raster_proj, merged_seg)
    
    # Extract raster values for the segments
    values <- extract(cropped_raster, merged_seg, fun = mean, na.rm = T, df = F)
    extracted_values[[date]] <- values
  }
}


# Convert to a data frame -------------------------------------------------

# Convert the extracted raster values to a data frame
data <- as.data.frame(do.call(cbind, lapply(extracted_values, function(x) unlist(x, use.names = FALSE))))



# Interpolate missing values ----------------------------------------------
# Interpolate missing values using bilinear approximation
data_interpolated_bilinear <- na.approx(data, method = "linear")

# Replace missing values with the interpolated values
data[is.na(data)] <- data_interpolated_bilinear[is.na(data)]


# Rename the columns of the data frame
colnames(data) <- date_list

# Add a unique ID column
data$ID <- seq_len(nrow(data))

# Rename the columns of the data frame with date_list
colnames(data) <- c(date_list, "id")


# Reshape data frame ------------------------------------------------------
# Melt the data frame
melted_data <- melt(data, id.vars = "id", variable.name = "date", value.name = "drought_value")

# Split the "Date" column into "Year" and "Month" 
melted_data <- melted_data %>%
  separate(date, into = c("year", "month"), sep = 4) %>%
  select(id,year,month,drought_value)

#Summarise drought by year
melted_data_summ <- melted_data %>%
  group_by(id,year) %>%
  summarize(avg_drought_value = mean(drought_value, na.rm = T),
            min_drought_value = min(drought_value,na.rm = T),
            max_drought_value = max(drought_value,na.rm = T))

# Export ------------------------------------------------------------------
saveRDS(melted_data_summ, file.path(lbn_file_path,
                               "Hazards",
                               "final",
                               "drought_2020.Rds"))
