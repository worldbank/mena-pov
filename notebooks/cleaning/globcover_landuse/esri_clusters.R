
# Set the folder path and filenames
folder_path <- file.path(yem_file_path, 
                         "Landcover", 
                         "raw", 
                         "esri")

filenames <- c("39P_20220101-20230101.tif", 
               "39Q_20220101-20230101.tif", 
               "38P_20220101-20230101.tif", 
               "38Q_20220101-20230101.tif")

# Create an empty list to store the rasters
raster_list <- list()

# Loop through the filenames and read the rasters
for (filename in filenames) {
  raster_file <- raster(file.path(folder_path, filename))
  raster_list[[filename]] <- raster_file
}

# Access the rasters using their names
landcover_yem1 <- raster_list[["39P_20220101-20230101.tif"]]
landcover_yem2 <- raster_list[["39Q_20220101-20230101.tif"]]
landcover_yem3 <- raster_list[["38P_20220101-20230101.tif"]]
landcover_yem4 <- raster_list[["38Q_20220101-20230101.tif"]]



# Combine the rasters -----------------------------------------------------
#transform the projection of the yemen shp
yemen <- spTransform(yemen, CRS(UTM_YEM))

#create extent of yemen
yem_extent <- extent(yemen)

#create an empty raster
combined_raster <- raster(yem_extent)

combined_raster <- crop(input_rasters[[1]], yem_extent)
combined_raster <- projectRaster(combined_raster, crs = crs(yem_extent))

for (i in 2:length(input_rasters)) {
  raster_file <- input_rasters[[i]]
  raster_cropped <- crop(raster_file, yem_extent)
  raster_cropped <- projectRaster(raster_cropped, crs = crs(yem_extent))
  
  # Align the origin and resolution with the combined raster
  raster_cropped <- alignExtent(raster_cropped, combined_raster, snap = "near")
  
  # Handle missing values
  raster_cropped[is.na(raster_cropped[])] <- 0 
  
  combined_raster <- combined_raster + raster_cropped
}



