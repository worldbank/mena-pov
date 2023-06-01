## Extract VIIRS for Yemen


# Load Data ---------------------------------------------------------------
# Step 1: Read shapefile
yemen <- readOGR(file.path(yem_file_path, 
                           "Boundaries", 
                           "raw"), 
                 layer = "gadm41_YEM_0")

# Step 2: Load VIIRS data
nightlights_stack <- stack(file.path(yem_file_path, 
                                 "Nighttime_Lights", 
                                 "raw", 
                                 "monthly", 
                                 "yemen_viirs_raw_monthly_start_201204_end_202303_avg_rad.tif"))




# Extract VIIRS -----------------------------------------------------------
# Extract nightlights values for Yemen
nightlights_values <- extract(nightlights_stack, yemen)
