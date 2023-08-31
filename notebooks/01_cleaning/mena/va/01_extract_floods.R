library(raster)
library(sf)
library(pbapply)

# Read the shapefile
MENA_shp <- st_read(file.path(mena_file_path, "Boundaries", "MENA_ADM2.shp"))

# Creating a named vector for matching 'WB_ADM0_NA' values to ISO alpha-3 codes
country_match <- c(
  "Algeria" = "DZA", "Bahrain" = "BHR", "Djibouti" = "DJI","Islamic Republic of Iran" = "IRN", "Iraq" = "IRQ",
  "Israel" = "ISR", "Jordan" = "JOR", "Kuwait" = "KWT", "Lebanon" = "LBN",
  "Libya" = "LBY", "Malta" = "MLT", "Morocco" = "MAR", "Oman" = "OMN",
  "Qatar" = "QAT", "Saudi Arabia" = "SAU", "Syrian Arab Republic" = "SYR",
  "Tunisia" = "TUN", "United Arab Emirates" = "ARE", "Republic of Yemen" = "YEM",
  "Arab Republic of Egypt" = "EGY"
)

# Match and add the ISO codes to the shapefile as a new column
MENA_shp$country_code <- country_match[MENA_shp$WB_ADM0_NA]

all_countries <- unique(MENA_shp$country_code)

# Initializing a results list to store the mean values
results_list <- list()

# Loop through each country
for(country in all_countries) {
  
  # Load the raster specific to this country
  raster_path <- paste0("M:/",country,"/GEO/Hazards/fathom_floods/pluvial/P_1in10.tif")
  flood_raster <- raster(raster_path)
  
  # Convert the values -9999 and 999 to NA
  flood_raster[flood_raster == -9999] <- NA
  flood_raster[flood_raster == 999] <- NA
  
  # Ensure raster and shapefile are in the same CRS
  flood_raster <- projectRaster(flood_raster, crs = st_crs(MENA_shp)$proj4string)
  
  # Subset shapefile for current country
  country_shp <- subset(MENA_shp, country_code == country)
  
  # Get unique administrative level 2 names for the current country
  adm2_names <- unique(country_shp$WB_ADM2_NA)
  
  for(idx in seq_along(adm2_names)) {
    adm2 <- adm2_names[idx]
    
    cat(paste0("Processing: ", country, " - ", adm2, " (", idx, " of ", length(adm2_names), ")\n"))
    
    # Subset shapefile for current administrative level 2 area
    adm2_shp <- subset(country_shp, WB_ADM2_NA == adm2)
    
    # Convert adm2_shp to SpatialPolygons for compatibility with raster package
    adm2_spatial <- as(adm2_shp, "Spatial")
    
    # Crop raster to the extent of the adm2 shapefile for efficiency
    adm2_raster <- crop(flood_raster, extent(adm2_spatial))
    
    # Extract raster values for the adm2 shape
    raster_values_df <- extract(adm2_raster, adm2_spatial, df=TRUE)
    
    # Calculate mean value for the adm2 area (ignoring NAs)
    mean_value <- mean(raster_values_df$layer, na.rm=TRUE)
    
    results_list[[paste0(country, "_", adm2)]] <- mean_value
  }
}

# Convert the results list to a data frame for better visualization and saving
results_df <- data.frame(country_adm2 = names(results_list), mean_value = unlist(results_list))

head(results_df)
