## PDSI
library(ncdf4)

# Load Data
r <- raster(file.path(mena_file_path, "Hazards", "PDSI", "raw", "pdsi.mon.mean.selfcalibrated.nc"))
last_layer <- r[[nlayers(r)]]

shp <- st_read(file.path(mena_file_path, "Boundaries", "MENA_ADM2.shp"))
pop_raster <- raster(file.path(mena_file_path, "Population", "raw","gpw_v4_population_count_rev11_2000_30_sec.tif"))



# Ensure same CRS ---------------------------------------------------------
shp <- st_transform(shp, crs(last_layer))
sp_shp <- as(shp, "Spatial")
if (!identical(proj4string(sp_shp), crs(last_layer))) {
  sp_shp <- spTransform(sp_shp, CRS = crs(last_layer))
}



# Extract values ----------------------------------------------------------
extracted_values <- extract(last_layer, sp_shp)
shp$extracted <- extracted_values
shp$mean_extracted <- sapply(extracted_values, mean, na.rm = TRUE)



# Create drought categories variable
shp$categories <- cut(shp$mean_extracted,
                             breaks = c(-Inf, -4, -3, -2, 1.9, 2.9, 3.9, Inf),
                             labels = c("Extreme Drought",
                                        "Severe Drought",
                                        "Moderate Drought",
                                        "Near Normal",
                                        "Unusual moist spell",
                                        "Very Moist Spell",
                                        "Extremely Moist"),
                             right = TRUE,
                             include.lowest = TRUE)

ggplot(shp, aes(x=categories)) +
  geom_bar() +
  theme_minimal() +
  labs(title="Category Distribution", x="Categories", y="Count")



