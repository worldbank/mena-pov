
library(raster)
library(rhdf5)
library(cowplot)
library(sf)


# Load Data ---------------------------------------------------------------
morocco_shape <- st_read(file.path(mor_file_path,
                                   "Boundaries",
                                   "gadm41_MAR_4.json"))

tif_files <- list.files(path="C:/Users/wb569257/OneDrive - WBG/Mor_earthquake analysis/data/NTL/raw", 
                        pattern="\\.tif$", full.names=TRUE)

# read each file
raster_list <- lapply(tif_files, function(file) {
  r <- raster(file)
})



# Prepare data ------------------------------------------------------------
rast <- raster_list[[1]]

# Ensure the CRS match
if (!compareCRS(projection(rast), st_crs(morocco_shape))) {
  morocco_shape <- st_transform(morocco_shape, crs(rast))
}

# Convert the sf object to a Spatial object
morocco_spatial <- as(morocco_shape, "Spatial")

# Now mask the raster with the shapefile
ntl_Sep7 <- mask(rast, morocco_spatial)









df1 <- as.data.frame(ntl_Sep7,xy=TRUE)
colnames(df1) <- c("lon", "lat", "value")
ho
hist(df1$value)

ggplot() +
  geom_raster(data = df1, 
              aes(x = lon, y = lat, 
                  fill = value)) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "red",
                       midpoint = 4.5) +
  labs(fill = "Nightlights \n(Sept 8)") +
  coord_quickmap() + 
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))



