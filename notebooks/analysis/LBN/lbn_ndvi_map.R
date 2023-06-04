## NDVI


# Load Data ---------------------------------------------------------------
ndvi <- raster(file.path(lbn_file_path,
                         "NDVI",
                         "raw",
                         "ndvi_modis_lebanon_monthly_1km_2023.tif"))

lbn_gadm <- readOGR(file.path(lbn_file_path,
                              "Boundaries"),
                    layer = "gadm41_LBN_1")

lbn_gadm3 <- readOGR(file.path(lbn_file_path,
                               "Boundaries"),
                     layer = "gadm41_LBN_3") %>% st_as_sf()


# Convert raster to df ----------------------------------------------------
ndvi_df <- as.data.frame(ndvi, xy = TRUE, na.rm = T)



# Plot --------------------------------------------------------------------
ggplot() +
  geom_raster(data = ndvi_df, aes(x = x, y = y, fill = NDVI_mean)) +
  geom_sf(data = st_as_sf(lbn_gadm), color = "blue", fill = NA) +
  geom_sf_text(data =st_as_sf(lbn_gadm), aes(label = str_wrap(NAME_1, 1)), size = 2)+
  scale_fill_gradientn(colours = rev(terrain.colors(10)), na.value = "transparent") +
  labs(fill = "Mean NDVI") +
  coord_sf() +
  theme_void()

