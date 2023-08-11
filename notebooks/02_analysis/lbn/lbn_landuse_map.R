# Load Data ---------------------------------------------------------------
landuse <- readOGR(file.path(lbn_file_path,
                         "Landcover",
                         "raw"),
                   layer = "Landcover_landuse2017") %>% st_as_sf()

lbn_gadm <- readOGR(file.path(lbn_file_path,
                              "Boundaries"),
                    layer = "gadm41_LBN_1")

lbn_gadm3 <- readOGR(file.path(lbn_file_path,
                               "Boundaries"),
                     layer = "gadm41_LBN_3") %>% st_as_sf()



# Create the dataframe
main_cities <- data.frame(name = c("Tyre", "Sidon","Baalbek"),
                          lat = c(33.2705, 33.5571, 34.0047),
                          lon = c(35.2038, 35.3729, 36.2110))

# Plot --------------------------------------------------------------------

ggplot() +
  geom_sf(data = st_as_sf(lbn_gadm), color = "black", fill = NA) +
  geom_sf(data = landuse, aes(fill = Level_2), color = NA, alpha = 0.6)+
  geom_sf_text(data =st_as_sf(lbn_gadm), aes(label = str_wrap(NAME_1, 1)), size = 2)+
  geom_text(data = main_cities, aes(x = lon, y = lat, label = name), size = 1.5) +
  labs(fill = "Landuse Type") +
  theme_void() +
  coord_sf()

