##  Relative Wealth Index


# Load Data ---------------------------------------------------------------
lbn_gadm <- readOGR(file.path(lbn_file_path,
                              "Boundaries"),
                    layer = "gadm41_LBN_1")

lbn_gadm3 <- readOGR(file.path(lbn_file_path,
                               "Boundaries"),
                     layer = "gadm41_LBN_3") %>% st_as_sf()

rwi <- read.csv(file.path(lbn_file_path,
                          "RWI",
                          "LBN_relative_wealth_index.csv"))



# Aggregate data ----------------------------------------------------------

#convert rwi to sf
rwi_sf = st_as_sf(rwi, coords = c("longitude", "latitude"), 
                  crs = 4326, agr = "constant")


# Perform a spatial join between the points and shapefile using the st_join() function
agg_data <- st_join(lbn_gadm3,rwi_sf, join = st_intersects)

summary_data <- agg_data %>%
  group_by(NAME_3) %>%
  summarize(mean_rwi = mean(rwi)) %>%
  st_as_sf()



# Add a dataframe with main cities ----------------------------------------
# Create the dataframe
main_cities <- data.frame(name = c("Tyre", "Sidon","Baalbek"),
                          lat = c(33.2705, 33.5571, 34.0047),
                          lon = c(35.2038, 35.3729, 36.2110))

# Plot --------------------------------------------------------------------
# First map: Relative Wealth Index (RWI)

 ggplot() +
  geom_sf(data = st_as_sf(lbn_gadm), color = "blue", fill = NA) +
  geom_sf(data = summary_data, aes(fill = mean_rwi), color = NA, alpha = 0.6)+
  geom_sf_text(data =st_as_sf(lbn_gadm), aes(label = str_wrap(NAME_1, 1)), size = 2)+
  geom_text(data = main_cities, aes(x = lon, y = lat, label = name), size = 1.5) +
  labs(fill = "Mean RWI") +
  theme_void() +
  coord_sf()+
  scale_fill_gradientn(colors = c("blue", "white", "green"),
                       values= c(0.0 , 0.5, 1.0))

