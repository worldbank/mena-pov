## Lebanon Maps for Poverty Analysis



# Load Data ---------------------------------------------------------------
enum_areas_1 <- readOGR(file.path(lbn_file_path,
                                "Team",
                                "Projects",
                                "Sampling",
                                "01- LVAP Listing map data",
                                "Phase1",
                                "Segment",
                                "Shapefile"),
                      layer = "Segmentation_Phase1")


enum_areas_2 <- readOGR(file.path(lbn_file_path,
                                  "Team",
                                  "Projects",
                                  "Sampling",
                                  "01- LVAP Listing map data",
                                  "Phase2",
                                  "Segment",
                                  "Shapefile"),
                        layer = "Segmentation_Phase2")



lbn_gadm <- readOGR(file.path(lbn_file_path,
                              "Boundaries"),
                              layer = "gadm41_LBN_1")

#relative wealth index
rwi <- read.csv(file.path(lbn_file_path,
                          "RWI",
                          "LBN_relative_wealth_index.csv"))

#predicted poverty estimates
est_pov <- read.csv(file.path(lbn_file_path,
                              "Team",
                              "Projects",
                              "pov_est",
                              "LBN_predicted_poverty_nat.csv"))



# Appending Enumeration Areas -----------------------------------------------
#Merge phase 1 and phase 2
enum_areas <- rbind(enum_areas_1, enum_areas_2)
enum_areas <- st_as_sf(enum_areas)


# Merging Poverty Estimates -------------------------------------------------------
est_pov <- est_pov %>%
  rename("UID_Name" = "segment")
enum_areas <- merge(enum_areas,est_pov, by=c("UID_Name"))



# Merging RWI -------------------------------------------------------------

#convert to sf
rwi_sf = st_as_sf(rwi, coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")

# Perform a spatial join between the points and shapefile using the st_join() function
aggregated_data <- st_join(enum_areas,rwi_sf, join = st_intersects)

summary_data <- aggregated_data %>%
  group_by(UID_Name) %>%
  summarize(mean_rwi = mean(rwi)) %>%
  st_as_sf()

# Convert summary_data to sf format if it's not already in sf format
summary_data_sf <- st_as_sf(summary_data)


# Plot --------------------------------------------------------------------
# RWI vs. OLS
location <- geocode("Lebanon")
center_longitude <- location$lon
center_latitude <- location$lat

map <- get_googlemap(center = c(lon =center_longitude , lat =center_latitude ), 
                     zoom = 8, maptype = "terrain")

# First map: Relative Wealth Index (RWI)

rwi_map <- ggmap(map,darken = c(0.4, "white")) +
  geom_sf(data = st_as_sf(lbn_gadm), 
          fill = NA, 
          color = "black",
          inherit.aes = FALSE) +
  geom_sf(data = enum_areas,
          fill = NA,
          color = "grey",
          inherit.aes = FALSE) +
  geom_sf(data = summary_data_sf, aes(fill = mean_rwi),
          inherit.aes = FALSE)+
  labs(fill = "Mean RWI") +
  theme_void() +
  coord_sf()+
  scale_fill_gradientn(colors = c("blue", "white", "red"),
                       values= c(0.0 , 0.5, 1.0))

rwi_map

# Second map: Poverty Estimation (OLS)
ols <- ggmap(map,darken = c(0.4, "white")) +
  geom_sf(data = st_as_sf(lbn_gadm), 
          fill = NA, 
          color = "black",
          inherit.aes = FALSE) +
  geom_sf(data = enum_areas[enum_areas$simulated == 0,],
          inherit.aes = FALSE,
          fill = NA,
          color = "grey") +
  geom_sf(data = enum_areas[enum_areas$simulated == 1,],
          aes(fill = poor685_ols, color = "Estimated"),
          inherit.aes = FALSE) +
  labs(fill = "Poverty Est.\n(OLS)", color = "") +
  theme_void() +
  coord_sf()+
  scale_fill_gradientn(colors = c("blue", "white", "red"),
                       values= c(0.0 , 0.5, 1.0))

ols

lasso <- ggmap(map,darken = c(0.4, "white")) +
  geom_sf(data = st_as_sf(lbn_gadm), 
          fill = NA, 
          color = "black",
          inherit.aes = FALSE) +
  geom_sf(data = enum_areas[enum_areas$simulated == 0,],
          fill = NA,
          color = "grey",
          inherit.aes = FALSE) +
  geom_sf(data = enum_areas[enum_areas$simulated == 1,],
          aes(fill = poor685_lasso, color = "Estimated"),
          inherit.aes = FALSE) +
  scale_fill_gradientn(colors = c("blue", "white", "red"),
                       values= c(0.0 , 0.5, 1.0)) +
  labs(fill = "Poverty Est.\n(Lasso)", color = "") +
  theme_void() +
  coord_sf()


lasso

rf <- ggmap(map,darken = c(0.4, "white")) +
  geom_sf(data = st_as_sf(lbn_gadm), fill = NA, color = "black",
          inherit.aes = FALSE) +
  geom_sf(data = enum_areas[enum_areas$simulated == 0,],color = "grey",
          inherit.aes = FALSE) +
  geom_sf(data = enum_areas[enum_areas$simulated == 1,],
          aes(fill = poor685_lasso, color = "Estimated"),
          inherit.aes = FALSE) +
  labs(fill = "Poverty Est.\n(Random Forest)", color = "") +
  theme_void() +
  coord_sf()+
  scale_fill_gradientn(colors = c("blue", "white", "red"),
                       values= c(0.0 , 0.5, 1.0))
rf

