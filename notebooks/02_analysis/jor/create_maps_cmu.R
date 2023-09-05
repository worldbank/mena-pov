# Set options
options(scipen = 999)

# Load Data ---------------------------------------------------------------
# Read Jordan shapefile
jor_shp <- st_read(file.path(jor_file_path, "Boundaries", "gadm41_JOR_0.shp"))

# Read relative wealth index data
rwi <- read_csv(file.path(jor_file_path, "RWI", "jor_relative_wealth_index.csv"))

# Read population density raster
pop_density <- raster(file.path(jor_file_path, "Population", "raw", "jor_pd_2020_1km_UNadj.tif"))

# Read nighttime lights data stack and extract the latest layer (Feb 2023)
ntl <- stack(file.path(jor_file_path, "Nighttime_Lights", "raw", "jordan_viirs_raw_monthly_start_201204_end_202303_avg_rad.tif"))
ntl_feb <- ntl[[nlayers(ntl) - 1]]

#Read Drought Data
pdsi <- raster(file.path(jor_file_path, "Hazards", "pdsi_drought", "raw", "PDSI_Jordan_December_2022.tif"))



# Prepare data ------------------------------------------------------------
# Prepare relative wealth index spatial data frame
rwi_sf_temp <- st_as_sf(rwi, coords = c("longitude", "latitude"))
rwi_sf <- st_set_crs(rwi_sf_temp, 4326) %>% 
  st_transform(st_crs(jor_shp))

# Prepare population density data frame
df_pop <- as.data.frame(pop_density, xy = TRUE, na.rm = TRUE)
breaks_custom <- c(0, 100, 5000, 10000, 24000)
labels_custom <- c("[0,100)", "[100,1000)", "[1000,10000)", "[10000,24000]")
df_pop$category <- cut(df_pop$jor_pd_2020_1km_UNadj, breaks = breaks_custom, labels = labels_custom,
                       include.lowest = TRUE, right = FALSE)
df_pop <- df_pop[!is.na(df_pop$category), ]
df_pop$category <- factor(df_pop$category, levels = labels_custom)

# Mask nighttime lights raster with Jordan shapefile
df_masked <- ntl_feb %>% 
  mask(jor_shp) %>% 
  as.data.frame(xy = TRUE, na.rm = TRUE)


#Mask PDSI with Jordan shapefile
df_pdsi <- pdsi %>%
  mask(jor_shp) %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)


# Plot maps ---------------------------------------------------------------
# Population Density
ggplot() +
  geom_tile(data = df_pop, aes(x = x, y = y, fill = category), width = res(pop_density)[1], height = res(pop_density)[2]) +
  scale_fill_manual(values = brewer.pal(length(breaks_custom) - 1, "YlGnBu"), 
                    name = "Pop. Density Range \n(People/sq.km)") +
  labs(title = "Population Density, Jordan", subtitle = "Source: Population Density 2020 UN adjusted, WorldPop") +
  coord_sf() +
  theme_void()
ggsave(file = file.path(jor_onedrive_dir,"pop_density_jor.png"), height = 12, width = 16, units = c("in"))


# Nighttime Lights
ggplot() +
  geom_sf(data = jor_shp, fill = "black", alpha = 0.9, linewidth = 0.1) +
  geom_tile(data = df_masked, aes(x = x, y = y, fill = df_masked$avg_rad_128), width = res(ntl_feb)[1], height = res(ntl_feb)[2]) +
  scale_fill_gradientn(colors = colorRampPalette(c("black", "yellow"))(length(breaks_custom) - 1), values = scales::rescale(breaks_custom), na.value = NA, name = "Light Intensity") +
  labs(title = "Nighttime Lights, Jordan", subtitle = "Source: February 2023, National Oceanic and Atmospheric Administration") +
  coord_sf() +
  theme_void()
ggsave(file = file.path(jor_onedrive_dir,"ntl_jor.png"), height = 12, width = 16, units = c("in"))

# Relative Wealth Index
ggplot() +
  geom_sf(data = jor_shp, fill = "black", alpha = 0.9, linewidth = 0.1) +
  geom_sf(data = rwi_sf, aes(color = rwi), size = 0.8, alpha = 2) +
  scale_color_gradient(low = "yellow", high = "red", name = "RWI", limits = c(-0.5, 2)) +
  theme_void() +
  labs(x = "", y = "", title = "Relative Wealth Index, Jordan", subtitle = "Source: Data for Good Project, Meta")
ggsave(file = file.path(jor_onedrive_dir,"rwi_jor.png"), height = 12, width = 16, units = c("in"))


#PDSI

# Plotting the masked raster
ggplot(df_pdsi, aes(x = x, y = y)) +
  geom_raster(aes(fill = pdsi)) +  # Assuming "layer" is the column containing the raster values; adjust if different
  scale_fill_gradientn(colors = brewer.pal(9, "Greens"),  # Using reverse to have light to dark gradient
                       name = "PDSI") +
  labs(title = "Palmer Severity Drought Index, Jordan",
       subtitle = "Source: December 2022, National Oceanic and Atmospheric Administration",
       caption = "Negative PDSI values indicate extremely dry conditions, while positive values signify extremely wet conditions.") +
  coord_sf() +
  theme_void() +
  theme(legend.position = "right")

ggsave(file = file.path(jor_onedrive_dir,"pdsi_jor.png"), height = 12, width = 16, units = c("in"))



