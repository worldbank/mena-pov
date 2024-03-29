# Set options
options(scipen = 999)
library(RColorBrewer)

# Load Data ---------------------------------------------------------------
# Read Jordan shapefile
jor_shp <- st_read(file.path(jor_file_path, "Boundaries", "gadm41_JOR_0.shp"))
jor_district <- st_read(file.path(jor_file_path, "Boundaries", "gadm41_JOR_2.shp"))

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

rwi_joined <- st_join(rwi_sf, jor_district, left = TRUE)

# Summarize the data by district to compute mean_rwi
rwi_dist_df <- rwi_joined %>%
  group_by(NAME_2) %>%
  summarize(mean_rwi = mean(rwi, na.rm = TRUE)) %>%
  st_drop_geometry() # Remove geometry column after summarizing

# Merge summarized data back to the original shapefile
jor_district <- left_join(jor_district, rwi_dist_df, by = "NAME_2")


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



# Apply a smoothing matrix to the raster; this example uses a simple average
matrix <- matrix(1/9, ncol=3, nrow=3)
smoothed_r <- focal(pdsi, w=matrix)


# Plot maps ---------------------------------------------------------------
# Population Density
ggplot() +
  geom_raster(data = df_pop, aes(x = x, y = y, fill = category), interpolate = TRUE) +
  scale_fill_manual(values = brewer.pal(length(breaks_custom) - 1, "YlGnBu"), 
                    name = "Population Density \n(People/sq.km)") +
  labs(title = "Population Density, Jordan", subtitle = "Source: Population Density 2020 UN adjusted, WorldPop") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(4, 'cm'), #change legend key size
        legend.key.height = unit(1.5, 'cm'), #change legend key height
        legend.key.width = unit(1.5, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=14)) #change legend text font size

ggsave(file = file.path(jor_onedrive_dir,"pop_density_jor.png"), height = 12, width = 16, units = c("in"))


# Nighttime Lights
ggplot() +
  geom_sf(data = jor_shp, fill = "black", alpha = 0.9, linewidth = 0.1) +
  geom_raster(data = df_masked, aes(x = x, y = y, fill = df_masked$avg_rad_128), interpolate = TRUE) +
  scale_fill_gradientn(colors = colorRampPalette(c("black", "yellow"))(length(breaks_custom) - 1), values = scales::rescale(breaks_custom), na.value = NA, name = "Light Intensity") +
  labs(title = "Nighttime Lights, Jordan", subtitle = "Source: February 2023, National Oceanic and Atmospheric Administration") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(4, 'cm'), #change legend key size
        legend.key.height = unit(1.5, 'cm'), #change legend key height
        legend.key.width = unit(1.5, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=14)) #change legend text font size
ggsave(file = file.path(jor_onedrive_dir,"ntl_jor.png"), height = 12, width = 16, units = c("in"))

# Relative Wealth Index
library(RColorBrewer)
# Define the breaks and corresponding colors
break_points <- c(-0.2, 0, 0.2, 0.6, 0.8, 1)
color_palette <- colorRampPalette(brewer.pal(5, "YlOrRd"))(length(break_points) - 1)

hist(jor_district$mean_rwi)

ggplot() +
  geom_sf(data = jor_district, fill = "black", alpha = 0.8, linewidth = 0.1) +
  geom_sf(data = jor_district, aes(fill = mean_rwi), size = 2, alpha = 2) +
  scale_fill_gradientn(colors = color_palette,
                       name = "Relative Wealth Index (Mean)", 
                       limits = c(-0.2, 1)) +
  theme_void() +
  labs(x = "", y = "", title = "Relative Wealth Index (District), Jordan", subtitle = "Source: Data for Good Project, Meta") +
  theme(legend.key.size = unit(4, 'cm'), #change legend key size
        legend.key.height = unit(1.5, 'cm'), #change legend key height
        legend.key.width = unit(1.5, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=14)) #change legend text font size
ggsave(file = file.path(jor_onedrive_dir,"rwi_jor.png"), height = 12, width = 16, units = c("in"))


#PDSI

# Plotting the masked raster
ggplot(df_pdsi, aes(x = x, y = y)) +
  geom_raster(aes(fill = pdsi), interpolate = T) +  # Assuming "layer" is the column containing the raster values; adjust if different
  scale_fill_gradientn(colors = brewer.pal(9, "Greens"),  # Using reverse to have light to dark gradient
                       name = "Palmer Severity \nDrought Index") +
  labs(title = "Palmer Severity Drought Index, Jordan",
       subtitle = "Source: December 2022, National Oceanic and Atmospheric Administration",
       caption = "Negative values indicate extremely dry conditions, while positive values signify extremely wet conditions.") +
  coord_sf() +
  theme_void() +
  theme(legend.key.size = unit(4, 'cm'), #change legend key size
        legend.key.height = unit(1.5, 'cm'), #change legend key height
        legend.key.width = unit(1.5, 'cm'), #change legend key width
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=14),
        legend.position = "right") #change legend text font size

ggsave(file = file.path(jor_onedrive_dir,"pdsi_jor.png"), height = 12, width = 16, units = c("in"))



