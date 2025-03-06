# ================================================================
# Script Name: 01_GRAPH_HAZARDS
# Purpose: Create maps with their thresholds for each hazard
# Input Dataset: merged_dataset_10km.Rds, grid_10km_poor_215_685.Rds,
# MENA_ADM2.shp, MENA_Country.shp
# Output Dataset:
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================

options(scipen =999)



# Load grid ---------------------------------------------------------------

grid_sf <- readRDS(file.path(final_replication,"merged_dataset_10km.Rds"))
grid_gsap_rwi <- readRDS(file.path(final_replication,"grid_10km_poor_215_685.Rds"))

# MENA shapefiles
shp <- st_read(file.path(raw_replication, "BOUNDARIES", "MENA_ADM2.shp"))
country <- st_read(file.path(raw_replication, "BOUNDARIES", "MENA_Country.shp"))

# Define common ggplot theme
common_theme <- theme(
  text = element_text(color = "#22211d"),
  axis.line = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  plot.title = element_text(hjust = 1),
  plot.subtitle = element_text(hjust = 1),
  plot.caption = element_text(hjust = 1),
  legend.text = element_text(size = 7),
  legend.title = element_text(size = 11)
)





# Heat Stress -------------------------------------------------------------

heat_stress <- grid_sf %>% dplyr::select(grid_id,upper_bound)
heat_stress_prj <- st_transform(heat_stress, crs = 4326)

# Select data
wbtemp <- heat_stress_prj %>%
  dplyr::select(upper_bound) %>% 
  filter(upper_bound > 10)


# Define a color scale for the 'clipped_wbtemp' layer
color_scale <- scale_fill_gradient(low = "white", high = "darkred", breaks = seq(10, 40, by = 2))

# Create the plot
wbtemp_plot <- ggplot() +
  geom_sf(data = wbtemp, aes(fill = upper_bound), color = NA) + 
  color_scale +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=country, aes(label = WB_ADM0_NA), size=2) +
  coord_sf() +
  theme_void() +
  guides(fill = guide_colourbar(title = "Wet-bulb temperature", direction = "horizontal", barwidth = 30, barheight = 0.5)) +
  theme(legend.position = "bottom")

wbtemp_plot
# Export
ggsave(filename = file.path(maps, "wbgt.png"), plot=wbtemp_plot, height = 4, width = 8, units = c("in"))










# Drought -----------------------------------------------------------------

drought <- grid_sf %>% dplyr::select(grid_id, discrete_drought_freq)
drought_prj <- st_transform(drought, crs = 4326)
drought_prj$discrete_drought_freq <- as.factor(drought_prj$discrete_drought_freq)

# Create the RdYlGn palette for as many levels as you have in discrete_drought_freq
n_levels <- length(unique(drought_prj$discrete_drought_freq))
colors <- rev(colorRampPalette(RColorBrewer::brewer.pal(min(11, n_levels), "RdYlGn"))(n_levels))

# Define a color scale for the 'clipped_wbtemp' layer
color_scale <- scale_fill_manual(values = colors, na.value = "grey50", name = "Frequency (%)") 

# Create the plot
drought_plot <- ggplot() +
  geom_sf(data = drought_prj, aes(fill = discrete_drought_freq), size = 0.0001, col = NA) + 
  color_scale +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=country, aes(label = WB_ADM0_NA), size=2) +
  coord_sf() +
  theme_void() +
  theme(legend.position = "bottom")

#Export
ggsave(filename = file.path(maps, "drought.png"), height = 4, width = 8, units = c("in"))






# Air Pollution -----------------------------------------------------------

air_pol <- grid_sf %>% dplyr::select(grid_id, annual_mean2_pm25)
air_pol_prj <- st_transform(air_pol, crs = 4326)

# Define a color scale for the 'clipped_wbtemp' layer
color_scale <- scale_fill_gradient(low = "white", high = "Purple4", breaks = seq(0, 150, by = 5))

# Create the plot
poll_plot <- ggplot() +
  geom_sf(data = air_pol_prj, aes(fill = annual_mean2_pm25), size = 0.01, col = NA) + 
  color_scale +
  geom_sf(data = shp, color="black", alpha =0, size=0.0001) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=country, aes(label = WB_ADM0_NA), size=2) +
  coord_sf() +
  theme_void() +
  guides(fill = guide_colourbar(title = "PM2.5 concentration", direction = "horizontal", barwidth = 30, barheight = 0.5)) +
  theme(legend.position = "bottom")

#Export
ggsave(filename = file.path(maps, "air_pol.png"), height = 4, width = 8, units = c("in"))












# Flood -------------------------------------------------------------------

flood <- grid_sf %>% dplyr::select(grid_id,grid_flood_expected)
flood_prj <- st_transform(flood, crs = 4326)

summary(flood_prj$grid_flood_expected)
# Define a color scale for the 'clipped_wbtemp' layergrid_flood_expected
#breaks <- c(0,0.00000001,0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10)
breaks <- c(0,0.00000001,0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2)
labels <- c("0", "<0.1", "<0.2", "<0.3", "<0.4","<0.5", "<0.6", "<0.7", "<0.8","<0.9", "<1", "<1.1", "<1.2","<1.3", "<1.4", "<1.5", "<1.6","<1.7", "<1.8", "<1.9", "<2")
colors <- c("white", colorRampPalette(c("#9898BF", "Midnight Blue"))(length(breaks) - 2))
flood_prj$flood_expected <- cut(flood_prj$grid_flood_expected, 
                                breaks = breaks, 
                                labels = labels, 
                                include.lowest = TRUE, 
                                right = FALSE)
# Create the plot
flood_plot <- ggplot() +
  geom_sf(data = flood_prj, aes(fill = flood_expected), size = 0.00001, col = NA) + 
  scale_fill_manual(values = colors, 
                    na.value="grey50",
                    breaks = labels,
                    name = "Flood water depth (m)")+
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=country, aes(label = WB_ADM0_NA), size=2) +
  coord_sf() +
  theme_void() +
  theme(legend.position = "bottom")

#Export
ggsave(filename = file.path(maps, "flood.png"), height = 4, width = 8, units = c("in"))
