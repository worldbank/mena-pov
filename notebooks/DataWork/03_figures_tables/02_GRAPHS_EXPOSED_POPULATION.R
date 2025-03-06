# ================================================================
# Script Name: 02_GRAPH_EXPOSED_POPULATIONS
# Purpose: Create maps with total population exposed to each hazard
# Input Dataset: merged_dataset_10km.Rds,MENA_ADM2.shp, MENA_Country.shp
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================

options(scipen =999)



# Load grid ---------------------------------------------------------------

grid_sf <- readRDS(file.path(final_replication,"grid_10km_people.Rds"))

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






# Population --------------------------------------------------------------
pop <- grid_sf %>% dplyr::select(grid_id, pop_density)


pop_proj <- st_transform(pop, crs = 4326)
pop_dens <- pop_proj %>% 
  dplyr::select(pop_density)

# I introduce -1 to capture the cases in which pop=0
breaks <- c(-1,0,1,2,3,4,5,6,7,8,9,10,20,30,40,50, 1000, 5000, 10000, 20000, 30000, 40000)
labels <- c("0","0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-20","20-30","30-40",
            "40-50","50-1000", "1000-5000", "5000-10000", "10000-20000", "20000-30000", "30000+")
pop_dens$brks <-cut(pop_dens$pop_density,
                    breaks=breaks,
                    labels=labels)
pop_brks<-pop_dens <- pop_dens %>% 
  dplyr::select(brks) 
# sequential_hcl(21, palette = "Blues 3")
# 
custom_colors <- c(
  "0" = "#F9F9F9",
  "0-1" = "#F9F9F9",
  "1-2" = "#F0F6FF",
  "2-3" = "#E6F1FF",
  "3-4" = "#DCEBFF",
  "4-5" = "#D0E4FF",
  "5-6" = "#C3DBFD",
  "6-7" = "#B6D3F9",
  "7-8" = "#A8C9F4",
  "8-9" = "#99BFEF",
  "9-10" = "#8AB5E9",
  "10-20" = "#79ABE2",
  "20-30" = "#67A0DB",
  "30-40" = "#5295D4",
  "40-50" = "#388ACC",
  "50-1000" = "#037EC4",
  "1000-5000" = "#0072B4",
  "5000-10000" = "#0066A5",
  "10000-20000" = "#005995",
  "20000-30000" = "#004D86",
  "30000+ " = "#004178",
  "30000+" = "#00366C"
)

color_scale <- scale_fill_manual(values=custom_colors,
                                 na.value="grey50",
                                 aesthetics="fill")
# Create the plot
popdens_plot <- ggplot() +
  geom_sf(data = pop_brks, aes(fill = brks), size = 0.01, col = NA) + 
  color_scale +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=country, aes(label = WB_ADM0_NA), size=2) +
  coord_sf() +
  theme_void() +
  guides(fill = guide_legend(title = "Population density (per km2)", title.position = "top")) +
theme(legend.position = "bottom")

#Export
ggsave(filename = file.path(maps, "pop_density_2020.png"), plot=popdens_plot, height = 4, width = 6, units = c("in"))








# Heat Stress -------------------------------------------------------------

# Exposed People to Heat stress
grid_final_10km_prj <- st_transform(grid_sf, 4326)
summary(grid_final_10km_prj$exposed_heat32_pop)
breaks <- c(0, 1, 10, 100,1000,5000,10000,100000,1000000,5000000)
labels <- c("0-1", "1 - 10", "10 - 100", "100 - 1000", "1000-5000", "5000-10000", "10000 - 100000", "100000 - 1000000", "1000000+")
colors <- colorRampPalette(c("white", "darkred"))(length(breaks) - 1)
grid_final_10km_prj$exposed_heat32_pop_discrete <- cut(grid_final_10km_prj$exposed_heat32_pop, 
                                                       breaks = breaks, 
                                                       labels = labels, 
                                                       include.lowest = TRUE, 
                                                       right = FALSE)



ggplot() +
  geom_sf(data = grid_final_10km_prj, aes(fill = exposed_heat32_pop_discrete), size = 0.01, col = "white") +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=country, aes(label = WB_ADM0_NA), size=2) +
  theme_classic() +
  common_theme +
  scale_fill_manual(values = colors, 
                    na.value="grey50",
                    breaks = labels,
                    name = "# People exposed to extreme heat (Wet-bulb temperature >32 at least 3 days a year)") +
  theme(legend.position = "bottom")


ggsave(filename = file.path(maps, "exposed_people_heat.png"), height = 12, width = 16, units = c("in"))









# Drought -----------------------------------------------------------------

# Exposed People to Drought
grid_final_10km_prj <- st_transform(grid_sf ,4326)
summary(grid_final_10km_prj$exposed_drought20_pop)
breaks <- c(0, 1, 10, 100,1000,5000,10000,100000,1000000,5000000)
labels <- c("0-1", "1 - 10", "10 - 100", "100 - 1000", "1000-5000", "5000-10000", "10000 - 100000", "100000 - 1000000", "1000000+")
colors <- colorRampPalette(c("white", "Orange Red 4"))(length(breaks) - 1)
grid_final_10km_prj$exposed_drought20_pop_discrete <- cut(grid_final_10km_prj$exposed_drought20_pop, 
                                                          breaks = breaks, 
                                                          labels = labels, 
                                                          include.lowest = TRUE, 
                                                          right = FALSE)



ggplot() +
  geom_sf(data = grid_final_10km_prj, aes(fill = exposed_drought20_pop_discrete), size = 0.01, col = NA) +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=country, aes(label = WB_ADM0_NA), size=2) +
  theme_classic() +
  common_theme +
  scale_fill_manual(values = colors, 
                    na.value="grey50",
                    breaks = labels,
                    name = "# People living in cropland and exposed to severe drought more than 20% of the years") +
  theme(legend.position = "bottom")

ggsave(filename = file.path(maps, "exposed_people_drought.png"), height = 12, width = 16, units = c("in"))











# Air Pollution -----------------------------------------------------------

# Exposed People to Air Pollution
grid_final_10km_prj <- st_transform(grid_sf, 4326)
summary(grid_final_10km_prj$exposed_airpol15_pop)
breaks <- c(0, 1, 10, 100,1000,5000,10000,100000,1000000,5000000)
labels <- c("0-1", "1 - 10", "10 - 100", "100 - 1000", "1000-5000", "5000-10000", "10000 - 100000", "100000 - 1000000", "1000000+")
colors <- colorRampPalette(c("white", "Purple4"))(length(breaks) - 1)
grid_final_10km_prj$exposed_airpol15_pop_discrete <- cut(grid_final_10km_prj$exposed_airpol15_pop, 
                                                         breaks = breaks, 
                                                         labels = labels, 
                                                         include.lowest = TRUE, 
                                                         right = FALSE)


ggplot() +
  geom_sf(data = grid_final_10km_prj, aes(fill = exposed_airpol15_pop_discrete), size = 0.01, col = NA) +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=country, aes(label = WB_ADM0_NA), size=2) +
  theme_classic() +
  common_theme +
  scale_fill_manual(values = colors, 
                    na.value="grey50",
                    breaks = labels,
                    name = "# Exposed People to PM2.5 > 15ug/m3") +
  theme(legend.position = "bottom")

ggsave(filename = file.path(maps, "exposed_people_air_pol.png"), height = 4, width = 8, units = c("in"))










# Floods ------------------------------------------------------------------
# Exposed People to Floods
grid_final_10km_prj <- st_transform(grid_sf, 4326)

# Exposed People to Floods
summary(grid_final_10km_prj$grid_pop_flood_expected)

breaks <- c(0, 1,5, 10,50,100,1000,5000,10000,50000,100000,500000)
labels <- c("0", "1 - 5","5 - 10","10 - 50","50 - 100","100 - 1000", "1000-5000", "5000-10000", "10000 - 50000", "50000 - 100000", "100000+")
colors <- colorRampPalette(c("white", "Midnight Blue"))(length(breaks) - 1)
grid_final_10km_prj$exposed_flood5_pop_discrete <- cut(grid_final_10km_prj$grid_pop_flood_expected, 
                                                       breaks = breaks, 
                                                       labels = labels, 
                                                       include.lowest = TRUE, 
                                                       right = FALSE)



ggplot() +
  geom_sf(data = grid_final_10km_prj, aes(fill = exposed_flood5_pop_discrete), size = 0.0001, col = "white") +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=country, aes(label = WB_ADM0_NA), size=2) +
  theme_classic() +
  common_theme +
  scale_fill_manual(values = colors, 
                    na.value="grey50",
                    breaks = labels,
                    name = "# Exposed People to Flood > 0.5m") +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank())

ggsave(filename = file.path(maps, "exposed_people_flood.png"), height = 12, width = 16, units = c("in"))

