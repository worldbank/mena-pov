## YIF Proposal Maps


#individual hazard grids
pop <- readRDS(file.path(mena_file_path,"Population","final","grid_pop_10km_v2.Rds")) %>% dplyr::filter(WB_ADM0_NA == "Arab Republic of Egypt")
rwi <- readRDS(file.path(mena_file_path,"RWI","final","grid_rwi_10km.Rds")) %>% dplyr::filter(WB_ADM0_NA == "Arab Republic of Egypt")
heat_stress <- readRDS(file.path(mena_file_path,"Hazards","TEMP","final","grid_wbgt_10km_v2.Rds")) %>% dplyr::filter(WB_ADM0_NA == "Arab Republic of Egypt")
flood <- readRDS(file.path(mena_file_path,"Hazards","FLOOD","final","grid_flood_10km_resampled_allrps_120823.Rds")) %>% dplyr::filter(WB_ADM0_NA == "Arab Republic of Egypt")

air_pol <- readRDS(file.path(mena_file_path,"Hazards","PM2_5","final","grid_PM25_10km_v2.Rds")) %>% dplyr::filter(WB_ADM0_NA == "Arab Republic of Egypt")
drought<- readRDS(file.path(mena_file_path,"Hazards","ASI","final","grid_drought_10km.Rds")) %>% dplyr::filter(WB_ADM0_NA == "Arab Republic of Egypt")

# combined grid data
grid_sf <- readRDS(file.path(mena_file_path,"Allsources","grid_10km_all_v2_resampled.Rds")) %>% dplyr::filter(WB_ADM0_NA == "Arab Republic of Egypt")
grid_gsap_rwi <- readRDS(file.path(final,"grid_10km_poor_final_resampled.Rds")) %>% dplyr::filter(WB_ADM0_NA == "Arab Republic of Egypt")
grid_population <- readRDS(file.path(final,"grid_10km_people_final_resampled.Rds")) %>% dplyr::filter(WB_ADM0_NA == "Arab Republic of Egypt")



# MENA shapefiles
shp <- st_read(file.path(mena_file_path,"Boundaries","raw","MENA_ADM2.shp")) %>% dplyr::filter(WB_ADM0_NA == "Arab Republic of Egypt")
country <- st_read(file.path(mena_file_path, "Boundaries", "raw", "MENA_Country.shp")) %>% dplyr::filter(WB_ADM0_NA == "Arab Republic of Egypt")

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

maps <- "C:/Users/wb569257/OneDrive - WBG/Y2Y Innovation Fund/Maps"

# heat
heat_stress_prj <- st_transform(heat_stress, crs = 4326)

# Select data
wbtemp <- heat_stress_prj %>%
  dplyr::select(upper_bound) %>% 
  filter(upper_bound > 10)

# Define a color scale for the 'clipped_wbtemp' layer
color_scale <- scale_fill_gradient(low = "white", high = "darkred", breaks = seq(10, 40, by = 2))

# Create the plot
wbtemp_plot <- ggplot() +
  geom_sf(data = wbtemp, aes(fill = upper_bound), size = 0.01) + 
  color_scale +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=shp, aes(label = WB_ADM1_NA), size=2) +
  coord_sf() +
  theme_void() +
  guides(fill = guide_colourbar(title = "Wet-bulb temperature", direction = "horizontal", barwidth = 30, barheight = 0.5)) +
  theme(legend.position = "bottom")
ggsave(filename = file.path(maps, "wbgt.png"), plot=wbtemp_plot, height = 12, width = 16, units = c("in"))


#drought

drought_prj <- st_transform(drought, crs = 4326)
drought_prj$discrete_drought_freq <- as.factor(drought_prj$discrete_drought_freq)

# Create the RdYlGn palette for as many levels as you have in discrete_drought_freq
n_levels <- length(unique(drought_prj$discrete_drought_freq))
colors <- rev(colorRampPalette(RColorBrewer::brewer.pal(min(11, n_levels), "RdYlGn"))(n_levels))

# Define a color scale for the 'clipped_wbtemp' layer
color_scale <- scale_fill_manual(values = colors, na.value = "grey50", name = "Frequency (%)") 

# Create the plot
drought_plot <- ggplot() +
  geom_sf(data = drought_prj, aes(fill = discrete_drought_freq), size = 0.01) + 
  color_scale +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=shp, aes(label = WB_ADM1_NA), size=2) +
  coord_sf() +
  theme_void() +
  theme(legend.position = "bottom")
ggsave(filename = file.path(maps, "drought.png"), height = 12, width = 16, units = c("in"))

#flood

#keep only the flood variables and grid id
flood_sub <- flood %>% dplyr::select(grid_id,
                                     grid_flood_expected,geometry)
flood_sub <- st_as_sf(flood_sub)

flood_prj <- st_transform(flood_sub, crs = 4326)

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
  geom_sf(data = flood_prj, aes(fill = flood_expected), size = 0.00001, col ="white") + 
  scale_fill_manual(values = colors, 
                    na.value="grey50",
                    breaks = labels,
                    name = "Flood water depth (m)")+
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=shp, aes(label = WB_ADM1_NA), size=2) +
  coord_sf() +
  theme_void() +
  theme(legend.position = "bottom")
ggsave(filename = file.path(maps, "flood.png"), height = 12, width = 16, units = c("in"))

# Pop Density
pop_proj <- st_transform(pop, crs = 4326)
pop_dens <- pop_proj %>% 
  dplyr::select(pop_density)
# plot(pop_dens)
# Define a color scale for the 'clipped_wbtemp' layer
# Chose the breaks
summary(pop_dens$pop_density)
#deciles <- quantile(pop_dens$pop_density, probs = seq(0.1, 0.9, by = 0.1))
#print(deciles)
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
  geom_sf(data = pop_brks, aes(fill = brks), size = 0.01) + 
  color_scale +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=shp, aes(label = WB_ADM1_NA), size=2) +
  coord_sf() +
  theme_void() +
  guides(fill = guide_legend(title = "Population density (per km2)", title.position = "top"))
theme(legend.position = "bottom")
ggsave(filename = file.path(maps, "\\pop_density_2020.png"), plot=popdens_plot, height = 12, width = 16, units = c("in"))



#Exposed People to Heat Stress
grid_final_10km_prj <- st_transform(grid_population, 4326)
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
  geom_sf(data = grid_final_10km_prj, aes(fill = exposed_heat32_pop_discrete), size = 0.01) +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=shp, aes(label = WB_ADM1_NA), size=2) +
  theme_classic() +
  common_theme +
  scale_fill_manual(values = colors, 
                    na.value="grey50",
                    breaks = labels,
                    name = "# People exposed to extreme heat (Wet-bulb temperature >32 at least 3 days a year)") +
  theme(legend.position = "bottom")
ggsave(filename = file.path(maps, "exposed_people_heat.png"), height = 12, width = 16, units = c("in"))


#Exposed People to Drought
grid_final_10km_prj <- st_transform(grid_population, 4326)
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
  geom_sf(data = grid_final_10km_prj, aes(fill = exposed_drought20_pop_discrete), size = 0.01) +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=shp, aes(label = WB_ADM1_NA), size=2) +
  theme_classic() +
  common_theme +
  scale_fill_manual(values = colors, 
                    na.value="grey50",
                    breaks = labels,
                    name = "# People living in cropland and exposed to severe drought more than 20% of the years") +
  theme(legend.position = "bottom")
ggsave(filename = file.path(maps, "exposed_people_drought.png"), height = 12, width = 16, units = c("in"))


#Exposed Poor to Drought
grid_final_10km_prj <- st_transform(grid_gsap_rwi, 4326)
summary(grid_final_10km_prj$exposed_drought20_poor215_rwi)
breaks <- c(-1, 0, 1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 500, 1000,5000,10000, 50000)
labels <- c("0-1", "1-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "100-500","500-1000", "1000-5000", "5000-10000", "10000-50000", "50000+")
colors <- colorRampPalette(c("white", "Orange Red 4"))(length(breaks) - 1)
grid_final_10km_prj$exposed_drought20_poor215_rwi_discrete <- cut(grid_final_10km_prj$exposed_drought20_poor215_rwi, 
                                                                  breaks = breaks, 
                                                                  labels = labels, 
                                                                  include.lowest = TRUE, 
                                                                  right = FALSE)
ggplot() +
  geom_sf(data = grid_final_10km_prj, aes(fill = exposed_drought20_poor215_rwi_discrete), size = 0.01) +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=shp, aes(label = WB_ADM1_NA), size=2) +
  theme_classic() +
  common_theme +
  scale_fill_manual(values = colors, 
                    na.value="grey50",
                    breaks = labels,
                    name = "# Poor people living in cropland and exposed to severe drought more than 20% of the years") +
  theme(legend.position = "bottom")
ggsave(filename = file.path(maps, "exposed_poor215_drought.png"), height = 12, width = 16, units = c("in"))

sum(grid_final_10km_prj$exposed_drought20_pop, na.rm = T)
