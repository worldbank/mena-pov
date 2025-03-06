# ================================================================
# Script Name: 03_GRAPH_EXPOSED_POOR_215
# Purpose: Create maps with extreme poor (2.15) population exposed to each hazard
# Input Dataset: gsap_grid, MENA_ADM2.shp, MENA_Country.shp
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================

options(scipen =999)






# Load grid ---------------------------------------------------------------
grid_gsap_rwi <- readRDS(file.path(final_replication,"grid_10km_poor_215_685.Rds"))
grid_population <- readRDS(file.path(final_replication,"grid_10km_people.Rds"))

# MENA shapefiles
shp <- st_read(file.path(raw_replication, "BOUNDARIES", "MENA_ADM2.shp"))
country <- st_read(file.path(raw_replication, "BOUNDARIES", "MENA_Country.shp"))



# Define common ggplot theme
common_theme <- ggplot2::theme(
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









# Number and Share of Poor ------------------------------------------------

#Number of Poor (2.15)
gsap_rwi_grid_prj <- st_transform(grid_gsap_rwi,4326)

# summary(gsap_grid_merged$poor215_count_rwi)
breaks <- c(-1,0,0.5,1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80, 90,100,500, 1000, 5000, 10000, 20000, 30000, 40000)
labels <- c("0","0-1", "1-2", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10",
            "10-20", "20-30", "30-40", "40-50",  "50-60", "60-70", "70-80","80-90","90-100",
            "100-500", "500-1000", "1000-5000", "5000,10000", "10000-20000", "20000-30000", "30000+")

gsap_rwi_grid_prj$brks215 <-cut(gsap_rwi_grid_prj$poor215_count_rwi_dup,breaks=breaks,labels=labels)
custom_colors <- setNames(rev(sequential_hcl(28, palette = "Reds 2")), labels)
ggplot() +
  geom_sf(data = gsap_rwi_grid_prj, aes(fill = brks215), size = 0.01, col = NA) +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=country, aes(label = WB_ADM0_NA), size=2) +
  theme_classic() +
  scale_fill_manual(values=custom_colors,na.value="grey50",aesthetics="fill", 
                    name = "Number of poor (100km2 and $2.15)") +
  coord_sf() +
  theme_classic() +
  common_theme +
  guides(fill = guide_legend(direction = "horizontal", title.position = "top", label.position = "bottom", nrow = 1, keywidth = 1, keyheight = 0.5))+
  theme(legend.position = "bottom")

ggsave(filename = file.path(maps, "number_poor_215.png"), height = 4, width = 10, units = c("in"))

names(gsap_rwi_grid_prj)

#Share of Poor (2.15)
gsap_rwi_grid_prj$poor215_rwi_pct<-gsap_rwi_grid_prj$poor215_rwi_dup*100
summary(gsap_rwi_grid_prj$poor215_rwi_pct)

# Create the plot
ggplot() +
  geom_sf(data = gsap_rwi_grid_prj, aes(fill = poor215_rwi_pct), size = 0.01, col = NA) +
  scale_fill_gradient2(low ="white", mid="#7F000D", high = "#3C2692", midpoint=40, breaks = seq(0, 90, by =5),
                       na.value="grey50", name ="Share of poor ($2.15)") +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=country, aes(label = WB_ADM0_NA), size=2) +
  coord_sf() +
  theme_classic() +
  common_theme +
  guides(fill = guide_legend(direction = "horizontal", title.position = "top", label.position = "bottom", nrow = 1, keywidth = 1, keyheight = 0.5)) +
  theme(legend.position = "bottom")

ggsave(filename = file.path(maps, "share_poor_215.png"), height = 4, width = 10, units = c("in"))






maps
# Heat Stress -------------------------------------------------------------

# Exposed Poor to Heat stress
grid_final_10km_prj <- st_transform(grid_gsap_rwi, 4326)
summary(grid_final_10km_prj$exposed_heat32_poor215_rwi)
breaks <- c(-1, 0, 1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 500, 1000,5000,10000, 50000)
labels <- c("0-1", "1-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "100-500","500-1000", "1000-5000", "5000-10000", "10000-50000", "50000+")
colors <- colorRampPalette(c("white", "darkred"))(length(breaks) - 1)
grid_final_10km_prj$exposed_heat32_poor_discrete <- cut(grid_final_10km_prj$exposed_heat32_poor215_rwi, 
                                                        breaks = breaks, 
                                                        labels = labels, 
                                                        include.lowest = TRUE, 
                                                        right = FALSE)
ggplot() +
  geom_sf(data = grid_final_10km_prj, aes(fill = exposed_heat32_poor_discrete), size = 0.01, col = NA) +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=country, aes(label = WB_ADM0_NA), size=2) +
  theme_classic() +
  common_theme +
  scale_fill_manual(values = colors, 
                    na.value="grey50",
                    breaks = labels,
                    name = "# Poor ($2.15) exposed to extreme heat (Wet-bulb temperature >32 at least 3 days a year)") +
  theme(legend.position = "bottom")

#Export
ggsave(filename = file.path(maps, "exposed_poor215_heat.png"), height = 12, width = 16, units = c("in"))






# Drought -----------------------------------------------------------------
# Exposed Poor to Drought
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
  geom_sf(data = grid_final_10km_prj, aes(fill = exposed_drought20_poor215_rwi_discrete), size = 0.01, col = NA) +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=country, aes(label = WB_ADM0_NA), size=2) +
  theme_classic() +
  common_theme +
  scale_fill_manual(values = colors, 
                    na.value="grey50",
                    breaks = labels,
                    name = "# Poor people living in cropland and exposed to severe drought more than 20% of the years") +
  theme(legend.position = "bottom")

#Export
ggsave(filename = file.path(maps, "exposed_poor215_drought.png"), height = 12, width = 16, units = c("in"))



# Air Pollution -----------------------------------------------------------
# Exposed Poor people to Air Pollution
grid_final_10km_prj <- st_transform(grid_gsap_rwi, 4326)
summary(grid_final_10km_prj$exposed_airpol15_poor215_rwi)
breaks <- c(-1, 0, 1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 500, 1000,5000,10000, 50000)
labels <- c("0-1", "1-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "100-500","500-1000", "1000-5000", "5000-10000", "10000-50000", "50000+")
colors <- colorRampPalette(c("white", "Purple4"))(length(breaks) - 1)
grid_final_10km_prj$exposed_airpol15_poor215_discrete <- cut(grid_final_10km_prj$exposed_airpol15_poor215_rwi, 
                                                             breaks = breaks, 
                                                             labels = labels, 
                                                             include.lowest = TRUE, 
                                                             right = FALSE)
ggplot() +
  geom_sf(data = grid_final_10km_prj, aes(fill = exposed_airpol15_poor215_discrete), size = 0.01, col = NA) +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=country, aes(label = WB_ADM0_NA), size=2) +
  theme_classic() +
  common_theme +
  scale_fill_manual(values = colors, 
                    na.value="grey50",
                    breaks = labels,
                    name = "# Exposed Poor people ($2.15) to PM2.5 > 15ug/m3") +
  theme(legend.position = "bottom")

#Export
ggsave(filename = file.path(maps, "exposed_poor215_air_pol.png"), height = 12, width = 16, units = c("in"))




# Floods ------------------------------------------------------------------
# Exposed People to Floods
grid_final_10km_prj <- st_transform(grid_gsap_rwi, 4326)
summary(grid_final_10km_prj$exposed_flood5_poor215_rwi)
breaks <- c(-1,0.000001, 1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 500, 1000,5000,20000)
labels <- c("0","0-1", "1-5", "5-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100", "100-500","500-1000", "1000-5000", "5000+")
colors <- colorRampPalette(c("white", "Midnight Blue"))(length(breaks) - 1)
grid_final_10km_prj$exposed_flood5_poor215_rwi_dis <- cut(grid_final_10km_prj$exposed_flood5_poor215_rwi, 
                                                          breaks = breaks, 
                                                          labels = labels, 
                                                          include.lowest = TRUE, 
                                                          right = FALSE)
ggplot() +
  geom_sf(data = grid_final_10km_prj, aes(fill = exposed_flood5_poor215_rwi_dis), size = 0.001, col = NA) +
  geom_sf(data = shp, color="black", alpha =0, size=0.1) +
  geom_sf(data = country, color="black", alpha =0, size=0.3) +
  geom_sf_text(data=country, aes(label = WB_ADM0_NA), size=2) +
  theme_classic() +
  common_theme +
  scale_fill_manual(values = colors, 
                    na.value="grey50",
                    breaks = labels,
                    name = "# Exposed Poor people ($2.15) to to Flood > 0.5m") +
  theme(legend.position = "bottom")

#Export
ggsave(filename = file.path(maps, "exposed_poor215_flood.png"), height = 12, width = 16, units = c("in"))

