# ================================================================
# Script Name: 11_GRAPH_POOR_EXPOSED_VULNERABLE_MOR_YEM
# Purpose: Create map of exposed, poor and vulnerable
# Input Dataset: ,  MENA_ADM2.shp, MENA_Country.shp
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================




options(scipen = 999)
# Load Data ---------------------------------------------------------------

poor_vul_exposed <- readRDS(file.path(final_replication,"grid_10km_poor_vul_exposed.Rds"))

# MENA shapefiles
shp <- st_read(file.path(raw_replication, "BOUNDARIES", "MENA_ADM2.shp"))
country <- st_read(file.path(raw_replication, "BOUNDARIES", "MENA_Country.shp"))
shp1 <- st_read(file.path(raw_replication,"BOUNDARIES", "MENA_ADM1.shp"))

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






poor_vul_exposed_pop <- poor_vul_exposed %>%
  dplyr::select(grid_id,WB_ADM1_NA, WB_ADM0_NA, ISO_A2, poor215_vul042_exposed,poor365_vul042_exposed,
                poor685_vul042_exposed,pop_count) %>%
  dplyr::mutate(poor215_vul042_exposed_pop = poor215_vul042_exposed*pop_count,
                poor365_vul042_exposed_pop = poor365_vul042_exposed*pop_count,
                poor685_vul042_exposed_pop = poor685_vul042_exposed*pop_count
  )


# List of countries where you want to set poor365_vul_exposed_pop_freq to NA
countries <- c("Libya", "Oman", "Saudi Arabia","Syria", "Qatar", "Syrian Arab Republic")

# Update the poor_vul_exposed_pop_prj dataset to set poor365_vul_exposed_pop_freq to NA for specified countries
poor_vul_exposed_pop <- poor_vul_exposed_pop %>%
  mutate(poor365_vul042_exposed_pop = ifelse(WB_ADM0_NA %in% countries, NA, poor365_vul042_exposed_pop))



poor_vul_exposed_pop_prj <- st_transform(poor_vul_exposed_pop,4326)





# Map of Morocco and Yemen
poor_vul_exposed_pop_MA_YE <- poor_vul_exposed_pop_prj %>%
  dplyr::filter(ISO_A2 == "MA" | ISO_A2 == "YE")

# test <- poor_vul_exposed_pop_MA_YE %>%
#   group_by(WB_ADM1_NA, ISO_A2) %>%
#   dplyr::summarise(
#     summ_poor_vul_exposed = sum(poor365_vul042_exposed_pop)
#   ) %>%
#   arrange(desc(summ_poor_vul_exposed))
# 
# test2 <- poor_vul %>%
#   filter(WB_ADM1_NA == "Nord Ouest") %>%
#   group_by(WB_ADM1_NA) %>%
#   summarise(
#     agg_yield_ratio_thresh = sum(agg_yield_ratio_thresh, na.rm = T),
#     forest_loss_thresh = sum(forest_loss_thresh, na.rm = TRUE),
#     ntl_nogf_thresh = sum(ntl_nogf_thresh, na.rm = TRUE),
#     ntl_gf_thresh = sum(ntl_gf_thresh, na.rm = TRUE),
#     conflict_thresh = sum(conflict_thresh, na.rm = TRUE),
#     tt_health_thresh = sum(tt_health_thresh, na.rm = TRUE),
#     tt_educ_thresh = sum(tt_educ_thresh, na.rm = TRUE),
#     tt_markets_thresh = sum(tt_markets_thresh, na.rm = TRUE),
#     upper_bound = sum(ifelse(upper_bound >=32,1,0),na.rm = T),
#     annual_mean2_pm25 = sum(ifelse(annual_mean2_pm25>=15,1,0), na.rm = TRUE),
#     drought_freq = sum(ifelse(drought_freq>25,1,0), na.rm = TRUE),
#     grid_pop_flood_expected = sum(ifelse(grid_pop_flood_expected >0, 1,0), na.rm = TRUE)
#   )
# 
# 
# names(poor_vul)
# Map (MA & YE) -----------------------------------------------------------

poor_vul_exposed_pop_MA_YE_prj <- st_transform(poor_vul_exposed_pop_MA_YE,4326)
summary(poor_vul_exposed_pop_MA_YE_prj$poor365_vul042_exposed_pop)


# Defining categorical data
poor_vul_exposed_pop_MA_YE_prj$poor365_vul042_exposed_pop_freq <-
  cut(
    poor_vul_exposed_pop_MA_YE_prj$poor365_vul042_exposed_pop,
    breaks = c(0,1,10,100,1000,10000,100000,1000000),
    labels = c("0","1-10","10-100","100-1000","1000-10000","10000-100000","100000 - 1000000"),
    include.lowest = TRUE,
    right = FALSE
  )

labels = c("0","1-10","10-100","100-1000","1000-10000","10000-100000","100000 - 1000000")
# Adjusting the number of colors to match the number of bins
custom_colors <- setNames(rev(sequential_hcl(8, palette = "RdPu")), labels)


p1 <- ggplot() +
  geom_sf(data = poor_vul_exposed_pop_MA_YE_prj %>% filter(ISO_A2 == "YE"), aes(fill = poor365_vul042_exposed_pop_freq), color = NA) +
  geom_sf(data = shp1 %>% filter(ISO_A2 == "YE"), color = "grey27", alpha = 0, size = 0.01) +
  #geom_sf(data = country, color = "grey", size = 0.3) +  # Outline of countries (commented out)
  geom_sf_text(data = shp1 %>% filter(ISO_A2 == "YE"), aes(label = WB_ADM1_NA), size = 4) +  # Country labels
  scale_fill_manual(
    values = custom_colors,
    guide = guide_legend(title = "# of Poor ($3.65), Exposed and Vulnerable [share > 0.42]")
  ) +
  labs(fill = "# of Poor ($3.65), Exposed (Any Hazard) and Vulnerable [share > 0.42]", title = "Yemen") +
  theme_void() +  # Removes background, grid lines, and axes
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  guides(fill = guide_legend(
    order = 0,
    direction = "horizontal",
    title.position = "top",
    label.position = "bottom",
    nrow = 1,
    keywidth = 1,
    keyheight = 0.5,
    override.aes = list(color = "black")
  )) 


p2 <- ggplot() +
  geom_sf(data = poor_vul_exposed_pop_MA_YE_prj %>% filter(ISO_A2 == "MA"), aes(fill = poor365_vul042_exposed_pop_freq), color = NA) +
  geom_sf(data = shp1 %>% filter(ISO_A2 == "MA"), color = "grey27", alpha = 0, size = 0.01) +
  #geom_sf(data = country, color = "grey", size = 0.3) +  # Outline of countries (commented out)
  geom_sf_text(data = shp1 %>% filter(ISO_A2 == "MA"), aes(label = WB_ADM1_NA), size = 4) +  # Country labels
  scale_fill_manual(
    values = custom_colors,
    guide = guide_legend(title = "# of Poor ($3.65), Exposed and Vulnerable [share > 0.42]")
  ) +
  labs(fill = "# of Poor ($3.65), Exposed (Any Hazard) and Vulnerable [share > 0.42]",
       title = "Morocco") +
  theme_void() +  # Removes background, grid lines, and axes
  theme(
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  guides(fill = guide_legend(
    order = 0,
    direction = "horizontal",
    title.position = "top",
    label.position = "bottom",
    nrow = 1,
    keywidth = 1,
    keyheight = 0.5,
    override.aes = list(color = "black")
  ))  # Assuming 'common_theme' is defined elsewhere in your script


ggpubr::ggarrange(
  p1,p2, # list of plots
  labels = "AUTO", # labels
  common.legend = TRUE, # COMMON LEGEND
  legend = "bottom", # legend position
  align = "hv", # Align them both, horizontal and vertical
  nrow = 1, # number of rows
  ncol = 2 # number of columns
)

ggsave(file.path(maps,"poor_vul042_exposed_MA_YE.png"), width = 8, height = 4)

