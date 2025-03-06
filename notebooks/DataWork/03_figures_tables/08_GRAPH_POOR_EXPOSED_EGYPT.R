# ================================================================
# Script Name: 07_GRAPH_POOR_EXPOSED_EGYPT
# Purpose: Create maps of the exposed poor for Egypt
# Input Dataset:grid_10km_poor_215_685.Rds,  MENA_ADM2.shp, MENA_Country.shp
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================


# Load Data ---------------------------------------------------------------

grid_poor <- readRDS(file.path(final_replication, "grid_10km_poor_215_685.Rds"))




# Compute the exposure estimates for Egypt --------------------------------

# Subset ----------------------------------------------------------

poor_exposed <- grid_poor %>%
  dplyr::select(ISO_A2,grid_id,WB_ADM1_NA, WB_ADM2_NA,starts_with("exposed"), grid_pop_flood_expected,pop_count,starts_with("poor")) %>%
  filter(ISO_A2 %in% c("EG"))



# Compute exposure (3.65 & 6.85) ------------------------------------------

poor_exposed <- poor_exposed %>%
  dplyr::mutate(exposed_heat32_poor365_rwi = exposed_heat32*poor365_count_rwi_dup,
                exposed_heat32_poor685_rwi = exposed_heat32*poor685_count_rwi_dup,
                
                exposed_drought20_poor365_rwi = exposed_drought20*poor365_count_rwi_dup,
                exposed_drought20_poor685_rwi = exposed_drought20*poor685_count_rwi_dup,
                
                exposed_airpol15_poor365_rwi = exposed_airpol15*poor365_count_rwi_dup,
                exposed_airpol15_poor685_rwi = exposed_airpol15*poor685_count_rwi_dup,
                
                exposed_flood5_poor365_rwi = grid_pop_flood_expected*poor365_rwi_dup,
                exposed_flood5_poor685_rwi = grid_pop_flood_expected*poor685_rwi_dup) %>%
  
  dplyr::select(grid_id,WB_ADM1_NA, WB_ADM2_NA,
                exposed_heat32_poor685_rwi,exposed_drought20_poor685_rwi,exposed_airpol15_poor685_rwi,exposed_flood5_poor685_rwi,geometry)




# Create Map --------------------------------------------------------------


# EGY shapefiles
shp <- sf::st_read(file.path(raw_replication,"BOUNDARIES", "MENA_ADM2.shp")) %>% dplyr::filter(ISO_A2 == "EG")
country <- sf::st_read(file.path(raw_replication, "BOUNDARIES", "MENA_Country.shp")) %>% dplyr::filter(ISO_A2 == "EG")

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

# Define a function to process and plot data
plot_exposure <- function(data, variable_name, color_palette, legend_title, breaks, labels) {
  # Transform to the desired projection
  data_prj <- st_transform(data, 4326)
  
  # Summary of the variable
  print(summary(data_prj[[variable_name]]))
  
  # Discretize the variable
  colors <- colorRampPalette(color_palette)(length(breaks) - 1)
  data_prj[[paste(variable_name, "discrete", sep = "_")]] <- cut(data_prj[[variable_name]], 
                                                                 breaks = breaks, 
                                                                 labels = labels, 
                                                                 include.lowest = TRUE, 
                                                                 right = FALSE)
  
  # Generate the plot
  p <- ggplot() +
    geom_sf(data = data_prj, aes(fill = get(paste(variable_name, "discrete", sep = "_"))), size = 0.01, color = NA) +
    geom_sf(data = shp, color = "black", alpha = 0, size = 0.1) +
    geom_sf(data = country, color = "black", alpha = 0, size = 0.3) +
    geom_sf_text(data = shp, aes(label = WB_ADM1_NA), size = 2) +
    theme_classic() +
    common_theme +
    scale_fill_manual(values = colors, 
                      na.value = "grey50",
                      breaks = labels,
                      name = legend_title) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_text(hjust = 0.5)  # Center the legend title
    )
  
  print(p)
}

# Usage example:
# Breaks and labels common to multiple plots
common_breaks <- c(-1, 0, 1, 500, 1000, 5000, 10000, 50000, 100000, 500000, Inf)
common_labels <- c("0", "0-1", "1-500", "500-1000", "1000-5000", "5000-10000", 
                   "10000-50000", "50000-100000", "100000-500000", "500000+")

# Plot for heat exposure
heat_plot <- plot_exposure(poor_exposed, "exposed_heat32_poor685_rwi", 
                           c("white", "darkred"),
                           "# Poor ($6.85) exposed to extreme heat stress\n[Extreme Heat Stress Index >32°C]", 
                           common_breaks, common_labels)

drought_plot <- plot_exposure(poor_exposed, "exposed_drought20_poor685_rwi", c("white", "Orange Red 4"),
                              "# of Poor living near their cropland and exposed to severe drought\n[ Severe drought occurrence >20% of the time]", common_breaks, common_labels)

airpol_plot <- plot_exposure(poor_exposed, "exposed_airpol15_poor685_rwi", c("white", "Purple4"),
                             "# of Poor ($6.85) exposed to PM2.5\n[PM2.5 >15ug/m3]", common_breaks, common_labels)

flood_plot <- plot_exposure(poor_exposed, "exposed_flood5_poor685_rwi", c("white", "Midnight Blue"),
                            "# of Poor($6.85) exposed to Flood Risk\n[Water Depth >0.5m]", c(-1,0, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000, Inf), 
                            c("0", "0-1", "1-5", "5-10", "10-50", "50-100", "100-500", "500-1000", "1000-5000", "5000-10000", "10000-50000", "50000+"))





# Create grid
ggpubr::ggarrange(
  heat_plot, drought_plot, airpol_plot, flood_plot, # list of plots
  labels = "AUTO", # labels
  #common.legend = TRUE, # COMMON LEGEND
  #legend = "bottom", # legend position
  align = "hv", # Align them both, horizontal and vertical
  nrow = 2, # number of rows
  ncol = 2 # number of columns
)

ggsave(file.path(graphs,"exposed_poor_allhazards_egy.png"), width = 14.5, height = 14.5)
