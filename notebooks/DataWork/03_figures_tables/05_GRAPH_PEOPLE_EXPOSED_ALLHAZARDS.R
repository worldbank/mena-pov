# ================================================================
# Script Name: 05_GRAPH_PEOPLE_EXPOSED_ALL_HAZARDS
# Purpose: Create table of number of people exposed to each hazard
# Input Dataset:grid_10km_poor_215_685.Rds,  MENA_ADM2.shp, MENA_Country.shp
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================



# Load Data ---------------------------------------------------------------

grid_poor <- readRDS(file.path(final_replication, "grid_10km_poor_215_685.Rds"))


grid_people <-readRDS(file.path(final_replication, "grid_10km_people.Rds"))





#############################################################################
############################## COUNTRY - LEVEL ##############################
#############################################################################

# Reshape ----------------------------------------------------------

people_exposed <- grid_people %>%
  dplyr::select(ISO_A2,starts_with("exposed"), grid_pop_flood_expected,pop_count)


people_exposed_summ <- people_exposed %>%
  group_by(ISO_A2) %>%
  dplyr::summarize(tot_exposed_heat = sum(exposed_heat32_pop, na.rm = T),
                   tot_exposed_drought = sum(exposed_drought20_pop, na.rm = T),
                   tot_exposed_airpol = sum(exposed_airpol15_pop, na.rm = T),
                   tot_exposed_flood = sum(grid_pop_flood_expected,na.rm = T),
                   tot_pop = sum(pop_count, na.rm = T)) %>% 
  st_drop_geometry()

#create share of population exposed var
people_exposed_summ <- people_exposed_summ %>%
  dplyr::mutate(share_exposed_heat = tot_exposed_heat/tot_pop,
                share_exposed_drought = tot_exposed_drought/tot_pop,
                share_exposed_airpol = tot_exposed_airpol/tot_pop,
                share_exposed_flood = tot_exposed_flood/tot_pop)



# Reshape the data to long format
data_long <- people_exposed_summ %>%
  gather(key = "hazard", value = "share_exposed", share_exposed_heat:share_exposed_flood) %>%
  mutate(hazard = case_when(
    hazard == "share_exposed_heat" ~ "Heat Exposure[Extreme Heat Stress >32°C]",
    hazard == "share_exposed_drought" ~ "Drought Exposure[Severe Drought Occurence >20%]",
    hazard == "share_exposed_airpol" ~ "Air Pollution Exposure[PM2.5 >15µg/m³]",
    hazard == "share_exposed_flood" ~ "Flood Risk[Water Depth >0.5m]"
  ))






# Plot --------------------------------------------------------------------


# Function to create individual plots
create_hazard_plot <- function(data, hazard_name, start_color, end_color) {
  # Determine the number of breaks for the gradient
  breaks <- seq(min(data$share_exposed, na.rm = TRUE), max(data$share_exposed, na.rm = TRUE), length.out = 100)
  # Generate the color palette
  colors <- colorRampPalette(c(start_color, end_color))(length(breaks) - 1)
  
  ggplot(filter(data, hazard == hazard_name), aes(y = reorder(ISO_A2, +share_exposed), x = share_exposed, fill = share_exposed)) +
    geom_bar(stat = 'identity') +
    labs(title = hazard_name, y = "", x = "Share of Total Population, Exposed") +
    theme_minimal() +
    theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 12),
          legend.position = "none", axis.text.x = element_text(margin = margin(t = 0.1)),
          panel.grid.major = element_blank(),  # Remove major grid lines
          panel.grid.minor = element_blank()   # Remove minor grid lines
    ) +
    scale_fill_gradientn(colors = colors) +
    scale_x_continuous(expand = c(0,0,0.2, 0) ,limits = c(0, 1)) +
    geom_text(aes(label = round(share_exposed, 3), hjust = -0.3))
}



# Generate individual plots using the function
heat_plot <- create_hazard_plot(data_long, "Heat Exposure[Extreme Heat Stress >32°C]", "bisque", "darkred")
heat_plot <- heat_plot + labs(x = "")
drought_plot <- create_hazard_plot(data_long, "Drought Exposure[Severe Drought Occurence >20%]","bisque",  "orangered4")
drought_plot <- drought_plot + labs(x = "")
airpol_plot <- create_hazard_plot(data_long, "Air Pollution Exposure[PM2.5 >15µg/m³]","bisque", "Purple4")
airpol_plot
flood_plot <- create_hazard_plot(data_long, "Flood Risk[Water Depth >0.5m]","bisque", "Midnight Blue")

combined_plot <- plot_grid(heat_plot, drought_plot, airpol_plot, flood_plot, ncol = 2,labels = c("A", "B", "C", "D"))



combined_plot


graphs


# Export ------------------------------------------------------------------

ggsave(combined_plot,filename = file.path(graphs, "share_of_exposed_byhazard.png"), height = 8, width = 10, units = c("in"))













